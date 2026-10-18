#' Case store, standardisation, scoring and the generic training loop (adapter-driven).

#' All candidate features in ONE numeric matrix, built once per process. Models select cases by row and features by name.
#' @param tab final table for modeling
#' @param idx case index (buildCaseIndex)
#' @param response column name of the response
buildCaseStore <- function(tab, idx, featureNames, response) {
  miss <- setdiff(c(featureNames, response), names(tab))
  if (length(miss)) stop("buildCaseStore: columns missing from the table: ", paste(miss, collapse = ", "))
  X <- as.matrix(tab[, featureNames, with = FALSE]); storage.mode(X) <- "double"
  if (anyNA(X)) stop("buildCaseStore: NA in feature(s) ", paste(featureNames[colSums(is.na(X)) > 0], collapse = ", "),
                     " (handle missing values explicitly in the prep module, never silently here)")
  y <- as.numeric(tab[[response]])
  if (anyNA(y)) stop("buildCaseStore: NA in the response")
  units <- sort(unique(idx$unit))
  list(index = idx, X = X, y = y, unitIndex = match(idx$unit, units), nUnits = length(units), featureNames = featureNames)
}

storeSlice <- function(store, rows, features, device = "cpu") {
  fi <- match(features, store$featureNames)
  if (anyNA(fi)) stop("storeSlice: unknown features: ", paste(features[is.na(fi)], collapse = ", "))
  list(x = torch::torch_tensor(store$X[rows, fi, drop = FALSE], dtype = torch::torch_float(), device = device),
       y = torch::torch_tensor(store$y[rows], dtype = torch::torch_float(), device = device),
       id = torch::torch_tensor(as.integer(store$unitIndex[rows]), dtype = torch::torch_long(), device = device))
}

#' Standardisation statistics from the TRAINING set only (sd 0 or NA -> 1).
scaleStats <- function(xTrain, featureNames) {
  m <- as.matrix(xTrain$cpu()); mu <- colMeans(m); sg <- apply(m, 2, stats::sd)
  sg[is.na(sg) | sg == 0] <- 1; mu[is.na(mu)] <- 0
  stats::setNames(Map(function(a, b) list(mean = a, sd = b), mu, sg), featureNames)
}
applyScale <- function(x, stats, zClip = 10) {
  mu <- torch::torch_tensor(vapply(stats, `[[`, numeric(1), "mean"), dtype = torch::torch_float(), device = x$device)$view(c(1, -1))
  sg <- torch::torch_tensor(vapply(stats, `[[`, numeric(1), "sd"), dtype = torch::torch_float(), device = x$device)$view(c(1, -1))
  xs <- (x - mu) / sg
  if (is.finite(zClip)) xs <- torch::torch_clamp(xs, -zClip, zClip)
  xs
}

#' Score cases. Units absent from training use the reserved embedding row nUnits + 1 set to the MEAN of the trained units' rows
#' (plain-R computation; trained rows are never modified). Without an embedding the id is ignored.
scoreCases <- function(net, adapter, x, y, id, trainUnitsSeen, nUnits, chunk = 4096L) {
  for (nm in c("idEmb", "idInt")) if (!is.null(net[[nm]])) {      # unseen units: the mean of the trained rows
    W <- net[[nm]]$weight
    avg <- colMeans(as.matrix(W$detach()$cpu())[trainUnitsSeen, , drop = FALSE])
    torch::with_no_grad(W[nUnits + 1L, ]$copy_(torch::torch_tensor(avg, dtype = W$dtype, device = W$device)))
  }
  idR <- as.integer(as.array(id$cpu())); seen <- idR %in% trainUnitsSeen
  idEff <- torch::torch_tensor(ifelse(seen, idR, nUnits + 1L), dtype = torch::torch_long(), device = x$device)
  wasTraining <- net$training; net$eval()
  n <- x$size(1); loss <- numeric(n); parts <- vector("list", ceiling(n / chunk)); k <- 0L
  torch::with_no_grad({
    for (a in seq(1L, n, by = chunk)) {
      z <- min(a + chunk - 1L, n); k <- k + 1L
      o <- net(x[a:z, ], idEff[a:z]); yy <- y[a:z]
      loss[a:z] <- as.numeric(adapter$caseLoss(o, yy, net)$cpu())
      parts[[k]] <- adapter$summarise(o, yy, net)
    }
  })
  if (wasTraining) net$train()
  d <- data.table::rbindlist(parts); d[, `:=`(loss = loss, seenUnit = seen)]
  d[]
}

#' Fit with Adam; ReduceLROnPlateau on the validation loss; early stopping; best-epoch weights are kept and reloaded.
#' `diag` (list(x, y, id)) is scored after every epoch and logged only (never used for stopping or selection).
#' @return list(net, bestState, history, bestEpoch, bestValLoss, trainUnitsSeen)
fitNet <- function(adapter, hyper, tr, va, nUnits, seed, device = "cpu", patience = 2L, factor = 0.5, minLr = 1e-6,
                   threshold = 1e-4, earlyStopPatience = 15L, verbose = FALSE, diag = NULL) {
  torch::torch_manual_seed(seed)
  net <- adapter$makeNet(tr$x$size(2), nUnits, hyper)$to(device = device)
  lr <- hyper$lr %||% 1e-3; batchSize <- hyper$batchSize %||% 128L; epochs <- hyper$epochs %||% 100L
  pn <- names(net$parameters); isId <- grepl("^idInt", pn)
  groups <- list(list(params = unname(net$parameters[!isId]), weight_decay = hyper$weightDecay %||% 0))
  if (any(isId)) groups[[2]] <- list(params = unname(net$parameters[isId]), weight_decay = hyper$idDecay %||% 1e-3)   # shrinkage of the route intercepts
  opt <- torch::optim_adam(groups, lr = lr)
  seen <- sort(unique(as.integer(as.array(tr$id$cpu()))))
  n <- tr$x$size(1); history <- vector("list", epochs)
  bestVal <- Inf; bestEpoch <- 0L; bestState <- NULL; bad <- 0L; curLr <- lr; sinceBest <- 0L
  for (ep in seq_len(epochs)) {
    t0 <- Sys.time(); net$train()
    perm <- torch::torch_randperm(n, device = device) + 1L; runLoss <- 0
    for (a in seq(1L, n, by = batchSize)) {
      z <- min(a + batchSize - 1L, n); ii <- perm[a:z]
      opt$zero_grad()
      loss <- adapter$caseLoss(net(tr$x[ii, ], tr$id[ii]), tr$y[ii], net)$mean()
      loss$backward(); opt$step()
      runLoss <- runLoss + loss$item() * (z - a + 1L)
    }
    valLoss <- mean(scoreCases(net, adapter, va$x, va$y, va$id, seen, nUnits)$loss)
    diagLoss <- if (is.null(diag)) NA_real_ else mean(scoreCases(net, adapter, diag$x, diag$y, diag$id, seen, nUnits)$loss)
    improved <- is.finite(valLoss) && (!is.finite(bestVal) || valLoss < bestVal - abs(bestVal) * threshold)
    if (improved) {
      bestVal <- valLoss; bestEpoch <- ep; bad <- 0L; sinceBest <- 0L
      bestState <- lapply(net$state_dict(), function(t) t$detach()$clone())
    } else { bad <- bad + 1L; sinceBest <- sinceBest + 1L }
    if (bad > patience) {
      curLr <- max(curLr * factor, minLr)
      for (g in seq_along(opt$param_groups)) opt$param_groups[[g]]$lr <- curLr
      bad <- 0L
    }
    history[[ep]] <- data.frame(epoch = ep, trainLoss = runLoss / n, valLoss = valLoss, diagLoss = diagLoss, lr = curLr, isBest = improved,
                                seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
    if (verbose) message(sprintf("epoch %d/%d train %.4f val %.4f lr %.2e%s", ep, epochs, runLoss / n, valLoss, curLr, if (improved) " *" else ""))
    if (sinceBest >= earlyStopPatience) break
  }
  history <- do.call(rbind, history[!vapply(history, is.null, logical(1))])
  if (is.null(bestState)) stop("fitNet: validation loss never became finite; no best epoch.")
  net$load_state_dict(bestState)
  list(net = net, bestState = bestState, history = history, bestEpoch = bestEpoch, bestValLoss = bestVal, trainUnitsSeen = seen)
}

#' Save best weights, reload into a FRESH network and verify that it reproduces the logged best validation loss.
checkWeightsRoundTrip <- function(adapter, hyper, bestState, weightsPath, nIn, nUnits, va, trainUnitsSeen, expectedValLoss, device = "cpu", tol = 1e-4) {
  torch::torch_save(bestState, weightsPath)
  fresh <- adapter$makeNet(nIn, nUnits, hyper)$to(device = device)
  fresh$load_state_dict(torch::torch_load(weightsPath))
  got <- mean(scoreCases(fresh, adapter, va$x, va$y, va$id, trainUnitsSeen, nUnits)$loss)
  if (!is.finite(got) || abs(got - expectedValLoss) > tol)
    stop(sprintf("Weights round-trip failed: reloaded val loss %.8f vs logged best %.8f", got, expectedValLoss))
  invisible(got)
}

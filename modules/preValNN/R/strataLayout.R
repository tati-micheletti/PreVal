#' STRATUM LAYOUT (added 2026-10-30): the engine for data whose case is a STRATUM = one observed step plus its 10 available steps (caribou, conditional likelihood).
#'
#' The engine was written for one row per case. A stratum case has a covariate TENSOR [cases, steps (11), features] and no scalar response; the adapter
#' (adapterStratum, below) scores the 11 steps and the loss of a case is the cross-entropy of the observed step (always step 1). Everything else (splits, gates, plan,
#' ranking, runner, analysis) is unchanged: it only reads the case index (one row per STRATUM: unit = animal, year, x/y = start of the observed step).
#' The hooks below are selected by the number of tensor dimensions (3 = strata) or by store$layout, so the birds path is byte-identical in behaviour.
#' The network, the loss and the standardisation reuse the old caribou code (modules/caribouNN/R/stratumNet.R, a test checks that the numbers agree).

#' Permutation of covariates for the importance in the stratum layout: the covariate values of the 11 steps are SHUFFLED WITHIN each stratum (the same shuffle for all columns of a
#' correlated group). This breaks the link between the covariate and the choice, which is what the importance measures; permuting whole strata between rows would keep the observed
#' step in position 1 with its covariate vector and destroy nothing.
.shuffleWithinStrata <- function(xg) {
  n <- xg$size(1); s <- xg$size(2); k <- xg$size(3)
  o <- torch::torch_argsort(torch::torch_rand(n, s), dim = 2)               # random order of the steps inside each stratum (1-based in R torch)
  xg$gather(2, o$unsqueeze(3)$expand(c(n, s, k)))
}

# (.nFeat and .rowsOf live in fitNet.R, which every user of the engine sources)

#' Case index of a long stratum table (11 rows per stratum, observed step first): ONE row per stratum.
#' @param tab long table with indiv_step_id, id, year, x1_, y1_, case_ (and optionally burst_)
buildStrataCaseIndex <- function(tab, steps = 11L, unitCol = "id", yearCol = "year", xCol = "x1_", yCol = "y1_") {
  miss <- setdiff(c("indiv_step_id", unitCol, yearCol, xCol, yCol, "case_"), names(tab))
  if (length(miss)) stop("buildStrataCaseIndex: missing columns: ", paste(miss, collapse = ", "))
  nR <- nrow(tab); if (nR %% steps != 0) stop("buildStrataCaseIndex: number of rows is not a multiple of ", steps)
  first <- seq.int(1L, nR, by = steps); last <- first + steps - 1L; sid <- tab$indiv_step_id
  if (!all(sid[first] == sid[last])) stop("buildStrataCaseIndex: strata are not stored in contiguous blocks of ", steps)
  if (!all(tab$case_[first] == TRUE) || sum(tab$case_) != length(first)) stop("buildStrataCaseIndex: the observed step must be first and unique in every stratum")
  idx <- data.table::data.table(row = seq_along(first), unit = as.character(tab[[unitCol]][first]), year = as.integer(tab[[yearCol]][first]),
                                x = as.numeric(tab[[xCol]][first]), y = as.numeric(tab[[yCol]][first]), indiv_step_id = as.character(sid[first]))
  idx[, burstKey := if ("burst_" %in% names(tab)) paste(unit, tab$burst_[first], sep = "_") else indiv_step_id]
  if (anyDuplicated(idx$indiv_step_id)) stop("buildStrataCaseIndex: duplicated stratum ids")
  if (anyNA(idx[, .(unit, year, x, y)])) stop("buildStrataCaseIndex: NA in unit, year or coordinates")
  idx[]
}

#' Stratum store: ONE float tensor [strata, steps, all candidate features]; models select strata and features by name (as in the old caribou store).
#' `unitIndex` = 1-based animal index. It follows the OLD numbering (as.numeric(as.factor(id)) over the animals of the prepared table) so that a network built with the
#' same seed has the same embedding rows as in the old module (the acceptance test relies on it).
buildStrataStoreEngine <- function(tab, idx, featureNames, steps = 11L) {
  miss <- setdiff(featureNames, names(tab)); if (length(miss)) stop("buildStrataStoreEngine: columns missing from the table: ", paste(miss, collapse = ", "))
  n <- nrow(idx); X <- torch::torch_empty(n, steps, length(featureNames), dtype = torch::torch_float())
  for (k in seq_along(featureNames)) {
    v <- as.numeric(tab[[featureNames[k]]])
    if (anyNA(v)) stop("buildStrataStoreEngine: NA in feature ", featureNames[k], " (impute explicitly in the prep step, never silently here)")
    X[, , k] <- torch::torch_tensor(t(matrix(v, nrow = steps)), dtype = torch::torch_float())
  }
  idxFirst <- seq.int(1L, nrow(tab), by = steps)
  unitIndex <- as.integer(as.numeric(as.factor(tab[["id"]][idxFirst])))
  list(index = idx, X = X, y = NULL, unitIndex = unitIndex, nUnits = max(unitIndex), featureNames = featureNames, layout = "strata", steps = steps)
}

.strataSlice <- function(store, rows, features, device = "cpu") {
  fi <- match(features, store$featureNames)
  if (anyNA(fi)) stop("storeSlice: unknown features: ", paste(features[is.na(fi)], collapse = ", "))
  r <- torch::torch_tensor(as.integer(rows), dtype = torch::torch_long())
  x <- store$X$index_select(1, r)$index_select(3, torch::torch_tensor(fi, dtype = torch::torch_long()))
  list(x = x$to(device = device), y = torch::torch_ones(length(rows), dtype = torch::torch_float(), device = device),     # y is a placeholder: the observed step is class 1
       id = torch::torch_tensor(as.integer(store$unitIndex[rows]), dtype = torch::torch_long(), device = device))
}

# per-feature mean and sd over all strata x steps of the TRAINING set (float64, as in the old caribou code); sd 0 or NA -> 1, mean NA -> 0
.scaleStats3 <- function(xTrain, featureNames) {
  stats <- lapply(seq_along(featureNames), function(i) {
    v <- xTrain[, , i]$to(dtype = torch::torch_float64()); mu <- as.numeric(v$mean()$item()); sg <- as.numeric(v$std()$item())
    if (is.na(mu)) mu <- 0; if (is.na(sg) || sg == 0) sg <- 1
    list(mean = mu, sd = sg) })
  names(stats) <- featureNames; stats
}
.applyScale3 <- function(x, stats, zClip = 10) {
  mu <- torch::torch_tensor(vapply(stats, `[[`, numeric(1), "mean"), dtype = torch::torch_float())$view(c(1, 1, -1))
  sg <- torch::torch_tensor(vapply(stats, `[[`, numeric(1), "sd"), dtype = torch::torch_float())$view(c(1, 1, -1))
  xs <- (x - mu$to(device = x$device)) / sg$to(device = x$device)
  if (is.finite(zClip)) xs <- torch::torch_clamp(xs, -zClip, zClip)
  xs
}

#' The network of the old caribou module (modules/caribouNN/R/stratumNet.R, makeStratumNet): per-animal embedding (8-d) concatenated to the step covariates -> 128 -> 64 -> 1 score per
#' candidate step (SELU); embedding row nAnimals + 1 is reserved for animals not seen in training. An IDENTICAL copy (the old file cannot be sourced next to the engine: it defines
#' scaleStats, applyScale and checkWeightsRoundTrip with different bodies); test_strata.R checks that both give the same scores for the same seed.
makeStratumNetEngine <- function() {
  torch::nn_module(
    "StratumNet",
    initialize = function(nIn, nAnimals, embDim = 8L) {
      self$idEmb <- torch::nn_embedding(nAnimals + 1L, embDim)
      self$fc1 <- torch::nn_linear(nIn + embDim, 128)
      self$fc2 <- torch::nn_linear(128, 64)
      self$out <- torch::nn_linear(64, 1)
      self$act <- torch::nn_selu()
    },
    forward = function(x, id) {
      steps <- x$size(2)
      emb <- self$idEmb(id)$unsqueeze(2)$expand(c(-1, steps, -1))
      h <- self$act(self$fc1(torch::torch_cat(list(x, emb), 3)))
      h <- self$act(self$fc2(h))
      torch::torch_squeeze(self$out(h), 3)
    }
  )
}

#' Stratum adapter: the network and the shape of the old caribou module (animal embedding concatenated to the step covariates, 128 -> 64 -> 1 score per step, SELU),
#' loss = cross-entropy of the observed step (chance = log 11 = 2.398). PRIMARY loss = the raw cross-entropy; CALIBRATED loss = the same scores divided by a temperature
#' re-estimated on the regime's OWN validation strata (nothing from the test set), the analogue of the dispersion calibration of the count adapter.
adapterStratum <- function(netFactory = NULL) {
  Net <- if (is.null(netFactory)) makeStratumNetEngine() else netFactory()
  nllT <- function(S, T) -as.numeric(torch::nnf_log_softmax(torch::torch_tensor(S / T, dtype = torch::torch_float64()), dim = 2)[, 1])
  scoreMat <- function(d) as.matrix(d[, paste0("s", 1:11), with = FALSE])
  list(name = "stratum_conditional_logit", responseType = "stratum", layout = "strata", primary = "cross-entropy of the observed step per stratum (chance = log 11)",
       makeNet = function(nIn, nUnits, hyper) Net(nIn = nIn, nAnimals = nUnits, embDim = hyper$embDim %||% 8L),
       caseLoss = function(out, y, net) -torch::nnf_log_softmax(out, dim = 2)[, 1],
       summarise = function(out, y, net) {
         lp <- torch::nnf_log_softmax(out, dim = 2); pT <- as.numeric(torch::torch_exp(lp[, 1])$cpu())
         rk <- as.integer(as.array((out > out[, 1]$unsqueeze(2))$sum(dim = 2)$cpu())) + 1L
         s <- as.matrix(as.array(out$cpu())); colnames(s) <- paste0("s", seq_len(ncol(s)))
         data.table::data.table(pred = pT, obs = 1, absErr = 1 - pT, sqErr = (1 - pT)^2, rankTrue = rk, correct = rk == 1L, data.table::as.data.table(s))
       },
       # TEMPERATURE CALIBRATION on the regime's own validation strata (scores held fixed); the saved per-case files drop the 11 score columns afterwards
       calibrateTable = function(vaT) exp(stats::optimize(function(lt) mean(nllT(scoreMat(vaT), exp(lt))), c(-3, 3))$minimum),
       lossCalTable = function(d, disp) nllT(scoreMat(d), disp),
       dropColumns = paste0("s", 1:11))
}

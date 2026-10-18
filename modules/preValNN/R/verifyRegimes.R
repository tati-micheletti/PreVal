#' Hard gate against leakage and sampling bias for the five regimes of one split. Every check STOPS the run on failure.
#'   L1  no case appears in two sets within a regime
#'   L2  the test set is identical in all regimes and lies entirely in year T
#'   L3  train/val/test sizes are equal across ALL regimes (equal training size included)
#'   L4  FutureUnseen: train years in [s, e-1], validation = year e only
#'   L5  FutureTainted / Grouped / Spatial: train+val years in [s, e] and inside the PreVal pool U
#'   L6  Internal: years in [s, T], no test case
#'   L7  Grouped: no unit in both train and validation
#'   L8  Spatial: no training case in a validation block or its buffer
#'   S1  random draws are uniform: year and unit composition of the status-quo training set, the Internal draw and the
#'       subsampled PreVal training set are consistent with their pools (|z| < zMax)
verifyRegimeSplits <- function(idx, bundle, s, e, testYear, splitId, spatial = NULL, zMax = 5, minExpected = 20) {
  stopifnot(isTRUE(bundle$ok))
  sp <- bundle$splits; yr <- idx$year; res <- list()
  add <- function(check, ok, detail = "") res[[length(res) + 1L]] <<- data.table::data.table(
    splitId = splitId, check = check, ok = isTRUE(ok), detail = as.character(detail))
  rowsOf <- function(m, st) m$row[m$set == st]
  for (rg in names(sp)) add(paste0("L1 disjoint sets [", rg, "]"), !anyDuplicated(sp[[rg]]$row))
  tests <- lapply(sp, rowsOf, st = "test")
  add("L2 test set identical across regimes", all(vapply(tests, function(t) identical(sort(t), sort(tests[[1]])), logical(1))))
  add("L2 test set lies entirely in the forecast year", all(yr[tests[[1]]] == testYear))
  sz <- vapply(sp, function(m) c(sum(m$set == "train"), sum(m$set == "val"), sum(m$set == "test")), numeric(3))
  add("L3 equal train/val/test sizes across regimes", all(apply(sz, 1, function(v) length(unique(v)) == 1)),
      paste(apply(sz, 2, paste, collapse = "/"), collapse = " | "))
  u <- sp$FutureUnseen
  add("L4 FutureUnseen train years in [s, e-1]", all(yr[rowsOf(u, "train")] >= s & yr[rowsOf(u, "train")] <= e - 1L))
  add("L4 FutureUnseen validation = year e only", all(yr[rowsOf(u, "val")] == e))
  Upool <- which(yr >= s & yr <= e)
  U <- Upool
  for (rg in intersect(c("FutureTainted", "FutureTaintedGrouped", "FutureTaintedSpatial"), names(sp))) {
    tv <- c(rowsOf(sp[[rg]], "train"), rowsOf(sp[[rg]], "val"))
    add(sprintf("L5 %s years in [s, e] and inside the pool", rg), all(yr[tv] >= s & yr[tv] <= e) && all(tv %in% Upool))
  }
  it <- NULL
  if ("Internal" %in% names(sp)) {
    it <- c(rowsOf(sp$Internal, "train"), rowsOf(sp$Internal, "val"))
    add("L6 Internal years in [s, T]", all(yr[it] >= s & yr[it] <= testYear))
    add("L6 Internal contains no test case", !any(it %in% tests[[1]]))
  }
  if ("FutureTaintedGrouped" %in% names(sp)) {
    g <- sp$FutureTaintedGrouped
    add("L7 Grouped: no unit in both train and validation",
        !length(intersect(idx$unit[rowsOf(g, "train")], idx$unit[rowsOf(g, "val")])))
  }
  if ("FutureTaintedSpatial" %in% names(sp)) {
    if (is.null(spatial)) stop("verifyRegimeSplits: spatial regime present but no spatial blocks given")
    g <- sp$FutureTaintedSpatial
    vb <- unique(spatial$key[rowsOf(g, "val")])
    zone <- blockZone(idx, vb, spatial)
    add("L8 Spatial: no training case in a validation block or its buffer", !any(zone[rowsOf(g, "train")]))
    add("L8 Spatial: validation blocks are the ones that were chosen", setequal(vb, bundle$spatialValBlocks))
  }
  zCheck <- function(drawRows, poolRows, key, label) {
    N <- length(poolRows); n <- length(drawRows)
    if (n >= N) return(add(label, TRUE, "draw equals pool"))
    if (n > N - n) { drawRows <- setdiff(poolRows, drawRows); n <- N - n }   # test the smaller side: the normal approximation fails when only a few cases were left out
    pk <- table(key[poolRows]); dk <- table(key[drawRows])[names(pk)]; dk[is.na(dk)] <- 0
    p <- as.numeric(pk) / N; expd <- n * p; sdv <- sqrt(n * p * (1 - p) * (1 - n / N))
    keep <- expd >= minExpected & sdv > 0
    z <- (as.numeric(dk) - expd)[keep] / sdv[keep]
    add(label, length(z) == 0 || max(abs(z)) < zMax, sprintf("max |z| = %.2f over %d categories", if (length(z)) max(abs(z)) else 0, length(z)))
  }
  tn <- sp$FutureTainted
  zCheck(rowsOf(tn, "train"), setdiff(U, rowsOf(tn, "val")), yr, "S1 Tainted train: year composition uniform")
  zCheck(rowsOf(tn, "train"), setdiff(U, rowsOf(tn, "val")), idx$unit, "S1 Tainted train: unit composition uniform")
  zCheck(rowsOf(tn, "val"), U, yr, "S1 Tainted validation: year composition uniform")
  zCheck(rowsOf(u, "train"), bundle$pools$FutureUnseen, yr, "S1 PreVal subsampled train: year composition uniform")
  if (!is.null(it)) {
    intPool <- setdiff(which(yr >= s & yr <= testYear), tests[[1]])
    zCheck(it, intPool, yr, "S1 Internal draw: year composition uniform")
    zCheck(it, intPool, idx$unit, "S1 Internal draw: unit composition uniform")
  }
  out <- data.table::rbindlist(res)
  if (any(!out$ok)) stop("verifyRegimeSplits FAILED for ", splitId, ":\n", paste(utils::capture.output(print(out[ok == FALSE])), collapse = "\n"))
  out
}

#' Re-check ONE model's manifest right before training (guards against stale or hand-edited manifests).
verifyRegimeManifest <- function(idx, manifest, regime, s, e, testYear, expected) {
  yr <- idx$year
  tr <- manifest$row[manifest$set == "train"]; va <- manifest$row[manifest$set == "val"]; te <- manifest$row[manifest$set == "test"]
  stopifnot(!anyDuplicated(manifest$row))
  if (length(tr) != expected[["nTrain"]] || length(va) != expected[["nVal"]] || length(te) != expected[["nTest"]])
    stop("Manifest sizes differ from the plan for ", regime)
  if (!all(yr[te] == testYear)) stop("Test cases outside the forecast year")
  tv <- c(tr, va)
  switch(regime,
    FutureUnseen = if (!(all(yr[tr] >= s & yr[tr] <= e - 1L) && all(yr[va] == e))) stop("FutureUnseen year rules violated"),
    FutureTainted = , FutureTaintedGrouped = , FutureTaintedSpatial = if (!all(yr[tv] >= s & yr[tv] <= e)) stop(regime, " year rules violated"),
    Internal = if (!all(yr[tv] >= s & yr[tv] <= testYear)) stop("Internal year rules violated"),
    stop("Unknown regime: ", regime))
  if (regime == "FutureTaintedGrouped" && length(intersect(idx$unit[tr], idx$unit[va]))) stop("Grouped regime: a unit is in train and validation")
  invisible(TRUE)
}

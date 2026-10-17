#' Case index: ONE row per case of the final table (a case = the unit that is scored, here a route in a year).
#'
#' The engine only needs: which case (row), which unit repeats across cases (`unit`, here the route), when (`year`) and where
#' (x, y in projected metres, used only to build spatial blocks). Everything else lives in the adapter and the feature table.
#' @param tab final table for modeling (data.table), one row per case
#' @param unitCol,yearCol,xCol,yCol column names
buildCaseIndex <- function(tab, unitCol = "SITE_ID", yearCol = "year", xCol = "x", yCol = "y") {
  miss <- setdiff(c(unitCol, yearCol, xCol, yCol), names(tab))
  if (length(miss)) stop("buildCaseIndex: missing columns: ", paste(miss, collapse = ", "))
  idx <- data.table::data.table(row = seq_len(nrow(tab)), unit = as.character(tab[[unitCol]]),
                                year = as.integer(tab[[yearCol]]), x = as.numeric(tab[[xCol]]), y = as.numeric(tab[[yCol]]))
  if (anyDuplicated(idx[, .(unit, year)])) stop("buildCaseIndex: a unit appears twice in the same year")
  if (anyNA(idx)) stop("buildCaseIndex: NA in unit, year or coordinates")
  idx[]
}

#' Square spatial blocks (projected metres) with a per-block random fold; the block key is "bx by".
assignBlocks <- function(idx, blockKm, bufferKm = 0, nFolds = 4L, seed) {
  B <- blockKm * 1000
  bx <- floor(idx$x / B); by <- floor(idx$y / B)
  key <- paste(bx, by)
  blocks <- sort(unique(key))
  fold <- withSeed(seed, sample(rep_len(seq_len(nFolds), length(blocks))))
  names(fold) <- blocks
  list(key = key, bx = bx, by = by, blockFold = fold, blockKm = blockKm, bufferKm = bufferKm, nFolds = nFolds, seed = seed)
}

#' Rows (of idx) inside the given blocks or within the buffer around them.
#' @return logical per row of idx
blockZone <- function(idx, blocks, spatial) {
  B <- spatial$blockKm * 1000; buf <- spatial$bufferKm * 1000
  inside <- spatial$key %in% blocks
  zone <- inside
  if (buf > 0 && length(blocks)) {
    parts <- do.call(rbind, strsplit(blocks, " ", fixed = TRUE))
    tbx <- as.numeric(parts[, 1]); tby <- as.numeric(parts[, 2])
    for (k in seq_along(tbx)) {
      x0 <- tbx[k] * B; y0 <- tby[k] * B
      cand <- which(!zone & abs(spatial$bx - tbx[k]) <= ceiling(buf / B) & abs(spatial$by - tby[k]) <= ceiling(buf / B))
      if (!length(cand)) next
      hit <- idx$x[cand] >= x0 - buf & idx$x[cand] <= x0 + B + buf & idx$y[cand] >= y0 - buf & idx$y[cand] <= y0 + B + buf
      zone[cand[hit]] <- TRUE
    }
  }
  zone
}

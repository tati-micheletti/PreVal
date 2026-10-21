#' Arm-specific changes to the fixed network settings (added 2026-10-21): "key=value;key=value", e.g. "idIntercept=FALSE" (route information off) or "idDecay=0.01"
#' (strong shrinkage of the route intercepts). Values are parsed as logical, number or text. Only keys that already exist in the settings (or are listed in `allowNew`)
#' may be changed, so a typo cannot silently do nothing.
applyHyperOverride <- function(hyper, str, allowNew = c("idIntercept", "idDecay", "embDim", "dropout", "weightDecay", "lr")) {
  if (is.null(str) || !nzchar(trimws(str))) return(hyper)
  for (kv in trimws(strsplit(str, ";", fixed = TRUE)[[1]])) {
    p <- strsplit(kv, "=", fixed = TRUE)[[1]]; if (length(p) != 2L) stop("hyperOverride: cannot read '", kv, "' (use key=value;key=value)")
    k <- trimws(p[1]); v <- trimws(p[2])
    if (!k %in% c(names(hyper), allowNew)) stop("hyperOverride: unknown setting '", k, "'")
    hyper[[k]] <- if (toupper(v) %in% c("TRUE", "FALSE")) as.logical(toupper(v)) else if (!is.na(suppressWarnings(as.numeric(v)))) as.numeric(v) else v
  }
  hyper
}

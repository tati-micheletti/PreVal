#' RNG helpers of the generic validation engine (same behaviour as the caribou modules' helpers).

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Pin R's RNG algorithms so that a seed means the same thing on every machine and worker.
pinRNG <- function() {
  suppressWarnings(RNGkind("Mersenne-Twister", "Inversion", "Rejection"))
  invisible(NULL)
}

#' Evaluate `expr` with a pinned RNG kind and `seed`, then restore the caller's RNG state.
withSeed <- function(seed, expr) {
  hadSeed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  oldSeed <- if (hadSeed) get(".Random.seed", envir = globalenv()) else NULL
  oldKind <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(oldKind[1], oldKind[2], oldKind[3]))
    if (hadSeed) assign(".Random.seed", oldSeed, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  pinRNG()
  set.seed(seed)
  force(expr)
}

#' Deterministic string -> integer seed in [1, 2147483646] (no dependency, same everywhere).
stringSeed <- function(x) {
  codes <- utf8ToInt(enc2utf8(as.character(x)))
  h <- 5381
  for (cc in codes) h <- (h * 33 + cc) %% 2147483647
  as.integer(max(1, h))
}

#' Append rows to the seed registry (one line per seed used anywhere in the pipeline).
registerSeeds <- function(registryPath, stage, name, seed, purpose) {
  if (is.null(registryPath)) return(invisible(NULL))
  rows <- data.table::data.table(stage = stage, name = name, seed = as.numeric(seed),
                                 purpose = purpose, time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  dir.create(dirname(registryPath), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(rows, registryPath, append = file.exists(registryPath))
  invisible(rows)
}

#' ADAPTERS: everything that depends on the kind of response. The engine (splits, gates, plan, runner, analysis) never looks inside.
#'
#' An adapter is a list with
#'   name           label
#'   responseType   "count" | "continuous" | ...
#'   makeNet(nIn, nUnits, hyper)   a torch module whose forward(x, id) returns a [n, k] tensor of raw outputs (k = adapter's outputs)
#'   caseLoss(out, y, net)         per-case loss tensor [n] (negative log-likelihood); the mean over cases is the training loss
#'   summarise(out, y, net)        data.table of per-case diagnostics (pred = predicted mean, plus anything useful)
#'   primary                       name of the loss reported as the headline (always the mean of caseLoss)
#' The network STRUCTURE (hidden sizes, unit embedding, dropout) is a `hyper` entry chosen per dataset and species by a pilot on the
#' pre-test history only, then FIXED across every regime and every covariate level of that species.
#'
#' Unit (route) information, three modes chosen per dataset and fixed for a run:
#'   none       (default of the tests) the network sees only the covariates;
#'   intercept  hyper$idIntercept = TRUE: one learned number per unit ADDED to the predicted log-mean, shrunk by an L2 penalty
#'              (hyper$idDecay). This is the network analogue of a random intercept of a mixed model (Bolker et al. 2009) and of the
#'              route effects of the BBS hierarchical models (Sauer and Link 2011);
#'   concat     hyper$embDim > 0: a learned vector per unit is fed to the network together with the covariates (entity embedding,
#'              Guo and Berkhahn 2016); more flexible, and it can interact with the covariates.
#' Units unseen in training use the mean of the trained rows in every mode (see scoreCases).

.mlpTrunk <- function(nIn, embDim, nUnits, hidden, dropout) {
  list(emb = if (embDim > 0) torch::nn_embedding(nUnits + 1L, embDim) else NULL,
       layers = {
         dims <- c(nIn + embDim, hidden); ls <- list()
         for (i in seq_len(length(dims) - 1L)) { ls[[length(ls) + 1L]] <- torch::nn_linear(dims[i], dims[i + 1L]); ls[[length(ls) + 1L]] <- torch::nn_selu() }
         if (dropout > 0) ls[[length(ls) + 1L]] <- torch::nn_alpha_dropout(dropout)
         do.call(torch::nn_sequential, ls) })
}

#' Count adapter: negative-binomial head (mean mu = exp(out), one global dispersion theta = exp(logTheta)).
adapterCount <- function() {
  Net <- torch::nn_module(
    "NBNet",
    initialize = function(nIn, nUnits, hidden = c(128, 64), embDim = 0L, dropout = 0, idIntercept = FALSE) {
      trunk <- .mlpTrunk(nIn, embDim, nUnits, hidden, dropout)
      self$embDim <- embDim
      if (embDim > 0) self$idEmb <- trunk$emb
      self$useInt <- idIntercept
      if (idIntercept) { self$idInt <- torch::nn_embedding(nUnits + 1L, 1L); torch::with_no_grad(self$idInt$weight$zero_()) }
      self$body <- trunk$layers
      self$out <- torch::nn_linear(tail(hidden, 1), 1)
      self$logTheta <- torch::nn_parameter(torch::torch_tensor(0))
    },
    forward = function(x, id) {
      h <- if (self$embDim > 0) torch::torch_cat(list(x, self$idEmb(id)), 2) else x
      o <- self$out(self$body(h))$squeeze(2)
      if (self$useInt) o <- o + self$idInt(id)$squeeze(2)
      torch::torch_clamp(o, -8, 8)          # log of the mean
    })
  nll <- function(logMu, y, logTheta) {
    theta <- torch::torch_exp(logTheta); mu <- torch::torch_exp(logMu)
    -(torch::torch_lgamma(y + theta) - torch::torch_lgamma(theta) - torch::torch_lgamma(y + 1) +
        theta * (logTheta - torch::torch_log(theta + mu)) + y * (logMu - torch::torch_log(theta + mu)))
  }
  list(name = "count_negbin", responseType = "count", primary = "negative-binomial NLL per case",
       makeNet = function(nIn, nUnits, hyper) Net(nIn = nIn, nUnits = nUnits, hidden = hyper$hidden %||% c(128, 64),
                                                  embDim = hyper$embDim %||% 0L, dropout = hyper$dropout %||% 0,
                                                  idIntercept = isTRUE(hyper$idIntercept)),
       caseLoss = function(out, y, net) nll(out, y, net$logTheta),
       summarise = function(out, y, net) {
         mu <- as.numeric(torch::torch_exp(out)$cpu()); yy <- as.numeric(y$cpu())
         data.table::data.table(pred = mu, obs = yy, absErr = abs(mu - yy), sqErr = (mu - yy)^2, theta = as.numeric(torch::torch_exp(net$logTheta)$item()))
       })
}

#' Continuous adapter (Gaussian head with a learned global scale). Shows that the engine is response-agnostic; used in the tests.
adapterGaussian <- function() {
  Net <- torch::nn_module(
    "GaussNet",
    initialize = function(nIn, nUnits, hidden = c(32, 16), embDim = 0L, dropout = 0, idIntercept = FALSE) {
      trunk <- .mlpTrunk(nIn, embDim, nUnits, hidden, dropout)
      self$embDim <- embDim
      if (embDim > 0) self$idEmb <- trunk$emb
      self$useInt <- idIntercept
      if (idIntercept) { self$idInt <- torch::nn_embedding(nUnits + 1L, 1L); torch::with_no_grad(self$idInt$weight$zero_()) }
      self$body <- trunk$layers
      self$out <- torch::nn_linear(tail(hidden, 1), 1)
      self$logSigma <- torch::nn_parameter(torch::torch_tensor(0))
    },
    forward = function(x, id) {
      h <- if (self$embDim > 0) torch::torch_cat(list(x, self$idEmb(id)), 2) else x
      o <- self$out(self$body(h))$squeeze(2)
      if (self$useInt) o <- o + self$idInt(id)$squeeze(2)
      o
    })
  list(name = "continuous_gaussian", responseType = "continuous", primary = "Gaussian NLL per case",
       makeNet = function(nIn, nUnits, hyper) Net(nIn = nIn, nUnits = nUnits, hidden = hyper$hidden %||% c(32, 16),
                                                  embDim = hyper$embDim %||% 0L, dropout = hyper$dropout %||% 0,
                                                  idIntercept = isTRUE(hyper$idIntercept)),
       caseLoss = function(out, y, net) 0.5 * (log(2 * pi) + 2 * net$logSigma + (y - out)^2 / torch::torch_exp(2 * net$logSigma)),
       summarise = function(out, y, net) {
         mu <- as.numeric(out$cpu()); yy <- as.numeric(y$cpu())
         data.table::data.table(pred = mu, obs = yy, absErr = abs(mu - yy), sqErr = (mu - yy)^2)
       })
}

getAdapter <- function(responseType) switch(responseType, count = adapterCount(), continuous = adapterGaussian(),
                                            stop("No adapter for responseType '", responseType, "' yet (available: count, continuous)."))

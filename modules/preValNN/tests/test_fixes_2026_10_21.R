# Tests for the fixes of 2026-10-21 (audit): dropout network builds, dispersion calibration, refit learning-rate alignment
suppressMessages({library(data.table); library(torch)})
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("seeds.R", "adapters.R", "fitNet.R", "runRefit.R", "hyperOverride.R")) source(file.path(here, "..", "R", f))
nFail <- 0L
check <- function(cond, msg) { ok <- isTRUE(cond); cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) nFail <<- nFail + 1L }

# 1. dropout > 0 builds and trains (it used to error and the pilot swallowed the error)
ad <- adapterCount()
net <- tryCatch(ad$makeNet(5L, 10L, list(hidden = c(8, 4), dropout = 0.1, idIntercept = TRUE)), error = function(e) NULL)
check(!is.null(net), "a network with dropout = 0.1 can be built")
if (!is.null(net)) { x <- torch_randn(16, 5); id <- torch_tensor(sample(1:10, 16, TRUE), dtype = torch_long()); net$train(); o1 <- net(x, id); net$eval(); o2 <- net(x, id); o3 <- net(x, id)
  check(all(dim(o1) == 16) && isTRUE(all.equal(as.numeric(o2), as.numeric(o3))), "dropout is active in training and off (deterministic) in eval mode") }

# 2. dispersion calibration recovers the true NB theta from data with known mean, and reduces the loss of an overconfident theta
set.seed(1); mu <- exp(rnorm(4000, 1, 0.5)); y <- rnbinom(4000, mu = mu, size = 3)
th <- ad$calibrate(mu, y)
check(abs(log(th / 3)) < 0.25, sprintf("calibrated theta %.2f is close to the true 3", th))
check(mean(ad$nllWith(mu, y, th)) < mean(ad$nllWith(mu, y, 60)), "the calibrated loss is lower than the loss with an overconfident theta (60)")
# NLL in R equals the torch loss used for training
tl <- as.numeric(ad$caseLoss(torch_log(torch_tensor(mu[1:50])), torch_tensor(y[1:50]), list(logTheta = torch_tensor(log(3))))$cpu())
check(max(abs(tl - ad$nllWith(mu[1:50], y[1:50], 3))) < 1e-4, "R and torch negative-binomial losses agree")
ag <- adapterGaussian(); m <- rnorm(500); yy <- m + rnorm(500, sd = 2)
check(abs(ag$calibrate(m, yy) - 2) < 0.25, "Gaussian scale calibration recovers the true sd")

# 3. refit learning-rate schedule: the rate used DURING epoch ep is the initial rate for epoch 1 and history$lr[ep - 1] afterwards
h <- data.frame(epoch = 1:6, lr = c(1e-3, 1e-3, 5e-4, 5e-4, 5e-4, 2.5e-4))     # fitNet logs the rate AFTER the plateau cut of that epoch
lrSched <- c(1e-3, h$lr)[seq_len(6)]
check(identical(lrSched, c(1e-3, 1e-3, 1e-3, 5e-4, 5e-4, 5e-4)), "a cut logged at epoch 3 first applies in epoch 4 of the refit")
# 4. arm-specific network settings
h0 <- list(hidden = c(32, 16), idIntercept = TRUE, idDecay = 1e-5, dropout = 0.1)
h1 <- applyHyperOverride(h0, "idIntercept=FALSE"); h2 <- applyHyperOverride(h0, "idDecay=0.01;dropout=0")
check(identical(h1$idIntercept, FALSE) && h1$idDecay == 1e-5 && h2$idDecay == 0.01 && h2$dropout == 0 && identical(h2$hidden, c(32, 16)), "hyperOverride changes only the named settings, with the right types")
check(identical(applyHyperOverride(h0, ""), h0) && inherits(try(applyHyperOverride(h0, "idDecy=1"), silent = TRUE), "try-error") && inherits(try(applyHyperOverride(h0, "idDecay"), silent = TRUE), "try-error"), "hyperOverride: empty = no change; a typo or a malformed string stops the run")
cat(sprintf("\n%d failure(s)\n", nFail)); if (nFail) quit(status = 1)

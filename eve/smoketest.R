# Compute-node smoke test: packages load, torch trains a tiny network, the repository code sources, paths resolve.
libs <- Sys.glob(file.path(path.expand("~"), ".local", "share", "R", "PreVal", "packages", "*", "*"))
if (length(libs)) .libPaths(c(libs, .libPaths()))
message("libPaths: ", paste(.libPaths(), collapse = " | "))
ok <- TRUE
chk <- function(cond, msg) { cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", msg)); if (!isTRUE(cond)) ok <<- FALSE }
for (p in c("data.table", "torch", "SpaDES.core")) chk(requireNamespace(p, quietly = TRUE), paste("package", p))
suppressMessages({ library(data.table); library(torch) })
root <- Sys.getenv("PREVAL_ROOT"); work <- Sys.getenv("PREVAL_WORK")
chk(file.exists(file.path(root, "runMe.R")), paste("repository found:", root))
chk(file.exists(Sys.getenv("PREVAL_TABLE")), paste("data table found:", Sys.getenv("PREVAL_TABLE")))
chk(dir.exists(work) && file.access(work, 2) == 0, paste("work folder writable:", work))
chk(file.access(Sys.getenv("TMPDIR"), 2) == 0, paste("TMPDIR writable:", Sys.getenv("TMPDIR")))
source(file.path(root, "modules", "caribouNN_Global", "R", "stratumNet.R"))
set.seed(1); n <- 3000; x <- array(rnorm(n * 11 * 4), c(n, 11, 4)); x[, 1, 1] <- x[, 1, 1] + 1
t0 <- Sys.time()
fit <- fitStratumNet(torch_tensor(x[1:2400, , ], dtype = torch_float()), torch_tensor(sample(1:5, 2400, TRUE), dtype = torch_long()),
                     torch_tensor(x[2401:n, , ], dtype = torch_float()), torch_tensor(sample(1:5, 600, TRUE), dtype = torch_long()),
                     nAnimals = 5, lr = 0.01, epochs = 5, seed = 1L, verbose = FALSE)
chk(fit$bestValLoss < log(11), sprintf("tiny network beats chance (%.3f < %.3f) in %.1f s", fit$bestValLoss, log(11), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
cat(sprintf("node: %s | R %s | torch %s | threads %d\n", Sys.info()[["nodename"]], getRversion(), as.character(packageVersion("torch")), torch_get_num_threads()))
cat(if (ok) "PREVAL SMOKETEST: ALL OK\n" else "PREVAL SMOKETEST: FAILED\n")
if (!ok) quit(status = 1)

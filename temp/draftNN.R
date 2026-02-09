# DRAFT MODULE -- NN
defineModule(sim, list(
  name = "caribouNN",
  description = "Neural Network iSSA using Torch/Luz",
  keywords = c("Neural Network", "iSSA", "Torch"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(caribouNN = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md"),
  reqdPkgs = list("data.table", "torch", "luz", "future", "future.apply"),
  
  parameters = bindrows(
    # --- Experiment Parameters (Driven by your CSV) ---
    defineParameter("numberOfCovariates", "numeric", 20, 2, 100, "Number of features to use"),
    defineParameter("trainYears", "numeric", c(2007, 2022), NA, NA, "Start/End year for training"),
    defineParameter("testYear", "numeric", NA, NA, NA, "Specific year for testing (Predictive Val)"),
    
    # --- NN Hyperparameters ---
    defineParameter("batchSize", "numeric", 512, 32, 4096, "Batch size"),
    defineParameter("learningRate", "numeric", 0.0025, 0.0001, 0.1, "Learning Rate"),
    defineParameter("device", "character", "cpu", NA, NA, "Force CPU for parallelization"),
    defineParameter("epochs", "numeric", 50, 1, 1000, "Max epochs")
  ),
  
  inputObjects = bindrows(
    expectsInput("extractedVariables", "data.table", "The big feature table"),
    expectsInput("featurePriority", "character", "Ordered list of variable names to select from")
  ),
  
  outputObjects = bindrows(
    createsOutput("modelMetrics", "data.table", "Loss and Accuracy metrics")
  )
))

## -----------------------------------------------------------------------------
## EVENT FUNCTIONS
## -----------------------------------------------------------------------------
doEvent.caribouNN = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- scheduleEvent(sim, time(sim), "caribouNN", "trainInfo")
    },
    
    trainInfo = {
      # 1. Prepare Data specific to this simulation parameters
      # We do the filtering inside the module so each worker does its own prep
      sim$dataTensors <- .prepData(
        dt = sim$extractedVariables, # The big table
        priority = sim$featurePriority,
        nCovs = P(sim)$numberOfCovariates,
        trainY = P(sim)$trainYears,
        testY  = P(sim)$testYear
      )
      
      # 2. Run Training
      sim$modelMetrics <- .trainLuz(
        tensors = sim$dataTensors,
        params = parameters(sim)
      )
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

## -----------------------------------------------------------------------------
## HELPER FUNCTIONS
## -----------------------------------------------------------------------------

.prepData <- function(dt, priority, nCovs, trainY, testY) {
  # A. Feature Selection
  # Always keep the Core 3, then add top N from priority
  core <- c("logSl", "cosTa", "sinTa")
  available <- setdiff(priority, core) # Remove core if present to avoid dupes
  # Select Top N (minus 3 for core)
  topN <- available[1:min(length(available), (nCovs - 3))]
  selectedFeatures <- c(core, topN)
  
  # Ensure columns exist
  selectedFeatures <- intersect(selectedFeatures, names(dt))
  
  # B. Filter Rows (Time)
  # Identify Train Rows
  trainIdx <- dt$year >= trainY[1] & dt$year <= trainY[2]
  
  # Identify Test Rows
  # If testYear is NA, we do a random holdout from the training years
  if (is.na(testY)) {
    # Internal Validation: Split Train into Train (80) / Test (20)
    # We use burst IDs to split
    allBursts <- unique(dt$indiv_step_id[trainIdx])
    testBursts <- sample(allBursts, size = floor(length(allBursts) * 0.2))
    
    dtTrain <- dt[indiv_step_id %in% setdiff(allBursts, testBursts)]
    dtTest  <- dt[indiv_step_id %in% testBursts]
    
  } else {
    # Predictive Validation: Train on TrainYears, Test on TestYear
    dtTrain <- dt[trainIdx]
    dtTest  <- dt[year == testY]
  }
  
  # Validation Split (taken from Train set)
  trainBursts <- unique(dtTrain$indiv_step_id)
  valBursts   <- sample(trainBursts, size = floor(length(trainBursts) * 0.2)) # 20% Val
  finalTrainBursts <- setdiff(trainBursts, valBursts)
  
  dtVal <- dtTrain[indiv_step_id %in% valBursts]
  dtTrain <- dtTrain[indiv_step_id %in% finalTrainBursts]
  
  # C. Shape Tensors (Helper)
  # Determine steps per burst (Mode)
  counts <- dt[, .N, by = indiv_step_id]
  steps <- as.numeric(names(sort(table(counts$N), decreasing=TRUE)[1]))
  
  to_tensor <- function(d) {
    if(nrow(d) < steps) return(NULL) # Handle empty
    # Filter to valid bursts
    valid <- d[, .N, by=indiv_step_id][N==steps, indiv_step_id]
    d <- d[indiv_step_id %in% valid]
    if(nrow(d)==0) return(NULL)
    
    mat <- as.matrix(d[, ..selectedFeatures])
    arr <- array(mat, dim=c(steps, uniqueN(d$indiv_step_id), length(selectedFeatures)))
    list(x=torch::torch_tensor(aperm(arr, c(2,1,3))), 
         ids=torch::torch_tensor(d[case_==TRUE, idIndex], dtype=torch::torch_long()))
  }
  
  list(
    train = to_tensor(dtTrain),
    val   = to_tensor(dtVal),
    test  = to_tensor(dtTest),
    meta  = list(nIn = length(selectedFeatures), nAnimals = max(dt$idIndex))
  )
}

.trainLuz <- function(tensors, params) {
  if(is.null(tensors$train) || is.null(tensors$val)) return(data.table(loss=NA))
  
  # Define Dataset
  ds <- torch::dataset(
    "ds", initialize=function(t){self$t<-t},
    .getitem=function(i){list(list(x=self$t$x[i,,], id=self$t$ids[i]), torch::torch_tensor(1, dtype=torch::torch_long()))},
    .length=function(){dim(self$t$x)[1]}
  )
  
  # Define Net
  Net <- torch::nn_module(
    "Net",
    initialize = function(nIn, nAnimals) {
      self$idEmb <- torch::nn_embedding(nAnimals+1, 8)
      self$fc1 <- torch::nn_linear(nIn+8, 128)
      self$fc2 <- torch::nn_linear(128, 64)
      self$out <- torch::nn_linear(64, 1)
      self$act <- torch::nn_selu()
    },
    forward = function(input) {
      x<-input$x; id<-input$id
      emb<-self$idEmb(id)$unsqueeze(2)$expand(c(-1,x$shape[2],-1))
      torch::torch_cat(list(x,emb),3) %>% self$fc1() %>% self$act() %>% self$fc2() %>% self$act() %>% self$out() %>% squeeze(3)
    }
  )
  
  # Fit
  fitted <- Net %>%
    luz::setup(loss = torch::nn_cross_entropy_loss(), optimizer = torch::optim_adam) %>%
    luz::set_hparams(nIn = tensors$meta$nIn, nAnimals = tensors$meta$nAnimals) %>%
    luz::set_opt_hparams(lr = params$learningRate) %>%
    luz::fit(
      torch::dataloader(ds(tensors$train), batch_size=params$batchSize, shuffle=TRUE),
      epochs = params$epochs,
      valid_data = torch::dataloader(ds(tensors$val), batch_size=params$batchSize, shuffle=FALSE),
      callbacks = list(luz::luz_callback_early_stopping(patience=5)),
      verbose = FALSE
    )
  
  # Evaluate Test
  model <- fitted$model
  model$eval()
  
  metrics <- data.table(val_loss = min(luz::get_metrics(fitted)$valid_loss$loss))
  
  if(!is.null(tensors$test)) {
    x <- tensors$test$x
    id <- tensors$test$ids
    y <- torch::torch_full(c(dim(x)[1]), 1)
    
    with_no_grad({
      scores <- model(list(x=x, id=id))
      loss <- torch::nnf_cross_entropy(scores, y)$item()
      preds <- torch::torch_argmax(scores, dim=2) + 1L
      acc <- sum((preds == y)$cpu()$numpy()) / length(preds)
    })
    metrics[, `:=`(test_loss = loss, test_acc = acc)]
  }
  
  return(metrics)
}
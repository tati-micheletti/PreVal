# DRAFT MODULE -- Global
defineModule(sim, list(
  name = "caribouNN_Global",
  description = "Trains a global NN on a subset to rank feature importance via Permutation",
  keywords = c("Neural Network", "Feature Importance", "Luz"),
  authors = c(person("Tati", "Micheletti", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(caribouNN_Global = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md"),
  reqdPkgs = list("data.table", "torch", "luz"),
  
  parameters = bindrows(
    defineParameter("sampleSize", "numeric", 0.20, 0.01, 1.0, "Fraction of data to use for ranking (0.2 = 20%)"),
    defineParameter("epochs", "numeric", 20, 1, 100, "Epochs for the ranking model (keep low for speed)"),
    defineParameter("batchSize", "numeric", 512, 32, 4096, "Batch size"),
    defineParameter("learningRate", "numeric", 0.0025, 0.0001, 0.1, "Learning rate"),
    defineParameter("device", "character", "cpu", NA, NA, "Device (cpu/cuda)")
  ),
  
  inputObjects = bindrows(
    expectsInput("extractedVariables", "data.table", "Raw feature table")
  ),
  
  outputObjects = bindrows(
    createsOutput("featurePriority", "character", "Ordered list of variable names based on importance"),
    createsOutput("globalModel", "luz_module_fitted", "The trained global model")
  )
))

## -----------------------------------------------------------------------------
## EVENT FUNCTIONS
## -----------------------------------------------------------------------------
doEvent.caribouNN_Global = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- scheduleEvent(sim, time(sim), "caribouNN_Global", "prepareData")
      sim <- scheduleEvent(sim, time(sim), "caribouNN_Global", "trainAndRank")
    },
    
    prepareData = {
      message("Global Model: Preparing Data & Creating Interactions...")
      dt <- copy(sim$extractedVariables)
      
      # 1. Cleaning
      dt <- na.omit(dt, cols = c("sl_", "ta_", "indiv_step_id", "id"))
      dt[, indiv_step_id := as.factor(indiv_step_id)]
      dt[, id := as.factor(id)]
      setorder(dt, indiv_step_id, -case_) # Critical sort
      
      # 2. Basic Transforms
      dt[, logSl := log(sl_ + 1)]
      dt[, cosTa := cos(ta_)]
      dt[, sinTa := sin(ta_)]
      
      varsToLog <- c("timeSinceFire", "timeSinceHarvest", "distpaved", "distunpaved", "distpolys")
      for(v in varsToLog) {
        if(paste0(v, "_start") %in% names(dt)) dt[, (paste0(v, "_startLog")) := log(get(paste0(v, "_start")) + 1)]
        if(paste0(v, "_end") %in% names(dt))   dt[, (paste0(v, "_endLog")) := log(get(paste0(v, "_end")) + 1)]
      }
      
      # 3. Create Interactions (The Full List)
      interVars <- c("prop_needleleaf_start", "prop_mixedforest_start", "prop_veg_start", "prop_wets_start",
                     "timeSinceFire_startLog", "timeSinceHarvest_startLog",
                     "distpaved_startLog", "distunpaved_startLog", "distpolys_startLog")
      
      interactionCols <- c()
      for (var in interVars) {
        if(var %in% names(dt)) {
          newCol <- paste0("inter_logSl_x_", var)
          dt[, (newCol) := logSl * get(var)]
          interactionCols <- c(interactionCols, newCol)
        }
      }
      
      # 4. Define All Candidates
      candidates <- c(
        "logSl", "cosTa", "sinTa",
        "prop_needleleaf_end", "prop_mixedforest_end", "prop_veg_end", "prop_wets_end",
        "timeSinceFire_endLog", "timeSinceHarvest_endLog", 
        "distpaved_endLog", "distunpaved_endLog", "distpolys_endLog",
        "prop_needleleaf_start", "prop_mixedforest_start", "prop_veg_start", "prop_wets_start",
        "timeSinceFire_startLog", "timeSinceHarvest_startLog",
        "distpaved_startLog", "distunpaved_startLog", "distpolys_startLog",
        interactionCols
      )
      # Filter to what actually exists in data
      sim$featureCandidates <- intersect(candidates, names(dt))
      
      # 5. Scale & Sample
      # We don't need all 1TB of data to find the ranking. We take a subset.
      message("Global Model: Subsetting ", P(sim)$sampleSize * 100, "% of bursts for ranking...")
      
      allBursts <- unique(dt$indiv_step_id)
      sampleBursts <- sample(allBursts, size = floor(length(allBursts) * P(sim)$sampleSize))
      dtSmall <- dt[indiv_step_id %in% sampleBursts]
      
      # Scale (Compute mean/sd on the subset is fine for ranking)
      dtSmall[, (sim$featureCandidates) := lapply(.SD, function(x) as.numeric(scale(x))), .SDcols = sim$featureCandidates]
      dtSmall[, idIndex := as.numeric(as.factor(id))]
      
      # Filter Step Count (Mode)
      counts <- dtSmall[, .N, by = indiv_step_id]
      steps <- as.numeric(names(sort(table(counts$N), decreasing=TRUE)[1]))
      dtSmall <- dtSmall[indiv_step_id %in% counts[N == steps, indiv_step_id]]
      
      # Shape Tensor
      sim$globalSteps <- steps
      sim$globalNAnimals <- max(dtSmall$idIndex)
      
      mat <- as.matrix(dtSmall[, ..sim$featureCandidates])
      arr <- array(mat, dim = c(steps, uniqueN(dtSmall$indiv_step_id), length(sim$featureCandidates)))
      
      # Store in sim
      sim$globalTensorX <- torch::torch_tensor(aperm(arr, c(2, 1, 3)), dtype = torch::torch_float())
      sim$globalTensorID <- torch::torch_tensor(dtSmall[case_==TRUE, idIndex], dtype = torch::torch_long())
      
      # Cleanup memory
      rm(dt, dtSmall, mat, arr); gc()
    },
    
    trainAndRank = {
      message("Global Model: Training...")
      
      # 1. Dataset & Dataloader
      ds <- dataset(
        "ds", initialize = function(x, id) {self$x<-x; self$id<-id},
        .getitem = function(i) { list(list(x=self$x[i,,], id=self$id[i]), torch_tensor(1, dtype=torch_long())) },
        .length = function() { self$x$size(1) }
      )
      dl <- dataloader(ds(sim$globalTensorX, sim$globalTensorID), batch_size=P(sim)$batchSize, shuffle=TRUE)
      
      # 2. Model
      Net <- nn_module(
        "Net",
        initialize = function(nIn, nAnimals) {
          self$idEmb <- nn_embedding(nAnimals+1, 8)
          self$fc1 <- nn_linear(nIn+8, 128)
          self$fc2 <- nn_linear(128, 64)
          self$out <- nn_linear(64, 1)
          self$act <- nn_selu()
        },
        forward = function(input) {
          x<-input$x; id<-input$id
          emb<-self$idEmb(id)$unsqueeze(2)$expand(c(-1,x$shape[2],-1))
          torch_cat(list(x,emb),3) %>% self$fc1() %>% self$act() %>% self$fc2() %>% self$act() %>% self$out() %>% squeeze(3)
        }
      )
      
      # 3. Fit
      fitted <- Net %>%
        setup(loss = nn_cross_entropy_loss(), optimizer = optim_adam) %>%
        set_hparams(nIn = length(sim$featureCandidates), nAnimals = sim$globalNAnimals) %>%
        set_opt_hparams(lr = P(sim)$learningRate) %>%
        fit(dl, epochs = P(sim)$epochs, verbose = TRUE)
      
      sim$globalModel <- fitted
      
      # 4. Permutation Importance
      message("Global Model: Calculating Permutation Importance...")
      model <- fitted$model
      model$eval()
      
      # Get Baseline
      base_loss <- 0; count <- 0
      # Use same DL for convenience (technically should be validation, but for RANKING it's okay)
      with_no_grad({
        coro::loop(for (b in dl) {
          scores <- model(b[[1]])$squeeze(3)
          base_loss <- base_loss + nnf_cross_entropy(scores, b[[2]])$item()
          count <- count + 1
        })
      })
      base_loss <- base_loss / count
      
      imp_df <- data.frame(Feature = sim$featureCandidates, Importance = 0)
      
      # Permute each feature
      # Note: We perform permutation on the CPU tensor to save GPU memory if needed
      # Or directly on the tensor if it fits.
      
      original_x <- sim$globalTensorX$clone()
      
      for(i in seq_along(sim$featureCandidates)) {
        # Shuffle column i
        perm_x <- original_x$clone()
        idx <- torch::torch_randperm(perm_x$size(1)) + 1L
        perm_x[,,i] <- perm_x[idx,,i]
        
        # Eval
        new_loss <- 0; count <- 0
        perm_ds <- ds(perm_x, sim$globalTensorID)
        perm_dl <- dataloader(perm_ds, batch_size=P(sim)$batchSize)
        
        with_no_grad({
          coro::loop(for (b in perm_dl) {
            scores <- model(b[[1]])$squeeze(3)
            new_loss <- new_loss + nnf_cross_entropy(scores, b[[2]])$item()
            count <- count + 1
          })
        })
        
        imp_val <- (new_loss / count) - base_loss
        imp_df$Importance[i] <- imp_val
        cat(".")
      }
      
      # 5. Set Output
      imp_df <- imp_df[order(-imp_df$Importance),]
      print(head(imp_df, 10))
      
      sim$featurePriority <- imp_df$Feature
      message("\nGlobal Feature Ranking Complete.")
    },
    
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}
# NN model
Require::Require("tictoc")
tic("TOTAL ELAPSED TIME TO FIT iSSA as NN: ")
Require::Require("torch")
Require::Require("luz")
torch::install_torch()
Require::Require("data.table")

# Data prep (constructing still needed covariates)
dt <- data.table::fread("~/projects/PreVal/outputs/iSSA/extractedFeatures_498a1edc8c19988e843def7542411d3e_2007_2022.csv")

# From the final table:
dt[, id := as.factor(id)]
dt[, indiv_step_id := as.factor(indiv_step_id)]
dt[, year := as.factor(year)]

# Construct covariates
  # Non-forest vegetation (veg)
  dt[, prop_veg_start := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_herbs_start", "prop_shrub_start", "prop_bryoids_start")]
  
  dt[, prop_veg_end := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_herbs_end", "prop_shrub_end", "prop_bryoids_end")]
  
  # Wetlands only
  dt[, prop_wets_start := prop_wetland_start]
  dt[, prop_wets_end   := prop_wetland_end]
  
  # Conifer - needleleaf + wet_treed
  dt[, prop_needleleaf_start := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_needleleaf_start", "prop_wet_treed_start")]
  
  dt[, prop_needleleaf_end := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_needleleaf_end", "prop_wet_treed_end")]
  
  # Mixed - mixed + deciduous
  dt[, prop_mixedforest_start := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_mixed_start", "prop_deciduous_start")]
  
  dt[, prop_mixedforest_end := rowSums(.SD, na.rm = TRUE),
     .SDcols = c("prop_mixed_end", "prop_deciduous_end")]
  
# 
# # iSSA model
# combi <- "each combination data"
# message("Starting iSSA for: ", combi)
# 
# tictoc::tic(paste0("Total time iSSA: ", combi))
# mod <- glmmTMB(
#   formula = as.formula(Par$iSSAformula),
#   family  = poisson(),
#   data    = dat,
#   map     = list(theta = factor(c(NA, 1:22))),
#   start   = list(theta = c(log(1000), rep(0, 22))) 
# )
# toc()
# models[[combi]] <- mod
# summaries[[combi]] <- summary(mod)
# message(paste0("Finished iSSA for: ", combi))
# Takes FOREEEEEVER...

# ---------------------------------------------------------
# PHASE 1: Data Preparation
# ---------------------------------------------------------

# 2. Filter NAs and Sort
# We drop rows with missing movement data
dt <- na.omit(dt, cols = c("sl_", "ta_", "indiv_step_id", "id"))
# Critical: Observed step (case_ == TRUE) must be first
setorder(dt, indiv_step_id, -case_)

# ---------------------------------------------------------
# 3. Base Feature Engineering
# ---------------------------------------------------------

# Movement Geometry
dt[, logSl := log(sl_ + 1)]   # "Speed"
dt[, cosTa := cos(ta_)]       # "Direction"
dt[, sinTa := sin(ta_)]

# Log Transforms for Context Variables (Start/End)
# These are the variables used in the GLMM formula
varsToLog <- c(
  "timeSinceFire_start", "timeSinceHarvest_start", 
  "distpaved_start", "distunpaved_start", "distpolys_start",
  "timeSinceFire_end", "timeSinceHarvest_end", 
  "distpaved_end", "distunpaved_end", "distpolys_end"
)

for (v in varsToLog) {
  newName <- paste0(v, "Log")
  dt[, (newName) := log(get(v) + 1)]
}

# ---------------------------------------------------------
# 4. Explicit Interaction Creation
# ---------------------------------------------------------
# Your colleague's model assumes that Speed (logSl) changes based on 
# where the animal starts. We calculate these manually to help the NN.

# List of start variables that interact with Speed in the GLMM
startVarsForInteraction <- c(
  "prop_needleleaf_start",
  "prop_mixedforest_start", 
  "prop_veg_start",
  "prop_wets_start",
  "timeSinceFire_startLog",
  "timeSinceHarvest_startLog",
  "distpaved_startLog",
  "distunpaved_startLog",
  "distpolys_startLog"
)

# Loop to create "logSl_x_Variable" columns
interactionCols <- c()
for (var in startVarsForInteraction) {
  newColName <- paste0("inter_logSl_x_", var)
  # Multiply Speed * Context
  dt[, (newColName) := logSl * get(var)]
  interactionCols <- c(interactionCols, newColName)
}

# ---------------------------------------------------------
# 5. Final Feature Selection
# ---------------------------------------------------------
# This list includes EVERYTHING: Movement, Selection, Context, and Interactions

featureCols <- c(
  # 1. Movement Kernel
  "logSl", "cosTa", "sinTa",
  
  # 2. End Variables (Habitat Selection - Where they want to go)
  "prop_needleleaf_end",
  "prop_mixedforest_end",
  "prop_veg_end",
  "prop_wets_end",
  "timeSinceFire_endLog",
  "timeSinceHarvest_endLog",
  "distpaved_endLog",
  "distunpaved_endLog",
  "distpolys_endLog",
  
  # 3. Start Variables (Context - Where they are now)
  "prop_needleleaf_start",
  "prop_mixedforest_start",
  "prop_veg_start",
  "prop_wets_start",
  "timeSinceFire_startLog",
  "timeSinceHarvest_startLog",
  "distpaved_startLog",
  "distunpaved_startLog",
  "distpolys_startLog",
  
  # 4. The Interaction Terms we just created
  interactionCols
)

# ---------------------------------------------------------
# 6. Scaling & Tensor Shaping
# ---------------------------------------------------------

# Scale Features (Standardize)
dt[, (featureCols) := lapply(.SD, function(x) as.numeric(scale(x))), .SDcols = featureCols]

# Handle IDs
dt[, idIndex := as.numeric(as.factor(id))]
nAnimals <- max(dt$idIndex)

# Handle Burst Sizes (Filter to Mode)
counts <- dt[, .N, by = indiv_step_id]
modeSteps <- as.numeric(names(sort(table(counts$N), decreasing=TRUE)[1]))

if (length(unique(counts$N)) > 1) {
  cat("Filtering to bursts with", modeSteps, "steps.\n")
  validSteps <- counts[N == modeSteps, indiv_step_id]
  dt <- dt[indiv_step_id %in% validSteps]
}
stepsPerBurst <- modeSteps

# Create Tensors
nBursts <- uniqueN(dt$indiv_step_id)
nFeatures <- length(featureCols)

# Matrix -> Array (Bursts, Steps, Features)
matData <- as.matrix(dt[, ..featureCols])
arrayData <- array(matData, dim = c(stepsPerBurst, nBursts, nFeatures))
arrayData <- aperm(arrayData, c(2, 1, 3)) 

# ID Vector
burstIds <- dt[case_ == TRUE, idIndex]

cat("--- Data Ready ---\n")
cat("Input Features:", nFeatures, "\n")
cat("Total Bursts:", nBursts, "\n")

# ---------------------------------------------------------
# PHASE 2: Model & Training
# ---------------------------------------------------------

# Dataset Class
ssfDataset <- dataset(
  name = "ssfDataset",
  initialize = function(data, ids) {
    self$data <- torch_tensor(data, dtype = torch_float())
    self$ids  <- torch_tensor(ids, dtype = torch_long())
    self$n    <- dim(data)[1]
  },
  .getitem = function(i) {
    list(x = self$data[i,,], id = self$ids[i], y = torch_tensor(1, dtype=torch_long()))
  },
  .length = function() { self$n }
)

# Aggressive Model Architecture
# No Dropout, SELU activation, Wider Layers
SsfNetwork <- nn_module(
  "SsfNetwork",
  initialize = function(nIn, nAnimals, embDim = 8) {
    self$idEmb <- nn_embedding(nAnimals + 1, embDim)
    
    # Wider layers to capture complex interactions
    self$fc1 <- nn_linear(nIn + embDim, 256)
    self$fc2 <- nn_linear(256, 128)
    self$fc3 <- nn_linear(128, 64)
    self$out <- nn_linear(64, 1)
    
    self$act <- nn_selu() # Self-Normalizing activation (good for deep tabular nets)
  },
  forward = function(x, id) {
    # Embed ID and expand to match steps
    emb <- self$idEmb(id)
    emb <- emb$unsqueeze(2)$expand(c(-1, x$shape[2], -1))
    
    # Concatenate
    x <- torch_cat(list(x, emb), dim = 3)
    
    # Forward
    x %>% 
      self$fc1() %>% self$act() %>% 
      self$fc2() %>% self$act() %>% 
      self$fc3() %>% self$act() %>% 
      self$out()
  }
)

# Setup
device <- if (cuda_is_available()) torch_device("cuda") else torch_device("cpu")
trainDs <- ssfDataset(arrayData, burstIds)
trainDl <- dataloader(trainDs, batch_size = 512, shuffle = TRUE) # Bigger batch for stability

model <- SsfNetwork(nIn = nFeatures, nAnimals = nAnimals)
model$to(device = device)

# ---------------------------------------------------------
# SETUP AUTOMATION
# ---------------------------------------------------------

# Optimizer (Start aggressive)
optimizer <- optim_adam(model$parameters, lr = 0.0025)

# Scheduler for learning rate decrease when hitting current optimum
# mode="min": We want loss to go DOWN
# factor=0.5: Cut LR in half when stuck
# patience=5: Wait 5 epochs before cutting
# verbose=TRUE: Tell us when it happens
scheduler <- lr_reduce_on_plateau(optimizer, mode = "min", factor = 0.5, patience = 5, verbose = TRUE)

# 3. Track the best model
best_loss <- Inf
model_save_path <- paste0("best_model_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pt")

# ---------------------------------------------------------
# TRAINING LOOP (With Automation)
# ---------------------------------------------------------
epochs <- 50 # increased since we now have a scheduler handling the fine-tuning
cat("Starting Auto-Tuned Training...\n")

for (epoch in 1:epochs) {
  model$train()
  losses <- c()
  
  coro::loop(for (b in trainDl) {
    x <- b$x$to(device = device)
    id <- b$id$to(device = device)
    y <- b$y$to(device = device)$squeeze()
    
    optimizer$zero_grad()
    
    scores <- model(x, id)$squeeze(3)
    loss <- nnf_cross_entropy(scores, y)
    
    loss$backward()
    optimizer$step()
    losses <- c(losses, loss$item())
  })
  
  avg_loss <- mean(losses)
  
  # 4. Step the Scheduler
  # This checks if loss improved. If not for 5 epochs, it lowers LR.
  scheduler$step(avg_loss)
  
  # 5. Save if Best
  if (avg_loss < best_loss) {
    best_loss <- avg_loss
    torch_save(model$state_dict(), model_save_path)
    cat(sprintf("Epoch %02d | Loss: %.4f (New Best!) \n", epoch, avg_loss))
  } else {
    cat(sprintf("Epoch %02d | Loss: %.4f \n", epoch, avg_loss))
  }
}

cat("Training Finished. Best Loss:", best_loss, "\n")

# ---------------------------------------------------------
# RELOAD BEST MODEL
# ---------------------------------------------------------
# Load the weights from the best epoch, ignoring the last epoch
model$load_state_dict(torch_load(model_save_path))
cat("Loaded best model state.\n")

toc()



# 1. Check the true structure using the UNIQUE identifier (indiv_step_id)
# We expect this to be 11 or 21, not 22.
dt[, .N, by = indiv_step_id][, table(N)]

# 2. Check how many "Observed" steps are in each of your current groups
# If you see '2', it proves we merged two animals by accident.
dt[, .(True_Count = sum(case_)), by = indiv_step_id][, table(True_Count)]


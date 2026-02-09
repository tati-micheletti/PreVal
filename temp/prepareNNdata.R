prepareNNdata <- function(extractedVariables, pathX, pathID){
  message("Preparing Data...")
  
  # 1. Create a full copy so we don't modify the input by reference
  dt <- copy(extractedVariables)
  
  # 2. Basic Cleaning
  # We only omit rows missing the essential movement/id data
  dt <- na.omit(dt, cols = c("sl_", "ta_", "indiv_step_id", "id"))
  
  # ----------------------------------------------------------------------
  # 3. FILTER VALID STRATA (BURSTS)
  # ----------------------------------------------------------------------
  originalTotalStrata <- uniqueN(dt$indiv_step_id)
  message("Validating ", originalTotalStrata, " strata...")
  
  # A. Determine Target Step Count (The Mode, usually 11)
  counts <- dt[, .N, by = indiv_step_id]
  steps <- as.numeric(names(sort(table(counts$N), decreasing=TRUE)[1]))
  
  # B. Identify Valid Strata
  # Rule: Must have correct number of steps AND exactly 1 observed case
  valid_stats <- dt[, .(n_rows = .N, n_case = sum(case_)), by = indiv_step_id]
  valid_ids <- valid_stats[n_rows == steps & n_case == 1, indiv_step_id]
  
  # C. Apply Filter
  dt <- dt[indiv_step_id %in% valid_ids]
  setorder(dt, indiv_step_id, -case_) # Case (TRUE) is always index 1
  
  # D. Reporting
  kept_pct <- round((length(valid_ids) / originalTotalStrata) * 100, 1)
  message("Kept ", length(valid_ids), " valid strata (", 
          kept_pct, "% of ", originalTotalStrata, 
          "). Proceeding to Engineering...")
  
  # ----------------------------------------------------------------------
  # 4. FEATURE ENGINEERING (ADDITIVE)
  # ----------------------------------------------------------------------
  
  # Vegetation Aggregates
  dt[, prop_veg_start := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_herbs_start", "prop_shrub_start", "prop_bryoids_start"))]
  dt[, prop_veg_end := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_herbs_end", "prop_shrub_end", "prop_bryoids_end"))]
  dt[, prop_wets_start := prop_wetland_start]
  dt[, prop_wets_end   := prop_wetland_end]
  dt[, prop_needleleaf_start := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_needleleaf_start", "prop_wet_treed_start"))]
  dt[, prop_needleleaf_end := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_needleleaf_end", "prop_wet_treed_end"))]
  dt[, prop_mixedforest_start := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_mixed_start", "prop_deciduous_start"))]
  dt[, prop_mixedforest_end := rowSums(.SD, na.rm = TRUE), .SDcols = intersect(names(dt), c("prop_mixed_end", "prop_deciduous_end"))]
  
  # Transforms
  dt[, logSl := log(sl_ + 1)]
  dt[, cosTa := cos(ta_)]
  dt[, sinTa := sin(ta_)]
  
  varsToLog <- c("timeSinceFire", "timeSinceHarvest", "distpaved", "distunpaved", "distpolys")
  for(v in varsToLog) {
    if(paste0(v, "_start") %in% names(dt)) dt[, (paste0(v, "_startLog")) := log(get(paste0(v, "_start")) + 1)]
    if(paste0(v, "_end") %in% names(dt))   dt[, (paste0(v, "_endLog")) := log(get(paste0(v, "_end")) + 1)]
  }
  
  # Fast Interactions (Using set())
  interVars <- c("prop_needleleaf_start", "prop_mixedforest_start", "prop_veg_start", "prop_wets_start",
                 "timeSinceFire_startLog", "timeSinceHarvest_startLog",
                 "distpaved_startLog", "distunpaved_startLog", "distpolys_startLog")
  
  vec_logSl <- dt$logSl
  for (var in interVars) {
    if(var %in% names(dt)) {
      set(dt, j = paste0("inter_logSl_x_", var), value = vec_logSl * dt[[var]])
    }
  }
  
  # ----------------------------------------------------------------------
  # 5. CANDIDATE IDENTIFICATION & SCALING
  # ----------------------------------------------------------------------
  candidates <- c(
    "logSl", "cosTa", "sinTa",
    "prop_needleleaf_end", "prop_mixedforest_end", "prop_veg_end", "prop_wets_end",
    "timeSinceFire_endLog", "timeSinceHarvest_endLog", 
    "distpaved_endLog", "distunpaved_endLog", "distpolys_endLog",
    "prop_needleleaf_start", "prop_mixedforest_start", "prop_veg_start", "prop_wets_start",
    "timeSinceFire_startLog", "timeSinceHarvest_startLog",
    "distpaved_startLog", "distunpaved_startLog", "distpolys_startLog",
    grep("^inter_", names(dt), value = TRUE)
  )
  featureCandidates <- intersect(candidates, names(dt))
  
  # Safe Scaling (Mean 0, SD 1)
  dt[, (featureCandidates) := lapply(.SD, function(x) {
    val <- as.numeric(scale(x))
    val[is.na(val)] <- 0 
    return(val)
  }), .SDcols = featureCandidates]
  
  # ID Mapping
  dt[, idIndex := as.numeric(as.factor(id))]
  dtglobalNAnimals <- max(dt$idIndex)
  
  # ----------------------------------------------------------------------
  # 6. TENSOR SHAPING & SAVING
  # ----------------------------------------------------------------------
  message("Shaping and saving tensors to disk...")
  
  # Convert to matrix of predictors
  mat <- as.matrix(dt[, ..featureCandidates])
  
  # Reshape: (Bursts, Steps, Features)
  # nrow / steps = total number of unique bursts
  arr <- array(mat, dim = c(steps, nrow(dt)/steps, length(featureCandidates)))
  
  # Save Environment Tensor (X)
  globalTensorX <- torch_tensor(aperm(arr, c(2, 1, 3)), dtype = torch_float())
  torch_save(globalTensorX, pathX)
  
  # Save ID Tensor (ID)
  # Take the ID of the first step of every burst (the observed case)
  valid_ids <- dt[case_ == TRUE, idIndex]
  globalTensorID <- torch_tensor(valid_ids, dtype = torch_long())
  torch_save(globalTensorID, pathID)
  
  # ----------------------------------------------------------------------
  # 7. RETURN
  # ----------------------------------------------------------------------
  return(list(
    modelPrep = list(
      globalTensorX = pathX,
      globalTensorID = pathID,
      globalNAnimals = dtglobalNAnimals,
      featureCandidates = featureCandidates
    ),
    preparedDataFinal = dt # The full table with all original and new columns
  ))
}

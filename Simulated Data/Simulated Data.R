# Load required libraries
if(!require("Require")){
  install.packages("Require")
}
library("Require")
Require("data.table")
Require("terra")
Require("spatialEco")
Require("RColorBrewer")
Require("SpaDES.project")
Require("reproducible")
Require("SpaDES.core")


# Define the environment using a SpatRaster
landscape_size <- 5000  # Size of the landscape
resolution <- 10  # Grid resolution
ncols <- nrows <- landscape_size / resolution  # Number of rows and columns
landscape <- rast(nrows = nrows, ncols = ncols, xmin = 0, xmax = landscape_size, ymin = 0, ymax = landscape_size)
smartPursue <- TRUE
terrainThreshold <- 0.2
maxPursueSteps <- 5
coolDownSteps <- 4
n_smurfs <- 75
# Define a "danger zone"
threat_distance <- 180  
totalSteps <- 100

# Add terrain heterogeneity (e.g., elevation) with Gaussian noise
values(landscape) <- rnorm(ncell(landscape), mean = 50, sd = 15)

# Create a custom Gaussian kernel
gaussian_kernel <- function(size, sigma) {
  # Create a grid of coordinates
  grid <- expand.grid(-size:size, -size:size)
  dist <- sqrt(grid[, 1]^2 + grid[, 2]^2)
  kernel <- exp(-(dist^2) / (2 * sigma^2))
  kernel <- kernel / sum(kernel)  # Normalize to sum to 1
  matrix(kernel, nrow = 2 * size + 1, ncol = 2 * size + 1)
}

# Generate the Gaussian kernel
kernel_size <- 30  # Radius of the kernel (in cells)
sigma <- 8  # Standard deviation of the Gaussian
kernel <- gaussian_kernel(kernel_size, sigma)

# Apply Gaussian smoothing
landscape <- focal(landscape, w = kernel, fun = mean, na.rm = TRUE)
# Generate Altitude
landscape <- terrain(landscape)
values(landscape) <- 100 * (landscape[]^2)  # Exaggerate for going from slope to altitude

# Define the cropping percentage (e.g., 80% of the raster to keep)
crop_percentage <- 90  # Keep 80% of the raster

# Calculate the cropping extent
original_extent <- ext(landscape)
crop_factor <- crop_percentage / 100
new_extent <- ext(
  original_extent$xmin + (1 - crop_factor) * (original_extent$xmax - original_extent$xmin) / 2,
  original_extent$xmax - (1 - crop_factor) * (original_extent$xmax - original_extent$xmin) / 2,
  original_extent$ymin + (1 - crop_factor) * (original_extent$ymax - original_extent$ymin) / 2,
  original_extent$ymax - (1 - crop_factor) * (original_extent$ymax - original_extent$ymin) / 2
)

# Crop the raster
landscape <- crop(landscape, new_extent)

# Reset extent and update lansdcape size
ncols <- ncol(landscape)
nrows <- nrow(landscape)
resolution <- res(landscape)[1]  # Assuming square cells
new_extent <- ext(0, ncols * resolution, 0, nrows * resolution)
ext(landscape) <- new_extent
landscape_size <- (landscape_size*crop_percentage)/100

# Initialize entities
smurf_positions <- data.table(
  id = 1:n_smurfs,
  x = runif(n_smurfs, min = 0, max = landscape_size),
  y = runif(n_smurfs, min = 0, max = landscape_size),
  captured = FALSE
)

village <- c(landscape_size * 0.09, landscape_size * 0.09) # Place the village on the bottom left corner
gargamel <- c(landscape_size * 0.5, landscape_size * 0.5)  # Start in the center
azrael <- c(landscape_size * 0.6, landscape_size * 0.6)  # Close to Gargamel
mycolors <- colorRampPalette(brewer.pal(9, "Greens"))(50)
plot(landscape, col = rev(mycolors))
points(village[1], village[2], col = "yellow", pch = 18, cex = 3)  # Village

# Append a landcover type to the raster -- first and last rows and columns have 0 as forest type, which
# also influences the
firstRow <- raster::cellFromRow(landscape, 1)  # Pixel numbers for the first row
lastRow <- raster::cellFromRow(landscape, NROW(landscape))  # Pixel numbers for the last row
# Get the first and last columns
firstCol <- raster::cellFromCol(landscape, 1)  # Pixel numbers for the first column
lastCol <- raster::cellFromCol(landscape, NCOL(landscape))  # Pixel numbers for the last column
completeAvoidance <- unique(c(firstRow, lastRow, firstCol, lastCol))

# Parameters
# Smurfs prioritize safety (avoiding Gargamel and Azrael), seeking the village, and maintaining group cohesion.
# smurfsParameters[1]: Attraction to the village.
# smurfsParameters[2]: Repulsion from Gargamel.
# smurfsParameters[3]: Repulsion from Azrael.
# smurfsParameters[4]: Cohesion within the group (moving closer to the group's center).
# smurfsParameters[5]: Attraction to mushrooms, available primarily in lower lands
smurfsParameters <- c(0.2, 10.0, 10.0, 0.2, 10.0) # Smurf movement weights
names(smurfsParameters) <- c("villageAttaction", 
                             "GargamelRepulsion", 
                             "AzraelRepulsion", 
                             "groupCohesion",
                             "mushroomAttraction")
# Gargamel moves towards the nearest Smurf while being influenced by the terrain's difficulty.
# gargamelsParameters[1]: Attraction to the closest Smurf.
# gargamelsParameters[2]: Influence of the altitude.
gargamelsParameters <- c(10.0, 0.02)  # Gargamel movement weights
names(gargamelsParameters) <- c("smurfAttaction", 
                                "difficultTerrainRepulsion")

# Azrael is motivated by its predatory instincts to follow Gargamel and target Smurfs.
# azraelsParameters[1]: Attraction to the closest Smurf.
# azraelsParameters[2]: Attraction to Gargamel (following Gargamel's lead).
# azraelsParameters[3]: Terrain influence.
azraelsParameters <- c(10.0, 5.0, 0.02)  # Azrael behavior weights
names(azraelsParameters) <- c("smurfAttaction", 
                              "gargamelAttaction", 
                              "difficultTerrainRepulsion")


# Movement limitations
max_smurf_step <- 60
max_azrael_step <- 1.8 * max_smurf_step
max_gargamel_step <- 1.4 * max_smurf_step

# Function to calculate Euclidean distance
distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}
# Function to extract terrain value at a location
get_terrain_value <- function(x, y, raster) {
  # Create coordinates matrix for extraction
  coords <- cbind(x, y)
  # Extract values from the raster
  extracted <- terra::extract(raster, coords)
  # Check if the extraction returns a valid data frame
  if (is.null(extracted) || nrow(extracted) == 0) {
    return(rep(1, length(x)))  # Default terrain factor for invalid coordinates
  }
  
  # Extract the appropriate column based on raster type
  return(extracted[, 1])  # Use the first column for single-layer rasters
}

# Helper function to calculate elevation gradients
gradient <- function(x, y, raster, direction) {
  res <- res(raster)  # Resolution of the raster
  if (direction == "x") {
    (get_terrain_value(x + res[1], y, raster) - get_terrain_value(x - res[1], y, raster)) / (2 * res[1])
  } else if (direction == "y") {
    (get_terrain_value(x, y + res[2], raster) - get_terrain_value(x, y - res[2], raster)) / (2 * res[2])
  } else {
    stop("Invalid direction. Use 'x' or 'y'.")
  }
}

# Initialize pursuit and cooldown state if not present
gargamelState <- data.table(pursued_smurf = NA,
                            pursuit_steps = 0,
                            cooldown_steps = 0)

azraelState <- data.table(pursued_smurf = NA,
                          pursuit_steps = 0,
                          cooldown_steps = 0)

# Smurf movement function with flee logic and capture toggles
move_smurf <- function(smurf_positions, 
                       village, 
                       gargamel, 
                       azrael, 
                       time_step = tstep,
                       smurfsParameters, 
                       terrainThreshold,
                       landscape, 
                       max_smurf_step) {
  
  # Add danger zones for Gargamel and Azrael
  smurf_positions[, dangerGG := distance(x, y, gargamel[1], gargamel[2]) < threat_distance]
  smurf_positions[, dangerAZ := distance(x, y, azrael[1], azrael[2]) < threat_distance]
  
  smurf_positions[, `:=`(group_cohesion_x = mean(x),
                         group_cohesion_y = mean(y))]

  # Add a column to track how long Smurfs have stayed in the same place
  if (!"stay_counter" %in% colnames(smurf_positions)) {
    smurf_positions[, stay_counter := 0]
  }
  
  # Ensure 'visited_locations' is properly initialized
  if (!"visited_locations" %in% colnames(smurf_positions)) {
    smurf_positions[, visited_locations := list()]
  }
  
  for (smurf in unique(smurf_positions$id)) {
    
    xSmurf <- smurf_positions[id == smurf,]
    
    # Retrieve visited coordinates list
    visited_coords <- xSmurf$visited_locations[[1]]
    
    inDanger  <- if (any(xSmurf$dangerGG,
                         xSmurf$dangerAZ)) TRUE else FALSE
    dangerBy <- if (xSmurf$dangerGG) "GG" else if (xSmurf$dangerAZ) "AZ" else NULL
    
    if (inDanger) {
      if (dangerBy == "GG"){
        byX <- gargamel[1]
        byY <- gargamel[2]
        repulse <- smurfsParameters["GargamelRepulsion"]
      } else {
        byX <- azrael[1]
        byY <- azrael[2]
        repulse <- smurfsParameters["AzraelRepulsion"]
      }
      Dx <- repulse * (xSmurf$x - byX)
      Dy <- repulse * (xSmurf$y - byY)
      
      if (any(Dx == 0,
              Dy == 0)){

        Dx <-  smurfsParameters["villageAttaction"] * (village[1] - xSmurf$x)
        Dy <-  smurfsParameters["villageAttaction"] * (village[2] - xSmurf$y)
        
      } 
      
      # Limit movement to max_step
      # Proportionately distribute the pull among the direction to the max step
      px <- (max_smurf_step * abs(Dx))/(abs(Dx)+abs(Dy))
      py <- (max_smurf_step * abs(Dy))/(abs(Dx)+abs(Dy))
      
      # Make sure to add the right direction
      dirx <- if (Dx < 0) -1 else 1
      diry <- if (Dy < 0) -1 else 1
      Dx <- px * dirx  
      Dy <- py * diry  

    } else {
      
      # Not in danger: Check if in a good place for mushrooms
      current_Alt <- terra::extract(landscape, cbind(xSmurf$x, xSmurf$y))$slope
      altitude_threshold <- quantile(values(landscape), terrainThreshold, na.rm = TRUE)  # Lower 15% slope threshold
      
      if (all(current_Alt < altitude_threshold, 
              xSmurf$stay_counter < 2)) {
        # Stay in the same place for 3 steps if slope is good
        smurf_positions[id == smurf, stay_counter := stay_counter + 1]
        Dx <- 0
        Dy <- 0
        
      } else {
        
        # Otherwise, move toward mushrooms, group cohesion, and the village
        smurf_positions[id == smurf, stay_counter := 0]  # Reset stay counter
        
        #TODO Implement an alternative way? 
        # 1. Buffer the location by max step
        # 2. Extract all cells within the buffer
        sExMin <- pmax(xSmurf$x - max_smurf_step, 0)
        sExMax <- pmin(xSmurf$x + max_smurf_step, xmax(landscape))
        sEyMin <- pmax(xSmurf$y - max_smurf_step, 0)
        sEyMax <- pmin(xSmurf$y + max_smurf_step, ymax(landscape))
        
        # Extract nearby cells and elevations
        nearby_cells <- cells(landscape, terra::ext(c(sExMin, sExMax, sEyMin, sEyMax)))
        # nearby_coords <- raster::xyFromCell(landscape, nearby_cells)
        nearby_elevations <- unlist(terra::extract(landscape, nearby_cells))
        
        # Filter out already visited cells
        unvisited_cells <- setdiff(nearby_cells, unlist(xSmurf$visited_locations))
        
        if (length(unvisited_cells) > 0) {
          lowest_index <- which.min(nearby_elevations[nearby_cells %in% unvisited_cells])
          target_cell <- unvisited_cells[lowest_index]
        } else {
          # If the best nearby cells are visited, pick the lowest available
          target_cell <- nearby_cells[which.min(nearby_elevations)]
        }
        
        # Convert target cell to (x, y) coordinates
        target_coords <- terra::xyFromCell(landscape, target_cell)
        lowest_x <- target_coords[1]
        lowest_y <- target_coords[2]
        
        # Combine influences: Lowest elevation, group cohesion, and village
        Dx <- smurfsParameters["mushroomAttraction"] * (lowest_x - xSmurf$x) +
          smurfsParameters["groupCohesion"] * (xSmurf$group_cohesion_x - xSmurf$x) +
          smurfsParameters["villageAttaction"] * (village[1] - xSmurf$x)
        Dy <- smurfsParameters["mushroomAttraction"] * (lowest_y - xSmurf$y) +
          smurfsParameters["groupCohesion"] * (xSmurf$group_cohesion_y - xSmurf$y) +
          smurfsParameters["villageAttaction"] * (village[2] - xSmurf$y)
        
        # Now that we know the strength of pull, we need to re-limit the movement
        # Limit movement to max_step
        # Proportionately distribute the pull among the direction to the max step
        px <- (max_smurf_step * abs(Dx))/(abs(Dx)+abs(Dy))
        py <- (max_smurf_step * abs(Dy))/(abs(Dx)+abs(Dy))
        
        # Make sure to add the right direction
        dirx <- if (Dx < 0) -1 else 1
        diry <- if (Dy < 0) -1 else 1
        Dx <- px * dirx  
        Dy <- py * diry  
        
        # Add location of mushroom extensive search to visited locations
        if (xSmurf$stay_counter > 1){
          smurf_positions[id == smurf, visited_locations := c(unlist(smurf_positions[id == smurf, 
                                                                                     visited_locations]), 
                                                              target_cell)]
          
      }
      }
    }
    
    smurf_positions[id == smurf, dx := Dx]
    smurf_positions[id == smurf, dy := Dy]
    
  }
  
  # update current positions
  smurf_positions[, `:=`(
    x = pmin(pmax(x + ifelse(is.na(dx), 0, dx), 0), landscape_size),
    y = pmin(pmax(y + ifelse(is.na(dy), 0, dy), 0), landscape_size)
  )]
  
  return(smurf_positions)
}

move_villain <- function(villain,
                         villainState,
                         smurf_positions, 
                         villainParameters, 
                         landscape, 
                         maxPursueSteps,
                         coolDownSteps,
                         gargamel = NULL,
                         terrainThreshold,
                         max_villain_step, 
                         time_step, 
                         smartPursue) {
  
  # Filter Smurfs within visual distance
  nearby_smurfs <- smurf_positions[distance(x, y, villain[1], villain[2]) <= threat_distance]
  nearby_smurfs[, distanceToThreat := distance(x, y, villain[1], villain[2])]
  
  # Cooldown logic: Ignore Smurfs if in cooldown mode
  if (villainState$cooldown_steps > 0) {
    villainState$cooldown_steps <- villainState$cooldown_steps - 1
    nearby_smurfs <- data.table()  # Ensure villain doesn’t pursue any Smurf
    # Make sure villain stays put during cool down
  } else {
    # Select a target Smurf if there are Smurfs in range
    if (nrow(nearby_smurfs) > 0) {
      if (any(is.na(villainState$pursued_smurf), !(villainState$pursued_smurf %in% nearby_smurfs$id))) {
        # Randomly pick a new Smurf to pursue
        if (smartPursue){
          villainState$pursued_smurf <- nearby_smurfs[distanceToThreat == min(nearby_smurfs$distanceToThreat), id]
        } else {
          villainState$pursued_smurf <- if (length(nearby_smurfs$id) == 1) nearby_smurfs$id else sample(x = nearby_smurfs$id, size = 1)
        }
        villainState$pursuit_steps <- 0  # Reset pursuit steps
      }
    } else {
      # No Smurfs in range, reset target
      villainState$pursued_smurf <- NA
    }
    
    # Movement logic
    if (!is.na(villainState$pursued_smurf)) {
      # Pursue the target Smurf
      target_smurf <- smurf_positions[id == villainState$pursued_smurf]
      
      # Calculate distance
      target_smurf[, distanceToThreat := distance(x, y, villain[1], villain[2])]
      
      # Calculate terrain factor at villain's position
      terrain_factor <- get_terrain_value(villain[1], villain[2], landscape)
      
      # Compute movement toward the target
        dx <- (villainParameters[["smurfAttaction"]] * (target_smurf$x - villain[1]))
        dy <- (villainParameters[["smurfAttaction"]] * (target_smurf$y - villain[2]))

      # Adjust movement based on terrain
      dx <- dx / (1 + (terrain_factor * villainParameters[["difficultTerrainRepulsion"]]))
      dy <- dy / (1 + (terrain_factor * villainParameters[["difficultTerrainRepulsion"]]))
      
      # Proportionately distribute the pull among the direction to the max step
      px <- (max_villain_step * abs(dx))/(abs(dx)+abs(dy))
      py <- (max_villain_step * abs(dy))/(abs(dx)+abs(dy))
      
      # If a villain is on top of a smurf, px and py become NaN (division by 0)
      if (is.nan(px)){
        px <- 0
      }
      if (is.nan(py)){
        py <- 0
      }
      
      # Make sure to add the right direction
      dirx <- if (dx < 0) -1 else 1
      diry <- if (dy < 0) -1 else 1
      dx <- px * dirx  
      dy <- py * diry  
      
      # Constraint the movement to the smurf's position
      # Compute the eucledian distance I could do
      maxDist <- sqrt(dx^2 + dy^2)
      # Compute the eucledian distance to the smurf
      distSmurf <- distance(x1 = villain[1], x2 = target_smurf$x, y1 = villain[2], y2 = target_smurf$y)
      
      if (maxDist < distSmurf){
        # If the maxDist is smaller, it won't reach the smurf,
        # so move to the calculated position
        villain[1] <- villain[1] + dx
        villain[2] <- villain[2] + dy
      } else {
        # If maxDist is higer, go straight to the smurf's position
        villain[1] <- target_smurf$x
        villain[2] <- target_smurf$y
      }
      
      # Update pursuit state
      villainState$pursuit_steps <- villainState$pursuit_steps + 1
      
      # Check if villain gives up after maxPursueSteps steps
      if (villainState$pursuit_steps >= maxPursueSteps) {
        villainState$pursued_smurf <- NA  # Stop targeting the Smurf
        villainState$pursuit_steps <- 0   # Reset pursuit steps
        villainState$cooldown_steps <- coolDownSteps   # Enter cooldown for 2 steps
      }
    } else {
      
      # Compute movement toward the target
      if ("gargamelAttaction" %in% names(villainParameters)){
        # This is then Azrael --> Search for Gargamel
        dx <- (villainParameters[["gargamelAttaction"]] * (gargamel[1] - villain[1]))
        dy <- (villainParameters[["gargamelAttaction"]] * (gargamel[2] - villain[2]))
        
        # Proportionately distribute the pull among the direction to the max step
        px <- (max_villain_step * abs(dx))/(abs(dx)+abs(dy))
        py <- (max_villain_step * abs(dy))/(abs(dx)+abs(dy))
        
        # If a villain is on top of a smurf, px and py become NaN (division by 0)
        if (is.nan(px)){
          px <- 0
        }
        if (is.nan(py)){
          py <- 0
        }
        
        # Make sure to add the right direction
        dirx <- if (dx < 0) -1 else 1
        diry <- if (dy < 0) -1 else 1
        dx <- px * dirx  
        dy <- py * diry  
        
      } else {
        # This is then Gargamel --> Search for smurfs
        # No Smurfs in range: Actively search for Smurfs 
        sExMin <- pmax(villain[1] - max_villain_step, 0)
        sExMax <- pmin(villain[1] + max_villain_step, xmax(landscape))
        sEyMin <- pmax(villain[2] - max_villain_step, 0)
        sEyMax <- pmin(villain[2] + max_villain_step, ymax(landscape))
        
        # Extract nearby cells and elevations
        nearby_cells <- cells(landscape, terra::ext(c(sExMin, sExMax, sEyMin, sEyMax)))
        nearby_coords <- raster::xyFromCell(landscape, nearby_cells)
        nearby_elevations <- terra::extract(landscape, nearby_coords)
        
        # Randomly pick a point within the elevation threshold
        lowest_indices <- which(nearby_elevations$slope < terrainThreshold)
        lowest_index <- if (length(lowest_indices) == 0){
          # If there are no values, pick the lowest
          which.min(nearby_elevations$slope)
        } else {
          if (length(lowest_indices) > 1){
            # If there are more values, randomly pick one 
            sample(lowest_indices, 1)
          } else {
            tCount <- 0
            while (!length(lowest_indices) > 1){
              terrainThresholdTemp <- terrainThreshold + (tCount * 2) 
              lowest_indices <- which(nearby_elevations$slope < terrainThresholdTemp)
              tCount <- tCount + 1
            }
            sample(lowest_indices, 1)
          }
        }
        lowest_x <- nearby_coords[lowest_index, 1]
        lowest_y <- nearby_coords[lowest_index, 2]
        
        # Compute movement toward the lowest elevation
        dx <- lowest_x - villain[1]
        dy <- lowest_y - villain[2]
      }
      
      # Update villain's position
      villain[1] <- villain[1] + dx
      villain[2] <- villain[2] + dy
    }
    
  }
  
  return(list(villain = villain, villainState = villainState))
}


# Updated simulation loop
simulate <- function(steps, 
                     smurf_positions, 
                     village, 
                     gargamel, 
                     azrael, 
                     smurfsParameters, 
                     gargamelsParameters, 
                     azraelsParameters, 
                     maxPursueSteps,
                     coolDownSteps,
                     terrainThreshold,
                     smartPursue,
                     landscape, 
                     max_smurf_step, 
                     max_azrael_step, 
                     max_gargamel_step) {
  
  for (tstep in 1:steps) {
    # Smurf movement
    if (tstep == 1){
      smurfData <- smurf_positions[, c("id", "x", "y")]
      names(smurfData)[names(smurfData) == "id"] <- "Entity_Number"
      smurfData[, Time := 0]
      smurfData[, Entity := "Smurf"]
      
      gargamelData <- data.table(Entity_Number = 1,
                                 x = gargamel[1],
                                 y = gargamel[2],
                                 Time = 0,
                                 Entity = "Gargamel")
      
      azraelData <- data.table(Entity_Number = 1,
                               x = azrael[1],
                               y = azrael[2],
                               Time = 0,
                               Entity = "Azrael")
      
      finalDataset <- rbindlist(list(smurfData, gargamelData, azraelData), 
                                use.names = TRUE)
    }
    
    smurf_positions <- move_smurf(smurf_positions = smurf_positions, 
                                  village = village, 
                                  gargamel = gargamel, 
                                  azrael = azrael,
                                  terrainThreshold = terrainThreshold,
                                  time_step = tstep,
                                  smurfsParameters = smurfsParameters, 
                                  landscape = landscape, 
                                  max_smurf_step = max_smurf_step)
    
    # Gargamel movement
    gargamel_result <- move_villain(villain = gargamel, 
                                    villainState = gargamelState,
                                    smurf_positions = smurf_positions, 
                                    villainParameters = gargamelsParameters, 
                                    smartPursue = smartPursue,
                                    coolDownSteps = coolDownSteps,
                                    maxPursueSteps = maxPursueSteps,
                                    terrainThreshold = terrainThreshold,
                                    landscape = landscape, 
                                    max_villain_step = max_gargamel_step, 
                                    time_step = tstep)
    
    # Update Gargamel's position and state
    gargamel <- gargamel_result$villain
    gargamelState <- gargamel_result$villainState
    
    # Azrael movement
    azrael_result <- move_villain(villain = azrael, 
                                  smurf_positions = smurf_positions, 
                                  villainState = azraelState,
                                  villainParameters = azraelsParameters, 
                                  landscape = landscape, 
                                  gargamel = gargamel,
                                  coolDownSteps = coolDownSteps,
                                  maxPursueSteps = maxPursueSteps,
                                  terrainThreshold = terrainThreshold,
                                  smartPursue = smartPursue,
                                  max_villain_step = max_azrael_step, 
                                  time_step = tstep)
    
    # Update Azrael's position and state
    azrael <- azrael_result$villain
    azraelState <- azrael_result$villainState
    
    # Save positions
    smurfData <- smurf_positions[, c("id", "x", "y")]
    names(smurfData)[names(smurfData) == "id"] <- "Entity_Number"
    smurfData[, Time := tstep]
    smurfData[, Entity := "Smurf"]
    
    gargamelData <- data.table(Entity_Number = 1,
                               x = gargamel[1],
                               y = gargamel[2],
                               Time = tstep,
                               Entity = "Gargamel")
    
    azraelData <- data.table(Entity_Number = 1,
                             x = azrael[1],
                             y = azrael[2],
                             Time = tstep,
                             Entity = "Azrael")
    
    finalDataset <- rbindlist(list(finalDataset, smurfData, gargamelData, azraelData), 
                              use.names = TRUE)
    
    # Plot terrain and positions
    mycolors <- colorRampPalette(brewer.pal(9, "Greens"))(50)
    terra::plot(landscape, col = rev(mycolors), main = paste("Step:", tstep), legend = TRUE)
    points(village[1], village[2], col = "yellow", pch = 15, cex = 2)  # Village
    points(gargamel[1], gargamel[2], col = "red", pch = 17, cex = 2)  # Gargamel
    symbols(x = gargamel[1], y = gargamel[2], circles = threat_distance, 
            inches = FALSE, add = TRUE, fg = "red")
    points(azrael[1], azrael[2], col = "orange", pch = 18, cex = 2)  # Azrael
    symbols(azrael[1], azrael[2], circles = threat_distance, 
            inches = FALSE, add = TRUE, fg = "orange")
    points(smurf_positions$x, smurf_positions$y, col = ifelse(smurf_positions$dangerGG, "darkred", 
                                                              ifelse(smurf_positions$dangerAZ, "gold4", 
                                                                     "lightblue")), pch = 16, cex = 1.2)  # Smurfs
    # symbols(smurf_positions$x, smurf_positions$y, circles = rep(threat_distance, length(smurf_positions$x)),
    #         inches = FALSE, add = TRUE, fg = "lightblue")
    
    if (tstep == steps){
      # Last time step
      finalDataset[, Cell := extract(landscape, cbind(x, y), cells=TRUE, method="simple")[1]]
      finalDataset[, Altitude := extract(landscape, cbind(x, y), cells=FALSE, method="simple")]
      
      # Create a raster with some relationship between slope (raster landscape) and 4 types of fictitious trees 
      # resembling temperate forest, with each species preferring -- but not exclusive to -- different slopes
      # Lowland Oak (Type 1) – Prefers flat areas (low altitudes).
      # Mountain Pine (Type 2) – Prefers steep areas (high altitudes).
      # Mixed Beech (Type 3) – Prefers moderate altitudes
      # Spruce (Type 4) – Prefers a mix of moderate to high altitudes
      # Define slope ranges for species preferences
      tree_classes <- c("Lowland Oak", "Mountain Pine", "Mixed Beech", "Spruce")
      
      # Extract slope values from the landscape
      slope_values <- values(landscape)
      
      # Assign tree species based on slope probability
      tree_raster <- landscape  # Copy original raster structure
      tree_values <- rep(NA, length(slope_values))
      
      # Define probabilistic assignment based on slope
      tree_values[slope_values <= quantile(slope_values, 0.25, na.rm = TRUE)] <- 1  # Lowland Oak
      tree_values[slope_values > quantile(slope_values, 0.25, na.rm = TRUE) & 
                    slope_values <= quantile(slope_values, 0.50, na.rm = TRUE)] <- 3  # Mixed Beech
      tree_values[slope_values > quantile(slope_values, 0.50, na.rm = TRUE) & 
                    slope_values <= quantile(slope_values, 0.75, na.rm = TRUE)] <- 4  # Spruce
      tree_values[slope_values > quantile(slope_values, 0.75, na.rm = TRUE)] <- 2  # Mountain Pine
      
      # Introduce some randomness: Each pixel has a small chance of being a different species
      random_noise <- runif(length(tree_values))  # Generate random numbers [0,1]
      noise_threshold <- 0.20  # 10% chance of random species assignment
      
      # Apply noise: If random number < threshold, assign a random tree species
      random_indices <- which(random_noise < noise_threshold)
      tree_values[random_indices] <- sample(1:4, length(random_indices), replace = TRUE)
      
      # Assign values to the new raster
      values(tree_raster) <- tree_values
      
      # Assign categorical labels
      tree_raster <- classify(tree_raster, rcl = cbind(1:4, 1:4), right = FALSE)
      names(tree_raster) <- "Tree Species"
      
      # Define color palette
      tree_colors <- c("darkgreen", "saddlebrown", "chartreuse4", "darkolivegreen3")
      tree_legend <- c("Lowland Oak", "Mountain Pine", "Mixed Beech", "Spruce")
      
      # Plot the new raster
      # plot(tree_raster, col = tree_colors, legend = FALSE, main = "Tree Species Distribution")
      # legend("topright", legend = tree_legend, fill = tree_colors, border = "black")
      
      # Create a raster of completely unrelated feature -- say 3 types of fictitious herbs
      # Starleaf (Type 1) 🌿 – Appears randomly across the landscape.
      # Redbloom (Type 2) 🌺 – Tends to form patches.
      # Moonvine (Type 3) 🌙 – Prefers isolated spots.
      # Create an empty raster with the same dimensions as the landscape
      herb_raster <- rast(landscape)
      
      # Generate a random distribution of herbs using uniform noise
      herb_values <- runif(ncell(herb_raster))  # Random values [0,1]
      
      # Convert the numeric vector to a raster
      herb_raster <- setValues(herb_raster, herb_values)
      
      # Apply a weaker Gaussian smoothing to retain more randomness
      smoothed_herb_values <- focal(herb_raster, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
      
      # Normalize the values to ensure they stay between 0 and 1
      smoothed_herb_values <- (smoothed_herb_values - min(values(smoothed_herb_values), na.rm = TRUE)) / 
        (max(values(smoothed_herb_values), na.rm = TRUE) - min(values(smoothed_herb_values), na.rm = TRUE))
      
      # Introduce controlled randomness: Randomly shuffle 30% of the values
      shuffle_indices <- sample(1:ncell(herb_raster), size = round(0.30 * ncell(herb_raster)), replace = FALSE)
      smoothed_herb_values[shuffle_indices] <- runif(length(shuffle_indices))  # Random noise added
      
      # Classify into three herb species based on thresholds
      herb_classified <- smoothed_herb_values  # Copy raster structure
      
      herb_classified[smoothed_herb_values <= 0.33] <- 1  # Starleaf
      herb_classified[smoothed_herb_values > 0.33 & smoothed_herb_values <= 0.66] <- 2  # Redbloom
      herb_classified[smoothed_herb_values > 0.66] <- 3  # Moonvine
      
      # Assign values to the herb raster
      values(herb_raster) <- values(herb_classified)
      
      # Define color palette for visualization
      herb_colors <- c("springgreen3", "firebrick2", "purple4")
      herb_legend <- c("Starleaf", "Redbloom", "Moonvine")
      
      # Plot the new herb raster
      # plot(herb_raster, col = herb_colors, legend = FALSE, main = "Balanced Herb Species Distribution")
      # legend("topright", legend = herb_legend, fill = herb_colors, border = "black")
      
      # Extract the information for these based on the column "Cell", which has 
      # the cell number, in a data.table named "finalDataset".
      finalDataset[, TreeSpecies := terra::extract(tree_raster, Cell)]
      finalDataset[, HerbSpecies := terra::extract(herb_raster, Cell)]
      
      return(finalDataset)
    }
  }
}

# Run the simulation
DT <- simulate(
  steps = totalSteps,
  smurf_positions = smurf_positions,
  village = village,
  gargamel = gargamel,
  azrael = azrael,
  maxPursueSteps = maxPursueSteps,
  terrainThreshold = terrainThreshold,
  smurfsParameters = smurfsParameters,
  coolDownSteps = coolDownSteps,
  gargamelsParameters = gargamelsParameters,
  azraelsParameters = azraelsParameters,
  smartPursue = smartPursue,
  landscape = landscape,
  max_smurf_step = max_smurf_step,
  max_azrael_step = max_azrael_step,
  max_gargamel_step = max_gargamel_step
)


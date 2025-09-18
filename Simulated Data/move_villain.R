move_villain <- function(villain,
                         villainState,
                         smurf_positions, 
                         villainParameters, 
                         landscape, 
                         max_villain_step, 
                         time_step, 
                         smartPursue,
                         allow_capture) {
  
  # Filter Smurfs within 150 distance
  if (allow_capture){
    nearby_smurfs <- smurf_positions[distance(x, y, villain[1], villain[2]) <= all((threat_distance + 200), !captured)] # TODO Add modifier for villain's distance as parameter
  } else {
    nearby_smurfs <- smurf_positions[distance(x, y, villain[1], villain[2]) <= (threat_distance + 200)]
  }
  nearby_smurfs[, distanceToThreat := distance(x, y, villain[1], villain[2])]
  
  # Cooldown logic: Ignore Smurfs if in cooldown mode
  if (villainState$cooldown_steps > 0) {
    villainState$cooldown_steps <- villainState$cooldown_steps - 1
    nearby_smurfs <- data.table()  # Ensure villain doesn’t pursue any Smurf
  }
  
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
    if ("gargamelAttaction" %in% names(villainParameters)){
      # This is then Azrael
      dx <- (villainParameters[["smurfAttaction"]] * (target_smurf$x - villain[1])) / target_smurf$distanceToThreat +
        (villainParameters[["gargamelAttaction"]] * (gargamel[1] - villain[1])) / distance(villain[1], villain[2], gargamel[1], gargamel[2])
      
      dy <- (villainParameters[["smurfAttaction"]] * (target_smurf$y - villain[2])) / target_smurf$distanceToThreat +
        (villainParameters[["gargamelAttaction"]] * (gargamel[2] - villain[2])) / distance(villain[1], villain[2], gargamel[1], gargamel[2])
    } else {
      # This is then Gargamel 
      dx <- (villainParameters[["smurfAttaction"]] * (target_smurf$x - villain[1])) / target_smurf$distanceToThreat
      dy <- (villainParameters[["smurfAttaction"]] * (target_smurf$y - villain[2])) / target_smurf$distanceToThreat
    }
    
    # Adjust movement based on terrain
    dx <- dx / (1 + terrain_factor) # TODO Rethink the terrain difficulty factor
    dy <- dy / (1 + terrain_factor)
    
    # Limit movement to max_villain_step
    movement_distance <- sqrt(dx^2 + dy^2)
    if (movement_distance > max_villain_step) {
      dx <- dx * (max_villain_step / movement_distance)
      dy <- dy * (max_villain_step / movement_distance)
    }
    
    # Update villain's position
    villain[1] <- villain[1] + dx
    villain[2] <- villain[2] + dy
    
    # Update pursuit state
    villainState$pursuit_steps <- villainState$pursuit_steps + 1
    
    # Check if villain gives up after 5 steps
    # TODO Make the maximum pursue steps an argument of the function 
    if (villainState$pursuit_steps >= 5) {
      villainState$pursued_smurf <- NA  # Stop targeting the Smurf
      villainState$pursuit_steps <- 0   # Reset pursuit steps
      villainState$cooldown_steps <- 2  # Enter cooldown for 2 steps
    }
  } else {
    # No Smurfs in range: Search for mushrooms (lowest elevation)
    sExMin <- pmax(villain[1] - max_villain_step, 0)
    sExMax <- pmin(villain[1] + max_villain_step, xmax(landscape))
    sEyMin <- pmax(villain[2] - max_villain_step, 0)
    sEyMax <- pmin(villain[2] + max_villain_step, ymax(landscape))
    
    # Extract nearby cells and elevations
    nearby_cells <- cells(landscape, terra::ext(c(sExMin, sExMax, sEyMin, sEyMax)))
    nearby_coords <- raster::xyFromCell(landscape, nearby_cells)
    nearby_elevations <- terra::extract(landscape, nearby_coords)
    
    # Find the lowest elevation and its coordinates
    lowest_index <- which.min(nearby_elevations$slope)
    lowest_x <- nearby_coords[lowest_index, 1]
    lowest_y <- nearby_coords[lowest_index, 2]
    
    # Compute movement toward the lowest elevation
    dx <- villainParameters[1] * (lowest_x - villain[1])
    dy <- villainParameters[1] * (lowest_y - villain[2])
    
    # Limit movement to max_villain_step
    movement_distance <- sqrt(dx^2 + dy^2)
    if (movement_distance > max_villain_step) {
      dx <- dx * (max_villain_step / movement_distance)
      dy <- dy * (max_villain_step / movement_distance)
    }
    
    # Update villain's position
    villain[1] <- villain[1] + dx
    villain[2] <- villain[2] + dy
  }
  
  return(villain)
}
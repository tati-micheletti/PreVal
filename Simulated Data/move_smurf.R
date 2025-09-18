# DOUBLE CHECK

move_smurf <- function(smurf_positions, 
                       village, 
                       gargamel, 
                       azrael, 
                       smurfsParameters, 
                       landscape, 
                       max_smurf_step, 
                       allow_capture) {
  
  # Danger zones
  smurf_positions[, dangerGG := distance(x, y, gargamel[1], gargamel[2]) < threat_distance]
  smurf_positions[, dangerAZ := distance(x, y, azrael[1], azrael[2]) < threat_distance]
  
  # Handle capture
  if (allow_capture) {
    smurf_positions[, captured := captured | (distance(x, y, gargamel[1], gargamel[2]) < capture_distance) |
                      (distance(x, y, azrael[1], azrael[2]) < capture_distance)]
  }
  
  # Movement logic
  smurf_positions[, `:=`(dx = 0, dy = 0)]
  
  smurf_positions[, `:=`(
    dx = fifelse(
      dangerGG | dangerAZ,
      # Escape danger
      -smurfsParameters["GargamelRepulsion"] * (x - fifelse(dangerGG, gargamel[1], azrael[1])) / distance(x, y, fifelse(dangerGG, gargamel[1], azrael[1]), fifelse(dangerGG, gargamel[2], azrael[2])),
      # Move to mushrooms and village
      smurfsParameters["mushroomAttraction"] * -gradient(x, y, landscape, "x") +
        smurfsParameters["villageAttaction"] * (village[1] - x)
    ),
    dy = fifelse(
      dangerGG | dangerAZ,
      -smurfsParameters["GargamelRepulsion"] * (y - fifelse(dangerGG, gargamel[2], azrael[2])) / distance(x, y, fifelse(dangerGG, gargamel[1], azrael[1]), fifelse(dangerGG, gargamel[2], azrael[2])),
      smurfsParameters["mushroomAttraction"] * -gradient(x, y, landscape, "y") +
        smurfsParameters["villageAttaction"] * (village[2] - y)
    )
  )]
  
  # Limit movement
  smurf_positions[, move_distance := sqrt(dx^2 + dy^2)]
  smurf_positions[, `:=`(
    dx = fifelse(move_distance > max_smurf_step, dx * (max_smurf_step / move_distance), dx),
    dy = fifelse(move_distance > max_smurf_step, dy * (max_smurf_step / move_distance), dy)
  )]
  
  # Update positions
  smurf_positions[, `:=`(
    x = pmin(pmax(x + dx, 0), xmax(landscape)),
    y = pmin(pmax(y + dy, 0), ymax(landscape))
  )]
  
  return(smurf_positions)
}

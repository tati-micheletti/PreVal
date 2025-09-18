# move_gargamel <- function(gargamel, 
#                           smurf_positions, 
#                           gargamelsParameters, 
#                           sigma, 
#                           landscape, 
#                           max_gargamel_step, 
#                           time_step, 
#                           allow_capture) {
#   
#   browser()
#   # Find Smurfs and groups
#   distances <- smurf_positions[, distance(x, y, gargamel[1], gargamel[2])]
#   closest_smurf_idx <- which.min(distances)
#   closest_smurf <- smurf_positions[closest_smurf_idx]
#   
#   # Check if the closest Smurf is in a group
#   group_members <- smurf_positions[distance(x, y, closest_smurf$x, closest_smurf$y) < group_distance]
#   in_group <- nrow(group_members) >= 3  # Group exists if 3 or more Smurfs are close
#   
#   # Release captured Smurf if faced with a group and allow_capture is TRUE
#   if (allow_capture && in_group && time_step %% 2 == 0) {
#     smurf_positions[, captured := FALSE]
#   }
#   
#   terrain_factor <- get_terrain_value(gargamel[1], gargamel[2], landscape)
#   
#   # Compute movement
#   if (allow_capture && in_group) {
#     # Repelled by groups
#     dx <- -gargamelsParameters[1] * (closest_smurf$x - gargamel[1]) / distances[closest_smurf_idx] +
#       rnorm(1, mean = 0, sd = sigma) * terrain_factor
#     dy <- -gargamelsParameters[1] * (closest_smurf$y - gargamel[2]) / distances[closest_smurf_idx] +
#       rnorm(1, mean = 0, sd = sigma) * terrain_factor
#   } else {
#     # Pursue closest Smurf
#     dx <- gargamelsParameters[1] * (closest_smurf$x - gargamel[1]) / distances[closest_smurf_idx] +
#       rnorm(1, mean = 0, sd = sigma) * terrain_factor
#     dy <- gargamelsParameters[1] * (closest_smurf$y - gargamel[2]) / distances[closest_smurf_idx] +
#       rnorm(1, mean = 0, sd = sigma) * terrain_factor
#   }
#   
#   # Capture logic if allow_capture is TRUE
#   if (allow_capture && distances[closest_smurf_idx] < capture_distance) {
#     smurf_positions[id == closest_smurf$id, captured := TRUE]
#   }
#   
#   # Limit movement distance
#   move_distance <- sqrt(dx^2 + dy^2)
#   if (move_distance > max_gargamel_step) {
#     dx <- dx * (max_gargamel_step / move_distance)
#     dy <- dy * (max_gargamel_step / move_distance)
#   }
#   
#   # Update Gargamel's position
#   gargamel <- c(
#     pmin(pmax(gargamel[1] + dx, 0), landscape_size),
#     pmin(pmax(gargamel[2] + dy, 0), landscape_size)
#   )
#   
#   return(gargamel)
# }

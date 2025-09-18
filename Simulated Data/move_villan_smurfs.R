x <- 2167.625 
y <- 2365.2394
group_cohesion_x <- 2552.665
group_cohesion_y <- 2222.484

whereDangerX <- smurfsParameters["villageAttaction"] * (village[1] - x) -
  smurfsParameters["GargamelRepulsion" ] * (x - gargamel[1])

whereNoDangerX <- smurfsParameters["villageAttaction"] * (village[1] - x) +
  smurfsParameters["groupCohesion"] * (group_cohesion_x - x) +
  smurfsParameters["mushroomAttraction"] * - gradient(x, y, landscape, "x")


whereDangerY <- smurfsParameters["villageAttaction"] * (village[2] - y) / distance(x, y, village[1], village[2]) -
  smurfsParameters["GargamelRepulsion" ] * (y - gargamel[2]) / distance(x, y, gargamel[1], gargamel[2])
# If not in danger by GG, go in the direction of the village, group and get mushrooms
whereNoDangerY <- smurfsParameters["villageAttaction"] * (village[2] - y) / distance(x, y, village[1], village[2]) +
  smurfsParameters["groupCohesion"] * (group_cohesion_y - y) +
  smurfsParameters["mushroomAttraction"] * - gradient(x, y, landscape, "y")

points(x, y, col = "white", pch = 16, cex = 1.2)  # original Place
points(gargamel[1], gargamel[2], col = "red", pch = 17, cex = 2)  # Gargamel
points(whereDangerX, whereDangerY, col = "orange", pch = 16, cex = 1.2)  # in Danger
points(whereNoDangerX, whereNoDangerY, col = "green", pch = 16, cex = 1.2)  # no Danger



smurf_positions[, `:=`(
  dx = fifelse(dangerGG,
               # If in danger by GG, go in the direction of the village, avoiding GG and AZ  
               smurfsParameters["villageAttaction"] * (village[1] - x) / distance(x, y, village[1], village[2]) -
                 smurfsParameters["GargamelRepulsion" ] * (x - gargamel[1]) / distance(x, y, gargamel[1], gargamel[2]),
               # NO DANGER: If not in danger by GG, go in the direction of the village, group and get mushrooms
               {
                 # Extract nearby cells and elevations
                 nearby_cells <- cellsFromExtent(landscape, ext(
                   x - max_smurf_step, x + max_smurf_step,
                   y - max_smurf_step, y + max_smurf_step
                 ))
                 nearby_coords <- xyFromCell(landscape, nearby_cells)
                 nearby_elevations <- extract(landscape, nearby_coords)
                 
                 # Find the lowest elevation and its coordinates
                 lowest_index <- which.min(nearby_elevations)
                 lowest_x <- nearby_coords[lowest_index, 1]
                 
                 # Combine influences: Lowest elevation, group cohesion, and village
                 smurfsParameters["mushroomAttraction"] * (lowest_x - x) +
                   smurfsParameters["groupCohesion"] * (group_cohesion_x - x) +
                   smurfsParameters["villageAttaction"] * (village[1] - x)
               }
,
  
  dy = fifelse(dangerGG,
               # If in danger by GG, go in the direction of the village, avoiding GG and AZ  
               smurfsParameters["villageAttaction"] * (village[2] - y) / distance(x, y, village[1], village[2]) -
                 smurfsParameters["GargamelRepulsion" ] * (y - gargamel[2]) / distance(x, y, gargamel[1], gargamel[2]),
               # NO DANGER: If not in danger by GG, go in the direction of the village, group and get mushrooms
               # INCLUDE THE EQUATIONS HERE FOR NO DANGER,
               )
)]



c(smurf_positions$sExMin, smurf_positions$sExMax, smurf_positions$sEyMin, smurf_positions$sEyMax)


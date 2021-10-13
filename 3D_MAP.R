# Fast DEM mapping

## --------------------------------------------------
source("DEPENDENCIES/Functions.R")
## --------------------------------------------------
library(elevatr)

cuba_boundary <- geoboundaries("andorra")

cuba_DEM <- get_elev_raster(locations = cuba_boundary, z = 9, clip = "locations") 


cuba_DEM_2 <- as.data.frame(cuba_DEM, xy = TRUE)
colnames(cuba_DEM_2)[3] = "elevation"
#remove rows of data frame with one or more NA's,using complete.cases

cuba_DEM_2 <- cuba_DEM_2[complete.cases(cuba_DEM_2),] 

ggplot() +
  geom_raster(data = cuba_DEM_2, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = cuba_boundary, color = "red", fill = NA) +
  coord_sf()


cuba_DEM_3D <-  raster_to_matrix(cuba_DEM)

cuba_DEM_3D %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(cuba_DEM_3D, zscale = 1), 0.5) %>%
  add_shadow(ambient_shade(cuba_DEM_3D), 0) %>%
  plot_3d(cuba_DEM_3D, zscale = 5, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))

Sys.sleep(0.2)

render_snapshot()

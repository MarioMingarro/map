
## --------------------------------------------------
source("DEPENDENCIES/Functions.R")
## --------------------------------------------------

caceres_extend <- getbb("caceres")

caceres_roads <- caceres_extend %>%
  opq() %>%
  add_osm_feature("highway", c("motorway", "primary")) %>%
  osmdata_sf()

ggplot()+
  geom_sf(data = caceres_roads$osm_polygons)
library(ggmap)
rm(caceres_map)
caceres_map <- get_map(caceres_extend, maptype = "satellite")

ggmap(caceres_map) +
  geom_sf(data = caceres_roads$osm_lines,
          inherit.aes = FALSE,
          colour = "red",
          fill = "black",
          alpha = .5,
          size = 1)


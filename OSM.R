

## --------------------------------------------------
source("DEPENDENCIES/Functions.R")
## --------------------------------------------------

# Extends
## --------------------------------------------------
palma_extend <- getbb("la palma")
spain_boundary <- geoboundaries("Spain")

# OSM data
## --------------------------------------------------
# https://wiki.openstreetmap.org/wiki/Map_features


palma_roads <- palma_extend %>%
  opq() %>%
  add_osm_feature("highway", c("motorway", "primary", "secondary")) %>% # https://wiki.openstreetmap.org/wiki/Map_features
  osmdata_sf()

palma_accomodation <- palma_extend %>%
  opq() %>%
  add_osm_feature("building", "hotel") %>%
  osmdata_sf()

palma_map <- get_map(palma_extend)


# Plotting with GGMAP
## --------------------------------------------------

ggmap(palma_map) +
  geom_sf(data = palma_roads$osm_lines,
          inherit.aes = FALSE,
          colour = "red",
          alpha = 0.5,
          size = 1)+
  geom_sf(data = palma_accomodation$osm_points,
          inherit.aes = FALSE,
          colour = "black",
          size = 2)


# Plotting with GGPLOt
## --------------------------------------------------



ggplot() + 
  geom_sf(data= spain_boundary,fill= "#ABABAB", colour = "#9B9B9B" , size=0.1 ) +
  geom_sf(data = palma_roads$osm_lines,
          colour = "red",
          size = 1)+
  geom_sf(data = palma_accomodation$osm_points,
             colour = "black",
             fill = "black",
             alpha = 0.5,
             size = 2) +
  coord_sf(xlim = c(palma_extend[1,1], palma_extend[1,2]), ylim = c(palma_extend[2,1],palma_extend[2,2]), expand = FALSE)+
  theme_void()

  

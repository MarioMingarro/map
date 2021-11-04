# Fast OSM mapping ----
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("DEPENDENCIES/Functions.R")
closeAllConnections()

# Extents ----
palma_extend <- getbb("la palma")
spain_boundary <- geoboundaries("Spain")

# OSM data ----

# https://wiki.openstreetmap.org/wiki/Map_features
palma_roads <- palma_extend %>%
  opq() %>%
  add_osm_feature("highway", c("motorway", "primary", "secondary")) %>% #
  osmdata_sf()

palma_accomodation <- palma_extend %>%
  opq() %>%
  add_osm_feature("building", "hotel") %>%
  osmdata_sf()




# Plotting with GGMAP ----
palma_map <- get_map(palma_extend)

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


# Plotting with GGPLOt ----
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

#-------------------------
library(leaflet)

map <- leaflet()

getBox <- function(m){
  view <- m$x$setView
  lat <- view[[1]][1]
  lng <- view[[1]][2]
  zoom <- view[[2]]
  zoom_width <- 360 / 2^zoom
  lng_width <- m$width / 256 * zoom_width
  lat_height <- m$height / 256 * zoom_width
  return(c(lng - lng_width/2, lng + lng_width/2, lat - lat_height/2, lat + lat_height/2))
}
getBox(m)


library(magrittr)
library(leaflet)

#width = 500,height = 400

map <- leaflet(width = 500,height = 400) %>% 
  addTiles() %>% 
  setView(lng = -3.7, lat = 40.4, zoom = 18) %>%
  addCircleMarkers(lng = -3.7, lat = 	40.4)

map

getBox <- function(map){
  view <- map$x$setView
  lat <- view[[1]][1]
  lng <- view[[1]][2]
  zoom <- view[[2]]
  zoom_width <- 360 / 2 ^ zoom
  lng_width <- map$width / 256 * zoom_width
  lat_height <- map$height / 256 * zoom_width
  return(c(lng - lng_width/2, lng + lng_width/2, lat - lat_height/2, lat + lat_height/2))
}
library(leaflet.extras)


library(leaflet.extras)

# Define UI 
ui <- fluidPage(
  leafletOutput("mymap",height=800)
)

# Define server logic 
server <- function(input, output) {
  
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
      setView(lng = -166, lat = 58.0, zoom = 5) %>%
      addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE))
  )
  
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    
    print(feature)
    
  })
  
}


#------------------
shinyApp(ui = ui, server = server)

library(shiny)
library(sp)
require(sp) 
require(leaflet) 
require(leaflet.extras)


ui <- fluidPage(
  
  textOutput("text"),leafletOutput("mymap"),
  downloadButton('downloadData', 'Download Shp'))

server<- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    leaflet("mymap") %>%
      addProviderTiles(providers$Stamen.TonerLite, #map type or map theme. -default($Stame.TonerLite)
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      addDrawToolbar(targetGroup = "drawnPoly", 
                     rectangleOptions = F, 
                     polylineOptions = F, 
                     markerOptions = F, 
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                     circleOptions=F,
                     polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE))) %>%
      
      addStyleEditor()
    
  })
  
  
  
  latlongs<-reactiveValues()   #temporary to hold coords
  latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
  
  #########
  #empty reactive spdf
  value<-reactiveValues()
  SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame (notes=character(0), stringsAsFactors = F))->value$drawnPoly
  
  #fix the polygon to start another
  
  observeEvent(input$mymap_draw_new_feature, {
    
    coor<-unlist(input$mymap_draw_new_feature$geometry$coordinates)
    
    Longitude<-coor[seq(1,length(coor), 2)] 
    
    Latitude<-coor[seq(2,length(coor), 2)]
    
    isolate(latlongs$df2<-rbind(latlongs$df2, cbind(Longitude, Latitude)))
    
    poly<-Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
    polys<-Polygons(list(poly),    ID=input$mymap_draw_new_feature$properties$`_leaflet_id`)
    spPolys<-SpatialPolygons(list(polys))
    
    
    #
    value$drawnPoly<-rbind(value$drawnPoly,SpatialPolygonsDataFrame(spPolys, 
                                                                    data=data.frame(notes=NA, row.names=
                                                                                      row.names(spPolys))))
    
    ###plot upon ending draw
    observeEvent(input$mymap_draw_stop, {
      
      #replot it - take off the DrawToolbar to clear the features and add it back and use the values from the SPDF to plot the polygons
      leafletProxy('mymap') %>%  removeDrawToolbar(clearFeatures=TRUE) %>% removeShape('temp') %>% clearGroup('drawnPoly') %>% addPolygons(data=value$drawnPoly, popup="poly",   group='drawnPoly', color="blue", layerId=row.names(value$drawnPoly)) %>% 
        
        addDrawToolbar(targetGroup = "drawnPoly", 
                       rectangleOptions = F, 
                       polylineOptions = F, 
                       markerOptions = F, 
                       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
                       circleOptions=F,
                       polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE)))
      
    })
    
    latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))   #clear df
    
  })
  
  ########################
  ### edit polygons / delete polygons
  
  observeEvent(input$mymap_draw_edited_features, {
    
    f <- input$mymap_draw_edited_features
    
    coordy<-lapply(f$features, function(x){unlist(x$geometry$coordinates)})
    
    Longitudes<-lapply(coordy, function(coor) {coor[seq(1,length(coor), 2)] })
    
    Latitudes<-lapply(coordy, function(coor) { coor[seq(2,length(coor), 2)] })
    
    polys<-list()
    for (i in 1:length(Longitudes)){polys[[i]]<- Polygons(
      list(Polygon(cbind(Longitudes[[i]], Latitudes[[i]]))), ID=f$features[[i]]$properties$layerId
    )}
    
    spPolys<-SpatialPolygons(polys)
    
    
    SPDF<-SpatialPolygonsDataFrame(spPolys, 
                                   data=data.frame(notes=value$drawnPoly$notes[row.names(value$drawnPoly) %in% row.names(spPolys)], row.names=row.names(spPolys)))
    
    value$drawnPoly<-value$drawnPoly[!row.names(value$drawnPoly) %in% row.names(SPDF),]
    value$drawnPoly<-rbind(value$drawnPoly, SPDF)
    
  })
  
  observeEvent(input$mymap_draw_deleted_features, { 
    
    f <- input$mymap_draw_deleted_features
    
    ids<-lapply(f$features, function(x){unlist(x$properties$layerId)})
    
    
    value$drawnPoly<-value$drawnPoly[!row.names(value$drawnPoly) %in% ids ,]
    
  }) 
  
  
  
  #write the polys to .shp
  output$downloadData <- downloadHandler(
    
    filename = 'shpExport.zip',
    content = function(file) {
      if (length(Sys.glob("shpExport.*"))>0){
        file.remove(Sys.glob("shpExport.*"))
      }
      
      proj4string(value$drawnPoly)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      writeOGR(value$drawnPoly, dsn="shpExport.shp", layer="shpExport", driver="ESRI Shapefile")
      zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"))
      file.copy("shpExport.zip", file)
      if (length(Sys.glob("shpExport.*"))>0){
        file.remove(Sys.glob("shpExport.*"))
      }
    }
  )
  
}



shinyApp(ui=ui,server=server)

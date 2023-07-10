library(sf)
library(leaflet)
library(tidyverse)

fda_all <- st_read("www/fda10.gpkg", 'fda', quiet=T)
bnd <- st_read('www/fda10.gpkg', 'bnd', quiet=T)
line_all <- st_read("www/fda10.gpkg", 'sd_line', quiet=T)
poly_all <- st_read("www/fda10.gpkg", 'sd_poly', quiet=T)
ifl2000 <- st_read("www/fda10.gpkg", 'ifl_2000', quiet=T)
ifl2020 <- st_read("www/fda10.gpkg", 'ifl_2020', quiet=T)
fda <- st_read("www/fda10.gpkg", 'fda', quiet=T)
line <- st_read("www/fda10.gpkg", 'sd_line', quiet=T)
poly <- st_read("www/fda10.gpkg", 'sd_poly', quiet=T)

bnd <- st_transform(bnd, 4326)
fdas <- st_transform(fda_all, 4326)
line <- st_transform(line_all, 4326)
poly <- st_transform(poly_all, 4326)
intact2000 <- st_transform(ifl2020, 4326)
intact2020 <- st_transform(ifl2000, 4326)
map_bounds <- bnd %>% st_bbox() %>% as.character()
m <- leaflet() %>%
    addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
    addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
    addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
    addPolygons(data=bnd, color='black', fill=F, weight=3, group="Data extent") %>%
    fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
    addPolygons(data=fdas, color='blue', fill=F, weight=2, group="FDAs") %>%
    addPolylines(data=line, color='black', weight=2, group="Linear disturbances") %>%
    addPolygons(data=poly, fill=T, stroke=F, fillColor='grey', fillOpacity=0.5, group="Areal disturbances") %>%
    addPolygons(data=intact2000, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intactness 2000") %>%
    addPolygons(data=intact2020, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intactness 2020") %>%
    addLayersControl(position="topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery"),
            overlayGroups = c("Data extent","FDAs","Areal disturbances","Linear disturbances", "Intactness 2000", "Intactness 2020"),
            options = layersControlOptions(collapsed=F)) %>%
    hideGroup(c("FDAs", "Intactness 2000", "Intactness 2020"))

if(input$select_fda >0) {
    if (input$select_fda %in% fda_list) {
      aoi_sf <- fda()
      #Display fda of interest and respectives disturbances
      aoi <- st_transform(aoi_sf, 4326)
      poly_clip <- st_transform(poly(), 4326)
      line_clip <- st_transform(line(), 4326)

      map_bounds1 <- aoi %>% st_bbox() %>% as.character()
      m <- m %>%
        fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
        addPolygons(data=aoi, fillColor='yellow', color="black", fillOpacity=0.5, weight=4, group="AOI") %>%
        addPolylines(data=line_clip, color='red', weight=2, group="AOI linear clipped") %>%
        addPolygons(data=poly_clip, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="AOI areal clipped") %>%
        addLayersControl(position = "topright",
                baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                overlayGroups = c("AOI", "AOI linear clipped", "AOI areal clipped", "FDAs", "Linear disturbances", "Areal disturbances",  "Intactness 2000", "Intactness 2020"),
                options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("FDAs","Linear disturbances","Areal disturbances", "Intactness 2000", "Intactness 2020"))
    }
}


output$tab1 <- renderTable({
  x <- tibble(Map=c("Area of interest (km2)", "IFL 2000 (%)", "IFL 2020 (%)", "Intactness (%)","Footprint (%)"), Area=NA)
  aoi <- sum(st_area(fda_all()))
  x$Area[x$Map=="Area of interest (km2)"] <- round(aoi/1000000, 0)
  x$Area[x$Map=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000()))/sum(st_area(fda_all()))*100,1)
  x$Area[x$Map=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020()))/sum(st_area(fda_all()))*100,1)

Tom, (587) 785-7983, (204) 781-3678, (204) 794-4678, (204) 799-3869, (204) 806-5030, (415) 990-7442, (701) 212-2539
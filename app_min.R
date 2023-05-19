library(sf)
library(dplyr)
library(terra)
library(raster)
library(leaflet)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shiny)
library(rgdal)
library(shinycssloaders)

ui = dashboardPage(skin="blue",
                   dashboardHeader(title = "Regional Disturbance"),
                   dashboardSidebar(
                     sidebarMenu(id = "tabs",
                                 menuItem("Footprint/intactness", tabName = "fri", icon = icon("th"))
                     ),
                     hr(),
                     
                     conditionalPanel(
                       condition = "input.tabs == 'fri' || input.tabs == 'land' || input.tabs == 'hydro' || input.tabs == 'upstream'",
                       selectInput("fda", label="Select FDA:", choices=c("10AB","09EA","FL"), selected="FL")
                     ),
                     conditionalPanel(
                       condition = "input.tabs == 'fri' || input.tabs == 'land' || input.tabs == 'hydro' || input.tabs == 'upstream'",
                       sliderInput("buffer1", label="Linear buffer size (m):", min=0, max=2000, value = 1000, step=100, ticks=FALSE),
                       sliderInput("buffer2", label="Areal buffer size (m):", min=0, max=2000, value = 1000, step=100, ticks=FALSE),
                       sliderInput("area1", label="Min size of intact areas (km2):", min=0, max=2000, value = 500, step=100, ticks=FALSE),
                       checkboxInput("nobound", label = 'Boundary is not a barrier', value = F),
                       actionButton("goButton", "Generate intactness map")
                     ),
                     conditionalPanel(
                       condition = "input.tabs == 'fri'",
                       hr(),
                       downloadButton("downloadFootprintMap","Download footprint/intactness")
                     ),
                     conditionalPanel(
                       condition = "input.tabs == 'upstream'",
                       hr(),
                       actionButton("goButtonUpstream", "View upstream disturbances")
                     ),
                     conditionalPanel(
                       condition = "input.tabs == 'upstream'",
                       hr(),
                       downloadButton("downloadCatchment","Download catchments")
                     )
                     
                   ),
                   dashboardBody(
                     useShinyjs(),
                     tags$head(tags$style(".skin-blue .sidebar a { color: #444; }")),
                     tabItems(
                       tabItem(tabName="overview",
                               fluidRow(
                                 tabBox(
                                   id = "zero", width="12",
                                   tabPanel(HTML("<b>Welcome!</b>"), htmlOutput("help")),
                                 )
                               )
                       ),
                       tabItem(tabName="fri",
                               fluidRow(
                                 tabBox(
                                   id = "one", width="8",
                                   tabPanel(HTML("<b>Map viewer</b>"), leafletOutput("map", height=750) %>% withSpinner()),
                                 ),
                                 tabBox(
                                   id = "two", width="4",
                                   tabPanel(HTML("<b>Intactness</b>"), tableOutput("tab1"))
                                 ),
                                 tabBox(
                                   id="three", width="4",
                                   tabPanel(HTML("<b>Linear disturbances</b>"), tableOutput("tab2")),
                                   tabPanel(HTML("<b>Areal disturbances</b>"), tableOutput("tab3"))
                                 ),
                               )
                       )
                     )
                   ))


server = function(input, output) {
  
  output$help <- renderText({
    includeMarkdown("docs/overview.md")
  })
  
  ####################################################################################################
  # READ SPATIAL DATA
  ####################################################################################################
  fda <- reactive({
    paste0('www/fda_',tolower(input$fda),'.gpkg')
  })

  fda_hydro <- reactive({
    paste0('www/fda_',tolower(input$fda),'_hydro.gpkg')
  })
  
  bnd <- reactive({
    st_read(fda(), 'FDA', quiet=T)
  })

  bnd10k <- reactive({
    x <- st_buffer(bnd(), 10000)
  })

  lakesrivers <- reactive({
    st_read(fda_hydro(), 'lakes_rivers', quiet=T) %>% st_union()
  })
  
  streams <- reactive({
    st_read(fda_hydro(), 'streams', quiet=T) %>% st_union()
  })
  
  fires <- reactive({
    st_read(fda(), 'Fire_History', quiet=T)
  })
  
  ifl2000 <- reactive({
    st_read(fda(), 'IFL_2000', quiet=T)
  })
  
  ifl2020 <- reactive({
    st_read(fda(), 'IFL_2020', quiet=T)
  })
  
  linear <- reactive({
    if (input$fda=='10AB') {
      st_read(fda(), 'Linear_Features+', quiet=T)
    } else {
      st_read(fda(), 'Linear_Features', quiet=T)
    }
  })
  
  quartz <- reactive({
    st_read(fda(), 'Quartz_Claims', quiet=T)
  })
  
  areal <- reactive({
    if (input$fda=='10AB') {
      st_read(fda(), 'Areal_Features+', quiet=T)
    } else {
      st_read(fda(), 'Areal_Features', quiet=T)
    }
  })
  
  catch <- reactive({
    catch <- paste0('www/fda_',tolower(input$fda),'_catch.gpkg')
  })
  catchments <- reactive({
    catchments <- st_read(catch(), 'catchments', quiet=T)
  })
  
  upstream_catch <- reactive({
    upstream_catch <- readRDS(file = paste0('www/upstream_catch_',tolower(input$fda),'.rds'))
  })    
  
  ####################################################################################################
  # BUFFER DISTURBANCES AND CALCULATE FOOTPRINT AND INTACTNESS
  ####################################################################################################
  
  # Footprint
  footprint_sf <- eventReactive(input$goButton, {
    
      v1 <- st_union(st_buffer(linear(), input$buffer1))
      v2 <- st_union(st_buffer(areal(), input$buffer2))
      v <- st_intersection(st_union(v1, v2), bnd())
      # The next lines are commented out because it is too slow
      #if (input$potential) {
      #  v <- st_union(v, quartz())
      #}
  })
  
  # Intactness
  intactness_sf <- eventReactive(input$goButton, {
    if (input$nobound) {
        ifl <- st_difference(bnd10k(), footprint_sf()) %>%
            st_cast('POLYGON')
        x <- mutate(ifl, area_km2=as.numeric(st_area(ifl)/1000000))
        y <- filter(x, area_km2 > input$area1)
        x <- st_intersection(y, bnd())
        y <- st_collection_extract(x, 'POLYGON')
    } else {
        ifl <- st_difference(bnd(), footprint_sf())
        x <- st_cast(ifl, "POLYGON")
        x <- mutate(x, area_km2=as.numeric(st_area(x)/1000000))
        y <- filter(x, area_km2 > input$area1)
    }
  })
  
  
  ####################################################################################################
  # FOOTPRINT/INTACTNESS SECTION
  ####################################################################################################
  
  # Map viewer
  # Render the initial map
  output$map <- renderLeaflet({
    
    # Re-project
    bnd <- st_transform(bnd(), 4326)
    ifl2000 <- st_transform(ifl2000(), 4326)
    ifl2020 <- st_transform(ifl2020(), 4326)
    fires <- st_transform(fires(), 4326)
    quartz <- st_transform(quartz(), 4326)
    areal <- st_transform(areal(), 4326)
    linear <- st_transform(linear(), 4326)
    
    #pal <- colorBin("YlOrRd", domain = fires$FIRE_YEAR, bins = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020, Inf))
    #labels <- sprintf("Fire year: %s<br/>Fire cause: %s", fires$FIRE_YEAR, fires$GENERAL_FIRE_CAUSE) %>% lapply(htmltools::HTML)
    labels <- sprintf("Fire year: %s", fires$FIRE_YEAR) %>% lapply(htmltools::HTML)
    
    map_bounds <- bnd %>% st_bbox() %>% as.character()
    
    m <- leaflet() %>% 
      
      addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      
      addPolygons(data=bnd, color='black', fill=F, weight=2, group="FDA") %>%
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
      #addPolygons(data=fires, fillColor = ~pal(FIRE_YEAR), color='grey', weight=1, group="Fires", opacity=1, fillOpacity = 0.7,
      addPolygons(data=fires, fillColor="red", color='grey', weight=1, group="Fires", opacity=1, fillOpacity=0.5,
                  highlightOptions = highlightOptions(weight=2, color="black", bringToFront=T),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
      #addLegend(pal = pal, values = ~fires$FIRE_YEAR, opacity = 0.7, title = NULL, position = "bottomright") %>%
      addPolygons(data=quartz, color='yellow', fill=F, weight=1, group="Quartz") %>%
      addPolylines(data=linear, color='red', weight=1, group="Linear features", popup = ~paste("Industry: ", TYPE_INDUSTRY, "<br>", "Disturbance: ", TYPE_DISTURBANCE)) %>%
      addPolygons(data=areal, color='black', fill=T, stroke=F, group="Areal features", popup = ~paste("Industry: ", TYPE_INDUSTRY, "<br>", "Disturbance: ", TYPE_DISTURBANCE), fillOpacity=0.5) %>%
      addPolygons(data=ifl2020, color='darkgreen', fillOpacity=0.5, group="IFL 2020") %>%
      addPolygons(data=ifl2000, color='darkgreen', fillOpacity=0.5, group="IFL 2000") %>%
      
      #pal <- colorBin("PuOr", fires$GENERAL_FIRE_CAUSE, bins = c(0, .1, .4, .9, 1))
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                       overlayGroups = c("IFL 2020","IFL 2000","Fires","Quartz","Areal features","Linear features"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("IFL 2020","IFL 2000","Fires","Quartz","Areal features","Linear features","Intactness"))
    #m <- m %>% addLegend(pal=pal, values=~fires$GENERAL_FIRE_CAUSE, position=c("bottomright"), title="Fire cause", opacity=0.8)
    
    # Add footprint if its already been made
    if(input$goButton > 0){
      v <- st_transform(intactness_sf(), 4326)
      vv <- st_transform(footprint_sf(), 4326)
      
      m <- m %>%
        addPolygons(data=v, color='blue', stroke=F, fillOpacity=0.5, group='Intactness') %>%
        addPolygons(data=vv, color='black', stroke=F, fillOpacity=0.5, group='Footprint') %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                         overlayGroups = c("IFL 2020","IFL 2000","Fires","Quartz","Areal features","Linear features","Footprint","Intactness"),
                         options = layersControlOptions(collapsed = FALSE))
    }
    m
  })
  
  # Intactness table
  output$tab1 <- renderTable({
    x <- tibble(Map=c("FDA (km2)","IFL 2000 (%)","IFL 2020 (%)","Intactness (%)","Footprint (%)"), Area=NA)
    x$Area[x$Map=="FDA (km2)"] <- round(st_area(bnd())/1000000,0)
    x$Area[x$Map=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000()))/st_area(bnd())*100,1)
    x$Area[x$Map=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020()))/st_area(bnd())*100,1)
    
    # If button has been pressed at least once, keep the intactness/footprint values updated
    if(input$goButton > 0) {
      x$Area[x$Map=="Intactness (%)"] <- round(sum(st_area(intactness_sf()))/st_area(bnd())*100,1)
      x$Area[x$Map=="Footprint (%)"] <- round(sum(st_area(footprint_sf()))/st_area(bnd())*100,1)
    }
    x
  })
  
  # Linear disturbances table
  output$tab2 <- renderTable({
    km <- group_by(linear(), TYPE_DISTURBANCE) %>%
      summarize(Length_km = sum(Length_km))
    x <- tibble(Disturbance_type=km$TYPE_DISTURBANCE, Length_km=km$Length_km, Length_pct=Length_km/sum(Length_km)*100)
  })
  
  # Areal disturbances table
  output$tab3 <- renderTable({
    ha <- group_by(areal(), TYPE_DISTURBANCE) %>%
      summarize(Area_ha = sum(Area_ha)/100)
    x <- tibble(Disturbance_type=ha$TYPE_DISTURBANCE, Area_ha=ha$Area_ha, Area_pct=Area_ha/sum(Area_ha)*100)
  })
    
  ####################################################################################################
  # DOWNLOAD SHAPEFILE
  ####################################################################################################
  
  output$downloadFootprintMap <- downloadHandler(
    filename = function() {'data_download.gpkg'},
    content = function(file) {
      st_write(footprint_sf(), dsn=file, layer='footprint')
      st_write(intactness_sf(), dsn=file, layer='intactness', append = TRUE)
      st_write(bnd(), dsn=file, layer='fda_boundary', append = TRUE)
    }
  )
  
  output$downloadCatchment <- downloadHandler(
    filename = function() {'catchments_upstream_stats.gpkg'},
    content = function(file) {
      catch_dl <- catch_out() %>%
        mutate(Area_Land = rount(Area_Land,1),
               Area_Water = round(Area_Water,1),
               Area_Total = round(Area_Total,1),
               Prop_Intact =round((Area_Total - area_dist)/Area_Total*100,1),
               Upstream_Area = round(uparea,1),
               Upstream_Prop_Intact = round(((uparea - upadist)/uparea)*100,1))
      catch_dl <- catch_dl[,c("CATCHNUM", "Area_Land", "Area_Water","Area_Total", "Prop_Intact",  "Upstream_Area", "Upstream_Prop_Intact", "STRAHLER")]
      st_write(catch_dl, dsn=file, layer='catchments')
    }
  )
}

shinyApp(ui, server)
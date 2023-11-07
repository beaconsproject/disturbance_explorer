library(sf)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(markdown)
library(shinyMatrix)

fda_list <- c("fda10ab","fda10ad")
x1_line <- st_read("www/fda10ab.gpkg", 'sd_line', quiet=T)
x1 <- pull(x1_line, TYPE_DISTURBANCE) %>% unique()
x2_poly <- st_read("www/fda10ab.gpkg", 'sd_poly', quiet=T) 
x2 <- pull(x2_poly, TYPE_DISTURBANCE) %>% unique()
m1 <- as.matrix(read_csv('docs/cas.csv')[40:60,2:4] %>% filter(TYPE_DISTURBANCE %in% x1))
m2 <- as.matrix(read_csv('docs/cas.csv')[1:39,2:4] %>% filter(TYPE_DISTURBANCE %in% x2))

ui = dashboardPage(skin="blue",
    dashboardHeader(title = "BEACONs Disturbance Explorer", titleWidth=320),
    dashboardSidebar(
        sidebarMenu(id="tabs",
            menuItem("Welcome!", tabName = "overview", icon = icon("th")),
            menuItem("Select study area", tabName = "select", icon = icon("th")),
            menuItem("Buffer features", tabName = "buffer", icon = icon("th")),
            menuItem("Download data", tabName = "download", icon = icon("th")),
            hr()
        ),
        conditionalPanel(
            condition="input.tabs=='select'",
            selectInput("select_fda", label="Select an FDA:", choices=c(fda_list), selected="10ab"),
            fileInput(inputId = "upload_poly", label = "Or upload a polygon:", multiple = FALSE, accept = ".gpkg")
        ),
         conditionalPanel(
           condition = "input.tabs == 'buffer'",
           sliderInput("buffer1", label="Linear buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
           sliderInput("buffer2", label="Areal buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
           sliderInput("area1", label="Min intact patch size (km2):", min=0, max=2000, value = 0, step=50, ticks=FALSE),
           hr(),
           actionButton("goButton", "Generate intactness map")
         ),
        conditionalPanel(
            condition="input.tabs=='download'",
            div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Download features"))
            )
    ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(".skin-blue .sidebar a { color: #8a8a8a; }")),
    tabItems(
      tabItem(tabName="overview",
            fluidRow(
                #box(title = "Mapview", leafletOutput("map1", height=750) %>% withSpinner(), width=12),
                tabBox(id = "one", width="8",
                    tabPanel("Mapview", leafletOutput("map1", height=750) %>% withSpinner()),
                    tabPanel("Custom buffers", checkboxInput("custom_buffers", "Use custom buffers", value=FALSE), tags$h4("Define linear buffer sizes:"), matrixInput("linear_buffers", value=m1, rows=list(names=FALSE, extend=TRUE), cols=list(names=TRUE)), tags$h4("Define areal buffer sizes:"), matrixInput("areal_buffers", value=m2, rows=list(names=FALSE, extend=TRUE), cols=list(names=TRUE))),
                    #tabPanel("Detailed statistics", tableOutput("tab2")),
                    tabPanel("Overview", includeMarkdown("docs/overview.md")),
                    #tabPanel("Workflow", includeMarkdown("docs/workflow.md")),
                    tabPanel("Quick start", includeMarkdown("docs/quick_start.md")),
                    tabPanel("Datasets", includeMarkdown("docs/datasets.md"))
                ),
                tabBox(
                    id = "two", width="4",
                    tabPanel("Statistics", tableOutput("tab1"))
                )
            )
        )
    )
  )
)

server = function(input, output, session) {

  ##############################################################################
  # Read input data
  ##############################################################################
  selected_fda <- reactive({
    if (input$select_fda>0) {
      paste0('www/',tolower(input$select_fda),'.gpkg')
    }
  })

  fda <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(selected_fda(), 'fda', quiet=T)
    } else {
      aoi_bnd()
    }
  })

  line <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(selected_fda(), 'sd_line', quiet=T)
    } else {
      aoi_line()
    }
  })

  poly <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(selected_fda(), 'sd_poly', quiet=T)
    } else {
      aoi_poly()
    }
  })

  fires <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(selected_fda(), 'fires', quiet=T)
    } else {
      aoi_fires()
    }
  })

  ifl2000 <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(selected_fda(), 'ifl_2000', quiet=T)
      } else {
      aoi_ifl2000()
    }
  })

  ifl2020 <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(selected_fda(), 'ifl_2020', quiet=T)
      } else {
      aoi_ifl2020()
    }
  })

  ##############################################################################
  # Uploaded data
  ##############################################################################
  aoi_bnd <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, 'fda', quiet=T) #%>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })
  
  aoi_line <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, 'sd_line', quiet=T) #%>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  aoi_poly <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, 'sd_poly', quiet=T) #%>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  aoi_fires <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, 'fires', quiet=T) #%>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  aoi_ifl2000 <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, 'ifl_2000', quiet=T) #%>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  aoi_ifl2020 <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file, 'ifl_2020', quiet=T) #%>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  observeEvent(c(input$select_fda,input$upload_poly), {
    x1 <- line() %>% pull(TYPE_DISTURBANCE) %>% unique()

    y1 <- line() %>%
      mutate(length_km=st_length(line())) %>%
      st_drop_geometry() %>%
      group_by(TYPE_DISTURBANCE) %>%
      summarize(LENGTH_KM = round(as.numeric(sum(length_km)/1000),1))
    cas <- read_csv('docs/cas.csv')[40:60,2:4] %>% filter(TYPE_DISTURBANCE %in% x1)
    y <- left_join(cas, y1) %>%
      relocate(TYPE_INDUSTRY, TYPE_DISTURBANCE, LENGTH_KM, BUFFER_SIZE)
    m1 <- as.matrix(y)
    
    #m1 <- as.matrix(read_csv('docs/cas.csv')[40:60,2:4] %>% filter(TYPE_DISTURBANCE %in% x1))
    updateMatrixInput(session, 'linear_buffers', m1)
  })

  observeEvent(c(input$select_fda,input$upload_poly), {
    x2 <- poly() %>% pull(TYPE_DISTURBANCE) %>% unique()

    y2 <- poly() %>%
      mutate(area_km2=st_area(poly())) %>%
      st_drop_geometry() %>%
      group_by(TYPE_DISTURBANCE) %>%
      summarize(AREA_KM2 = round(as.numeric(sum(area_km2)/1000000),3))
    cas <- read_csv('docs/cas.csv')[1:39,2:4] %>% filter(TYPE_DISTURBANCE %in% x2)
    y <- left_join(cas, y2) %>%
      relocate(TYPE_INDUSTRY, TYPE_DISTURBANCE, AREA_KM2, BUFFER_SIZE)
    m2 <- as.matrix(y)

    #m2 <- as.matrix(read_csv('docs/cas.csv')[1:39,2:4] %>% filter(TYPE_DISTURBANCE %in% x2))
    updateMatrixInput(session, 'areal_buffers', m2)
  })


  ##############################################################################
  # Buffer disturbances and calculate footprint and intactness
  ##############################################################################
  footprint_sf <- eventReactive(input$goButton, {
    aoi <- fda()
    if (input$custom_buffers==TRUE) {
      m1sub <- as_tibble(input$linear_buffers) %>% select(TYPE_DISTURBANCE, BUFFER_SIZE) %>%      
        mutate(BUFFER_SIZE=as.integer(BUFFER_SIZE))
      line <- left_join(line(), m1sub) %>% filter(!is.na(BUFFER_SIZE))
      v1 <- st_union(st_buffer(line, line$BUFFER_SIZE))
      m2sub <- as_tibble(input$areal_buffers) %>% select(TYPE_DISTURBANCE, BUFFER_SIZE) %>% 
        mutate(BUFFER_SIZE=as.integer(BUFFER_SIZE))
      poly <- left_join(poly(), m2sub) %>% filter(!is.na(BUFFER_SIZE))
      v2 <- st_union(st_buffer(poly, poly$BUFFER_SIZE))
    } else {
      v1 <- st_union(st_buffer(line(), input$buffer1))
      v2 <- st_union(st_buffer(poly(), input$buffer2))
    }
    v <- st_intersection(st_union(v1, v2), st_buffer(aoi, 100))
  })

  intactness_sf <- eventReactive(input$goButton, {
    aoi <- fda()
    ifl <- st_difference(aoi, footprint_sf())
    x <- st_cast(ifl, "POLYGON")
    x <- mutate(x, area_km2=as.numeric(st_area(x)/1000000))
    y <- filter(x, area_km2 > input$area1)
  })
  
  ##############################################################################
  # View initial set of maps
  ##############################################################################
  output$map1 <- renderLeaflet({
    intact2000 <- st_transform(ifl2000(), 4326)
    intact2020 <- st_transform(ifl2020(), 4326)
    m <- leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")
      #addPolygons(data=fdas, color='blue', fill=F, weight=2, group="FDAs")
        aoi_sf <- fda()
        aoi <- st_transform(aoi_sf, 4326)
        poly_clip <- st_transform(poly(), 4326)
        line_clip <- st_transform(line(), 4326)
        fires_clip <- st_transform(fires(), 4326)
        intact2000_clip <- st_transform(ifl2000(), 4326)
        intact2020_clip <- st_transform(ifl2020(), 4326)
        map_bounds1 <- aoi %>% st_bbox() %>% as.character()
        m <- m %>%
          fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
          #addPolygons(data=aoi, fillColor='yellow', color="black", fillOpacity=0.5, weight=4, group="Study region") %>%
          addPolygons(data=aoi, color='black', fill=F, weight=3, group="Study region") %>%
          addPolylines(data=line_clip, color='red', weight=2, group="Linear disturbances") %>%
          addPolygons(data=poly_clip, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group="Areal disturbances") %>%
          addPolygons(data=fires_clip, fill=T, stroke=F, fillColor='orange', fillOpacity=0.5, group="Fires") %>%
          addPolygons(data=intact2000_clip, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intactness 2000") %>%
          addPolygons(data=intact2020_clip, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intactness 2020") %>%
          addLayersControl(position = "topright",
                  baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                  overlayGroups = c("Study region", "Linear disturbances", "Areal disturbances", "Fires", "Intactness 2000", "Intactness 2020"),
                  options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(c("Fires", "Intactness 2000", "Intactness 2020"))
    m
  })

  ##############################################################################
  # Update map with intactness/footprint
  ##############################################################################
  observe({
    if (input$goButton) {
      fp_sf <- st_transform(footprint_sf(), 4326)
      intact_sf <- st_transform(intactness_sf(), 4326)
      proxy <- leafletProxy("map1") %>%
      clearGroup('Intactness') %>%
      clearGroup('Footprint') %>%
      addPolygons(data=intact_sf, color='darkblue', stroke=F, fillOpacity=0.5, group='Intactness') %>%
      addPolygons(data=fp_sf, color='black', stroke=F, fillOpacity=0.5, group='Footprint') %>%
      addLayersControl(position = "topright",
        baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
        overlayGroups = c("Study region", "Intactness", "Footprint", "Linear disturbances", "Areal disturbances", "Fires", "Intactness 2000", "Intactness 2020"),
        options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Footprint", "Fires", "Intactness 2000", "Intactness 2020"))
    }
  })

  ##############################################################################
  # Generate detailed statistics table
  ##############################################################################
    output$tab2 <- renderTable({
      x <- line() %>%
        mutate(length_km=st_length(line())) %>%
        st_drop_geometry() %>%
        group_by(TYPE_DISTURBANCE) %>%
        summarize(LENGTH_KM = mean(length_km))
      y <- left_join(as_tibble(m1), x) %>%
        relocate(TYPE_INDUSTRY, TYPE_DISTURBANCE, LENGTH_KM, BUFFER_SIZE)
      y
    })

  ##############################################################################
  # Generate statistics table
  ##############################################################################
    output$tab1 <- renderTable({
      x <- tibble(Attribute=c("Area of interest (km2)", "Linear disturbances (km)", "Areal disturbances (km2)", "Fires (%)", "IFL 2000 (%)", "IFL 2020 (%)", "Intactness (%)","Footprint (%)","Footprint + Fires (%)"), Value=NA)
      aoi <- sum(st_area(fda()))
      x$Value[x$Attribute=="Area of interest (km2)"] <- round(aoi/1000000, 0)
      x$Value[x$Attribute=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000()))/sum(st_area(fda()))*100,1)
      x$Value[x$Attribute=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020()))/sum(st_area(fda()))*100,1)

      # If button has been pressed at least once, keep the intactness/footprint values updated
      if (is.null(input$upload_poly)) {
          aoi <- sum(st_area(fda()))
          line <- st_intersection(st_geometry(line()), st_geometry(fda()))
          x$Value[x$Attribute=="Linear disturbances (km)"] <- round(sum(st_length(line()))/1000,1)
          poly <- st_intersection(st_geometry(poly()), st_geometry(fda()))
          x$Value[x$Attribute=="Areal disturbances (km2)"] <- round(sum(st_area(poly()))/1000000,1)
          fires <- st_intersection(st_geometry(fires()), st_geometry(fda()))
          x$Value[x$Attribute=="Fires (%)"] <- round(sum(st_area(fires()))/sum(st_area(fda()))*100,1)
          ifl2000 <- st_intersection(st_geometry(ifl2000()), st_geometry(fda()))
          ifl2020 <- st_intersection(st_geometry(ifl2020()), st_geometry(fda()))
          x$Value[x$Attribute=="Area of interest (km2)"] <- round(st_area(fda())/1000000,0)
          x$Value[x$Attribute=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000))/sum(st_area(fda()))*100,1)
          x$Value[x$Attribute=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020))/sum(st_area(fda()))*100,1)
        #}
      } else {
          aoi <- sum(st_area(aoi_bnd()))
          line <- st_intersection(st_geometry(line()), st_geometry(aoi_bnd()))
          x$Value[x$Attribute=="Linear disturbances (km)"] <- round(sum(st_length(line()))/1000,1)
          poly <- st_intersection(st_geometry(poly()), st_geometry(aoi_bnd()))
          x$Value[x$Attribute=="Areal disturbances (km2)"] <- round(sum(st_area(poly()))/1000000,1)
          fires <- st_intersection(st_geometry(fires()), st_geometry(aoi_bnd()))
          x$Value[x$Attribute=="Fires (%)"] <- round(sum(st_area(fires()))/sum(st_area(aoi_bnd()))*100,1)
          ifl2000 <- st_intersection(st_geometry(ifl2000()), st_geometry(aoi_bnd()))
          ifl2020 <- st_intersection(st_geometry(ifl2020()), st_geometry(aoi_bnd()))
          x$Value[x$Attribute=="Area of interest (km2)"] <- round(st_area(aoi_bnd())/1000000,0)
          x$Value[x$Attribute=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000))/sum(st_area(aoi_bnd()))*100,1)
          x$Value[x$Attribute=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020))/sum(st_area(aoi_bnd()))*100,1)
      }
      if(input$goButton > 0) {
        x$Value[x$Attribute=="Intactness (%)"] <- round(sum(st_area(intactness_sf()))/aoi*100, 1)
        x$Value[x$Attribute=="Footprint (%)"] <- round(sum(st_area(footprint_sf()))/aoi*100, 1)
        foot_fires <- st_union(st_union(footprint_sf()), st_union(fires()))
        x$Value[x$Attribute=="Footprint + Fires (%)"] <- round(sum(st_area(foot_fires))/aoi*100, 1)
      }
      x
    })

  ##############################################################################
  # Save features to a geopackage
  ##############################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("disturbance_explorer-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
      if(is.null(input$upload_poly)) { #upload polygon
        st_write(fda(), dsn=file, layer='aoi')
        st_write(line(), dsn=file, layer='linear_disturbance', append=TRUE)
        st_write(poly(), dsn=file, layer='areal_disturbance', append=TRUE)
        if (input$goButton) {
          st_write(footprint_sf(), dsn=file, layer='footprint', append=TRUE)
          st_write(intactness_sf(), dsn=file, layer='intactness', append=TRUE)
        }
      } else {
        poly_clip <- st_intersection(poly(), aoi_bnd())
        line_clip <- st_intersection(line(), aoi_bnd())
        st_write(aoi_bnd(), dsn=file, layer='aoi')
        st_write(line_clip, dsn=file, layer='linear_disturbance', append=TRUE)
        st_write(poly_clip, dsn=file, layer='areal_disturbance', append=TRUE)
        if (input$goButton) {
          st_write(footprint_sf(), dsn=file, layer='footprint', append=TRUE)
          st_write(intactness_sf(), dsn=file, layer='intactness', append=TRUE)
        }
      }
    }
  )

  #session$onSessionEnded(function() {
  #  stopApp()
  #})

}
shinyApp(ui, server)

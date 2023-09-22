# Import packages listed in req.txt
#req <- scan(file.path(dirname(getwd()), "req.txt"), character(), quiet = T)
#invisible(lapply(req, library, character.only = T))

library(sf)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(markdown)

fda_list <- c("10aa", "10ab", "10ac", "10ad", "10ba", "10bb", "10bc", "10bd", "10be")

ui = dashboardPage(skin="blue",
    dashboardHeader(title = "Regional Disturbance Explorer", titleWidth=320),
    dashboardSidebar(
        sidebarMenu(id="tabs",
            menuItem("Overview", tabName = "overview", icon = icon("th")),
            menuItem("Select AOI", tabName = "select", icon = icon("th")),
            menuItem("Buffer features", tabName = "buffer", icon = icon("th")),
            menuItem("Download data", tabName = "download", icon = icon("th")),
            hr()
        ),
        conditionalPanel(
            condition="input.tabs=='select'",
            selectInput("select_fda", label="Select an FDA:", choices=c("Full extent",fda_list), selected="10ab"),
            #selectInput("select_fda", label="Select an FDA:", choices=fda_list,
            fileInput(inputId = "upload_poly", label = "Or upload a polygon:", multiple = FALSE, accept = ".gpkg")
        ),
         conditionalPanel(
           condition = "input.tabs == 'buffer'",
           sliderInput("buffer1", label="Linear buffer size (m):", min=0, max=2000, value = 1000, step=100, ticks=FALSE),
           sliderInput("buffer2", label="Areal buffer size (m):", min=0, max=2000, value = 1000, step=100, ticks=FALSE),
           sliderInput("area1", label="Min size of intact areas (km2):", min=0, max=2000, value = 500, step=100, ticks=FALSE),
           actionButton("goButton", "Generate intactness map")
         ),
        conditionalPanel(
            condition="input.tabs=='download'",
            div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Download features"))
            )
    ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(".skin-blue .sidebar a { color: #444; }")),
    tabItems(
      tabItem(tabName="overview",
            fluidRow(
                #box(title = "Mapview", leafletOutput("map1", height=750) %>% withSpinner(), width=12),
                tabBox(id = "one", width="12",
                    tabPanel("Mapview", leafletOutput("map1", height=750) %>% withSpinner()),
                    tabPanel("Statistics", tableOutput("tab1")),
                    tabPanel("Overview", includeMarkdown("docs/overview.md")),
                    tabPanel("Quick start", includeMarkdown("docs/quick_start.md"))
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
  fda_all <- reactive({
    st_read("www/fda10.gpkg", 'fda', quiet=T)
  })

  bnd <- reactive({
    st_read('www/fda10.gpkg', 'bnd', quiet=T)
  })

  line_all <- reactive({
    st_read("www/fda10.gpkg", 'sd_line', quiet=T)
  })

  poly_all <- reactive({
    st_read("www/fda10.gpkg", 'sd_poly', quiet=T)
  })

  ifl2000_all <- reactive({
    st_read("www/fda10_extra.gpkg", 'ifl_2000', quiet=T)
  })

  ifl2020_all <- reactive({
    st_read("www/fda10_extra.gpkg", 'ifl_2020', quiet=T)
  })

  fires_all <- reactive({
    st_read("www/fda10_extra.gpkg", 'fires', quiet=T)
  })

  selected_fda <- reactive({
    if (input$select_fda>0) {
      paste0('www/fda',tolower(input$select_fda),'.gpkg')
    } else {
     "www/fda10.gpkg"
    }
  })

  fda <- reactive({
    st_read(selected_fda(), 'fda', quiet=T)
  })

  line <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(selected_fda(), 'sd_line', quiet=T)
    } else {
      st_read("www/fda10.gpkg", 'sd_line', quiet=T) %>%
        st_intersection(aoi_sf()) %>%
        st_cast('MULTILINESTRING')
    }
  })

  poly <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(selected_fda(), 'sd_poly', quiet=T)
    } else {
      st_read("www/fda10.gpkg", 'sd_poly', quiet=T) %>%
        st_intersection(aoi_sf()) %>%
        st_cast('MULTIPOLYGON')
    }
  })

  fires <- reactive({
    if (is.null(input$upload_poly)) {
      st_read(selected_fda(), 'fires', quiet=T)
    } else {
      st_read("www/fda10_extra.gpkg", 'fires', quiet=T) %>%
        st_intersection(aoi_sf()) %>%
        st_cast('MULTIPOLYGON')
    }
  })

  ifl2000 <- reactive({
    if (is.null(input$upload_poly)) {
      if(input$select_fda=="Full extent") {
        st_read("www/fda10_extra.gpkg", 'ifl_2000', quiet=T)
      } else {
        st_read(selected_fda(), 'ifl_2000', quiet=T)
      }
    } else {
        st_read("www/fda10_extra.gpkg", 'ifl_2000', quiet=T) %>%
          st_intersection(aoi_sf())
    }
  })

  ifl2020 <- reactive({
   if (is.null(input$upload_poly)) {
     if(input$select_fda=="Full extent") {
        st_read("www/fda10_extra.gpkg", 'ifl_2020', quiet=T)
      } else {
        st_read(selected_fda(), 'ifl_2020', quiet=T)
      }
    } else {
        st_read("www/fda10_extra.gpkg", 'ifl_2020', quiet=T) %>%
          st_intersection(aoi_sf())
    }
  })

  ##############################################################################
  # Upload AOI and zoom in to its extent
  ##############################################################################
  aoi_sf <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file) #%>% st_transform(4326)
    } else {
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  ##############################################################################
  # Buffer disturbances and calculate footprint and intactness
  ##############################################################################
  footprint_sf <- eventReactive(input$goButton, {
   if (is.null(input$upload_poly)) {
      if (input$select_fda=='Full extent') {
        aoi <- st_union(fda_all())
        v1 <- st_union(st_buffer(line_all(), input$buffer1))
        v2 <- st_union(st_buffer(poly_all(), input$buffer2))
      } else {
        aoi <- fda()
        v1 <- st_union(st_buffer(line(), input$buffer1))
        v2 <- st_union(st_buffer(poly(), input$buffer2))
      }
    } else {
      aoi <-  aoi_sf()
      v1 <- st_union(st_buffer(line(), input$buffer1))
      v2 <- st_union(st_buffer(poly(), input$buffer2))
    }    
    v <- st_intersection(st_union(v1, v2), st_buffer(aoi, 100))
  })

  intactness_sf <- eventReactive(input$goButton, {
   if (is.null(input$upload_poly)) {
      if (input$select_fda=='Full extent') {
        aoi <- fda_all() %>%
          summarize(geometry = st_union(geom))
      } else {
        aoi <- fda()
      }
    } else {
      aoi <-  aoi_sf()
    }
    ifl <- st_difference(aoi, footprint_sf())
    x <- st_cast(ifl, "POLYGON")
    x <- mutate(x, area_km2=as.numeric(st_area(x)/1000000))
    y <- filter(x, area_km2 > input$area1)

    return(y)
  })
  
  ##############################################################################
  # View initial set of maps
  ##############################################################################
  output$map1 <- renderLeaflet({
    fdas <- st_transform(fda_all(), 4326)
    line <- st_transform(line_all(), 4326)
    poly <- st_transform(poly_all(), 4326)
    fires <- st_transform(fires_all(), 4326)
    intact2000 <- st_transform(ifl2000(), 4326)
    intact2020 <- st_transform(ifl2020(), 4326)
    m <- leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
      addPolygons(data=fdas, color='blue', fill=F, weight=2, group="FDAs")
   if (is.null(input$upload_poly)) {
      if (input$select_fda=='Full extent') {
        bnd <- st_transform(bnd(), 4326)
        map_bounds <- bnd %>% st_bbox() %>% as.character()
        m <- m %>%
          addPolygons(data=bnd, color='black', fill=F, weight=3, group="Data extent") %>%
          fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
          addPolylines(data=line, color='black', weight=2, group="Linear disturbances") %>%
          addPolygons(data=poly, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Areal disturbances") %>%
          addPolygons(data=fires, fill=T, stroke=F, fillColor='orange', fillOpacity=0.5, group="Fires") %>%
          addPolygons(data=intact2000, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intactness 2000") %>%
          addPolygons(data=intact2020, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intactness 2020") %>%
          addLayersControl(position="topright",
            baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery"),
            overlayGroups = c("Data extent","FDAs","Areal disturbances","Linear disturbances", "Fires", "Intactness 2000", "Intactness 2020"),
            options = layersControlOptions(collapsed=F)) %>%
          hideGroup(c("FDAs", "Fires", "Intactness 2000", "Intactness 2020"))
      } else { # User selected FDA
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
          #addPolygons(data=aoi, fillColor='yellow', color="black", fillOpacity=0.5, weight=4, group="AOI") %>%
          addPolygons(data=aoi, color='black', fill=F, weight=3, group="AOI") %>%
          addPolylines(data=line_clip, color='red', weight=2, group="Linear disturbances") %>%
          addPolygons(data=poly_clip, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group="Areal disturbances") %>%
          addPolygons(data=fires_clip, fill=T, stroke=F, fillColor='orange', fillOpacity=0.5, group="Fires") %>%
          addPolygons(data=intact2000_clip, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intactness 2000") %>%
          addPolygons(data=intact2020_clip, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intactness 2020") %>%
          addLayersControl(position = "topright",
                  baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                  overlayGroups = c("AOI", "Linear disturbances", "Areal disturbances", "FDAs", "Fires", "Intactness 2000", "Intactness 2020"),
                  options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(c("FDAs", "Fires", "Intactness 2000", "Intactness 2020"))
      }
    } else { # User uploaded AOI
      #poly_clip <- st_intersection(st_geometry(poly()), st_geometry(aoi_sf())) %>% st_transform(4326)
      #line_clip <- st_intersection(st_geometry(line()), st_geometry(aoi_sf())) %>% st_transform(4326)
      aoi <- st_transform(aoi_sf(), 4326)
      line_clip <- st_transform(line(), 4326)
      poly_clip <- st_transform(poly(), 4326)
      fires_clip <- st_transform(fires(), 4326)
      intact2000_clip <- st_transform(ifl2000(), 4326)
      intact2020_clip <- st_transform(ifl2020(), 4326)
      map_bounds2 <- aoi %>% st_bbox() %>% as.character()
      m <- m %>%
        fitBounds(map_bounds2[1], map_bounds2[2], map_bounds2[3], map_bounds2[4]) %>%
        #addPolygons(data = aoi, fillColor='yellow', color="black", fillOpacity=0.5, weight=4, group="AOI") %>%
        addPolygons(data=aoi, color='black', fill=F, weight=3, group="AOI") %>%
        addPolylines(data=line_clip, color='red', weight=2, group="Linear disturbances") %>%
        addPolygons(data=poly_clip, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group="Areal disturbances") %>%
        addPolygons(data=fires_clip, fill=T, stroke=F, fillColor='orange', fillOpacity=0.5, group="Fires") %>%
        addPolygons(data=intact2000_clip, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intactness 2000") %>%
        addPolygons(data=intact2020_clip, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intactness 2020") %>%
        addLayersControl(position = "topright",
                  baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                  overlayGroups = c("AOI","Linear disturbances", "Areal disturbances", "FDAs", "Fires", "Intactness 2000", "Intactness 2020"),
                  options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("FDAs", "Fires", "Intactness 2000", "Intactness 2020"))
    }
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
        overlayGroups = c("AOI", "Intactness", "Footprint", "FDAs", "Linear disturbances", "Areal disturbances", "Intactness 2000", "Intactness 2020"),
        options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Footprint", "FDAs", "Intactness 2000", "Intactness 2020"))
    }
  })

  ##############################################################################
  # Generate statistics table
  ##############################################################################
    output$tab1 <- renderTable({
      x <- tibble(Map=c("Area of interest (km2)", "Fires (%)", "IFL 2000 (%)", "IFL 2020 (%)", "Intactness (%)","Footprint (%)"), Area=NA)
      aoi <- sum(st_area(fda_all()))
      x$Area[x$Map=="Area of interest (km2)"] <- round(aoi/1000000, 0)
      x$Area[x$Map=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000()))/sum(st_area(fda_all()))*100,1)
      x$Area[x$Map=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020()))/sum(st_area(fda_all()))*100,1)

      # If button has been pressed at least once, keep the intactness/footprint values updated
      if (is.null(input$upload_poly)) {
        if (input$select_fda=='Full extent') {
          aoi <- sum(st_area(bnd()))
          x$Area[x$Map=="Area of interest (km2)"] <- round(st_area(bnd())/1000000,0)
          x$Area[x$Map=="Fires (%)"] <- round(sum(st_area(fires_all()))/sum(st_area(bnd()))*100,1)
          x$Area[x$Map=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000()))/sum(st_area(bnd()))*100,1)
          x$Area[x$Map=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020()))/sum(st_area(bnd()))*100,1)
        } else {
          aoi <- sum(st_area(fda()))
          fires <- st_intersection(st_geometry(fires()), st_geometry(fda()))
          x$Area[x$Map=="Fires (%)"] <- round(sum(st_area(fires()))/sum(st_area(fda()))*100,1)
          ifl2000 <- st_intersection(st_geometry(ifl2000()), st_geometry(fda()))
          ifl2020 <- st_intersection(st_geometry(ifl2020()), st_geometry(fda()))
          x$Area[x$Map=="Area of interest (km2)"] <- round(st_area(fda())/1000000,0)
          x$Area[x$Map=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000))/sum(st_area(fda()))*100,1)
          x$Area[x$Map=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020))/sum(st_area(fda()))*100,1)
        }
      } else {
          aoi <- sum(st_area(aoi_sf()))
          fires <- st_intersection(st_geometry(fires()), st_geometry(aoi_sf()))
          x$Area[x$Map=="Fires (%)"] <- round(sum(st_area(fires()))/sum(st_area(aoi_sf()))*100,1)
          ifl2000 <- st_intersection(st_geometry(ifl2000()), st_geometry(aoi_sf()))
          ifl2020 <- st_intersection(st_geometry(ifl2020()), st_geometry(aoi_sf()))
          x$Area[x$Map=="Area of interest (km2)"] <- round(st_area(aoi_sf())/1000000,0)
          x$Area[x$Map=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000))/sum(st_area(aoi_sf()))*100,1)
          x$Area[x$Map=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020))/sum(st_area(aoi_sf()))*100,1)
      }
      if(input$goButton > 0) {
        x$Area[x$Map=="Intactness (%)"] <- round(sum(st_area(intactness_sf()))/aoi*100, 1)
        x$Area[x$Map=="Footprint (%)"] <- round(sum(st_area(footprint_sf()))/aoi*100, 1)
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
        if (input$select_fda=='Full extent') {
          st_write(bnd(), dsn=file, layer='aoi')
          st_write(line_all(), dsn=file, layer='linear_disturbance', append=TRUE)
          st_write(poly_all(), dsn=file, layer='areal_disturbance', append=TRUE)
          if (input$goButton) {
            st_write(footprint_sf(), dsn=file, layer='footprint', append=TRUE)
            st_write(intactness_sf(), dsn=file, layer='intactness', append=TRUE)
          }
        } else {
          st_write(fda(), dsn=file, layer='aoi')
          st_write(line(), dsn=file, layer='linear_disturbance', append=TRUE)
          st_write(poly(), dsn=file, layer='areal_disturbance', append=TRUE)
          if (input$goButton) {
            st_write(footprint_sf(), dsn=file, layer='footprint', append=TRUE)
            st_write(intactness_sf(), dsn=file, layer='intactness', append=TRUE)
          }
      }
      } else {
        poly_clip <- st_intersection(poly(), aoi_sf())
        line_clip <- st_intersection(line(), aoi_sf())
        st_write(aoi_sf(), dsn=file, layer='aoi')
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

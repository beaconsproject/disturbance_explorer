library(sf)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)

bnd <- st_read('www/fda9.gpkg', 'bnd', quiet=T)
fda <- st_read('www/fda9.gpkg', 'fda', quiet=T) #%>% st_zm(drop=T)
line = st_read('www/fda9.gpkg', 'sd_line', quiet=T)
poly = st_read('www/fda9.gpkg', 'sd_poly', quiet=T)
fda_list <- pull(fda, FDA)

ui = dashboardPage(skin="blue",
    dashboardHeader(title = "Regional Disturbance Explorer", titleWidth=320),
    dashboardSidebar(
        sidebarMenu(id="tabs",
            menuItem("Overview", tabName = "overview", icon = icon("th")),
            menuItem("Select AOI", tabName = "select", icon = icon("th")),
            #menuItem("Upload & clip using AOI", tabName = "upload", icon = icon("th")),
            menuItem("Buffer features", tabName = "buffer", icon = icon("th")),
            menuItem("Download data", tabName = "download", icon = icon("th")),
            hr()
        ),
        conditionalPanel(
            condition="input.tabs=='select'",
            selectInput("select_fda", label="Select an FDA:", choices=c("",fda_list), selected=""),
        #    actionButton("clipData1", "Clip FDA features"),
        #),
        #conditionalPanel(
        #    condition="input.tabs=='upload'",
            fileInput(inputId = "upload_poly", label = "Or upload a polygon:", multiple = FALSE, accept = ".gpkg")
            #actionButton("clipData2", "Clip AOI features")
        ),
         conditionalPanel(
           condition = "input.tabs == 'buffer'",
           sliderInput("buffer1", label="Linear buffer size (m):", min=0, max=2000, value = 1000, step=100, ticks=FALSE),
           sliderInput("buffer2", label="Areal buffer size (m):", min=0, max=2000, value = 1000, step=100, ticks=FALSE),
           sliderInput("area1", label="Min size of intact areas (km2):", min=0, max=2000, value = 500, step=100, ticks=FALSE),
           #checkboxInput("nobound", label = 'Boundary is not a barrier', value = F),
           actionButton("goButton", "Generate intactness map")
         ),
        conditionalPanel(
            condition="input.tabs=='download'",
            div(style="position:relative; left:calc(5%);", downloadButton("downloadData", "Download features"))
        )
    ),
  dashboardBody(
    tabItems(
        tabItem(tabName="overview",
            fluidRow(
                #box(title = "Mapview", leafletOutput("map1", height=750) %>% withSpinner(), width=12),
                tabBox(id = "one", width="12",
                    tabPanel("Mapview", leafletOutput("map1", height=750) %>% withSpinner()),
                    tabPanel("Statistics", tableOutput("tab1")),
                    tabPanel("About", includeMarkdown("docs/overview.md"))
                )
            )
        )
    )
  )
)

server = function(input, output) {

    ################################################################################################
    # Initial full extent mapview
    ################################################################################################

    output$map1 <- renderLeaflet({
        bnd <- st_transform(bnd, 4326)
        fdas <- st_transform(fda, 4326)
        lines <- st_transform(line, 4326)
        polygons <- st_transform(poly, 4326)
        map_bounds <- bnd %>% st_bbox() %>% as.character()
        m <- leaflet() %>% 
            addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
            addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
            addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
            addPolygons(data=bnd, color='black', fill=F, weight=2, group="Data extent") %>%
            fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) #%>% # set view to the selected FDA
        m <- m %>%
            addPolygons(data=fdas, color='blue', fill=F, weight=2, group="FDAs") %>%
            addPolylines(data=lines, color='black', weight=2, group="Lines") %>%
            addPolygons(data=polygons, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group="Polygons") %>%
            addLayersControl(position="topright",
                baseGroups=c("Esri.WorldTopoMap","Esri.WorldImagery"),
                overlayGroups = c("Data extent","FDAs","Polygons","Lines"),
                options = layersControlOptions(collapsed=F)) %>%
            hideGroup(c("FDAs"))
    })

    ################################################################################################
    # Select FDA and zoom in to its extent
    ################################################################################################

    selected_fda <- reactiveValues(
        poly = NULL
    )

    fda_poly <- reactiveValues(
        poly = NULL
    )
    
    fda_line <- reactiveValues(
        line = NULL
    )

    ii <- reactiveValues(
        val = NULL
    )

    observeEvent(input$select_fda, {
        if (input$select_fda %in% fda_list) {
            selected_fda$poly <- filter(fda, FDA==input$select_fda) #%>% st_transform(4326)
            #selected_fda <- eventReactive(input$select_fda, {
            #    if (input$select_fda %in% fda_list) {
            #        selected_fda$poly <- filter(fda, FDA==input$select_fda) #%>% st_transform(4326)
            #    }
            #})
            
            #clipped_poly <- eventReactive(input$select_fda, {
            #    if (input$select_fda %in% fda_list) {
                    #selected_fda$poly <- filter(fda, FDA==input$select_fda) #%>% st_transform(4326)
            #        fda_poly$poly <- st_intersection(poly, selected_fda()) #%>% st_transform(4326)
            #    }
            #})

            #clipped_line <- eventReactive(input$select_fda, {
            #    if (input$select_fda %in% fda_list) {
                    #selected_fda$poly <- filter(fda, FDA==input$select_fda) #%>% st_transform(4326)
            #        fda_line$line <- st_intersection(line, selected_fda()) #%>% st_transform(4326)
            #    }
            #})
            
            # Clip to FDA and display those features
            #ii$val = 0
            fda_poly$poly <- st_intersection(poly, selected_fda$poly) #%>% st_transform(4326)
            fda_line$line <- st_intersection(line, selected_fda$poly) #%>% st_transform(4326)
            fda1 <- st_transform(selected_fda$poly, 4326) # (selected_fda$poly, 4326)
            poly1 <- st_transform(fda_poly$poly, 4326) # (fda_poly$poly, 4326)
            line1 <- st_transform(fda_line$line, 4326) # (fda_line$line, 4326)

            # Alternatively, if they exist, read FDA-level layers
            #fda1 <- st_read(paste0('www/fda_',input$select_fda,'.gpkg'), 'fda', quiet=T) %>% st_transform(4326) #%>% st_zm(drop=T)
            #line1 = st_read(paste0('www/fda_',input$select_fda,'.gpkg'), 'sd_line', quiet=T) %>% st_transform(4326)
            #poly1 = st_read(paste0('www/fda_',input$select_fda,'.gpkg'), 'sd_poly', quiet=T) %>% st_transform(4326)
            
            map_bounds1 <- fda1 %>% st_bbox() %>% as.character()
            m <- leafletProxy("map1") %>%
                fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
                clearGroup("Poly_clipped") %>%
                clearGroup("Line_clipped") %>%
                clearGroup("FDA") %>%
                clearGroup("AOI")
            m <- m %>%
                addPolygons(data=fda1, fillColor='yellow', color="black", fillOpacity=0.5, weight=2, group="FDA") %>%
                addPolylines(data=line1, color='red', weight=2, group="Line_clipped") %>%
                addPolygons(data=poly1, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Poly_clipped") %>%
                addLayersControl(position = "topright",
                    baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                    overlayGroups = c("FDA","FDAs","Line_clipped", "Poly_clipped","Lines","Polygons"),
                    options = layersControlOptions(collapsed = FALSE)) %>%
                hideGroup(c("FDAs","Lines","Polygons"))
        }
    })

    selected_aoi <- reactiveValues(
        poly = NULL
    )

    aoi_poly <- reactiveValues(
        poly = NULL
    )
    
    aoi_line <- reactiveValues(
        line = NULL
    )

    ################################################################################################
    # Upload AOI and zoom in to its extent
    ################################################################################################

    observe({
        req(input$upload_poly)
        file <- input$upload_poly$datapath
        ext <- tools::file_ext(file)
        if(ext == "gpkg"){
            selected_aoi$poly <- st_read(file) #%>% st_transform(4326)
            
            # Clip to area of interest and display those features
            #ii$val = 1
            aoi_poly$poly <- st_intersection(poly, selected_aoi$poly) %>% st_transform(4326)
            aoi_line$line <- st_intersection(line, selected_aoi$poly) %>% st_transform(4326)
            aoi <- st_transform(selected_aoi$poly, 4326)
            
            map_bounds2 <- aoi %>% st_bbox() %>% as.character()
            m <- leafletProxy("map1") %>%
                fitBounds(map_bounds2[1], map_bounds2[2], map_bounds2[3], map_bounds2[4]) %>%
                clearGroup("Poly_clipped") %>%
                clearGroup("Line_clipped") %>%
                clearGroup("AOI") %>%
                clearGroup("FDA")
            m <- m %>%
                addPolygons(data = aoi, fillColor='yellow', color="black", fillOpacity=0.5, weight=2, group="AOI") %>%
                addPolylines(data=aoi_line$line, color='red', weight=2, group="Line_clipped") %>%
                addPolygons(data=aoi_poly$poly, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Poly_clipped") %>%
                addLayersControl(position = "topright",
                    baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                    overlayGroups = c("AOI","FDAs","Line_clipped", "Poly_clipped","Lines","Polygons"),
                    options = layersControlOptions(collapsed = FALSE)) %>%
                hideGroup(c("FDAs","Lines","Polygons"))
          } else{
              showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
          }
    })

    ################################################################################################
    # Buffer disturbances and calculate footprint and intactness
    ################################################################################################

    # Footprint & Intactness
    observeEvent(input$goButton, {

        # footprint
        v1 <- st_union(st_buffer(line, input$buffer1))
        v2 <- st_union(st_buffer(poly, input$buffer2))
        v <- st_intersection(st_union(v1, v2), bnd)
        ifl <- st_difference(bnd, v)
        x <- st_cast(ifl, "POLYGON")
        x <- mutate(x, area_km2=as.numeric(st_area(x)/1000000))
        y <- filter(x, area_km2 > input$area1)
        vv <- st_transform(v, 4326)
        yy <- st_transform(y, 4326)
        fdas <- st_transform(fda, 4326)
        m <- leafletProxy("map1") %>%
            #fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
            #clearGroup("Poly_clipped") %>%
            #clearGroup("Line_clipped") %>%
            clearGroup("Intactness") %>%
            clearGroup("Footprint")
        m <- m %>%
            addPolygons(data=fdas, color='blue', fill=F, weight=2, group="FDAs") %>%
            addPolygons(data=yy, color='darkgreen', stroke=F, fillOpacity=0.5, group='Intactness') %>%
            addPolygons(data=vv, color='black', stroke=F, fillOpacity=0.5, group='Footprint') %>%
            #addPolygons(data=vv, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Footprint") %>%
            addLayersControl(position = "topright",
                baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                overlayGroups = c("FDAs","Intactness","Footprint","Lines","Polygons"),
                options = layersControlOptions(collapsed = FALSE)) %>%
            hideGroup(c("Footprint","Lines","Polygons"))
    })

    ################################################################################################
    # Intactness table
    ################################################################################################
    output$tab1 <- renderTable({
    x <- tibble(Map=c("FDA (km2)","IFL 2000 (%)","IFL 2020 (%)","Intactness (%)","Footprint (%)"), Area=NA)
    #x$Area[x$Map=="FDA (km2)"] <- round(st_area(bnd())/1000000,0)
    #x$Area[x$Map=="IFL 2000 (%)"] <- round(sum(st_area(ifl2000()))/st_area(bnd())*100,1)
    #x$Area[x$Map=="IFL 2020 (%)"] <- round(sum(st_area(ifl2020()))/st_area(bnd())*100,1)

    # If button has been pressed at least once, keep the intactness/footprint values updated
    #if(input$goButton > 0) {
    #  x$Area[x$Map=="Intactness (%)"] <- round(sum(st_area(intactness_sf()))/st_area(bnd())*100,1)
    #  x$Area[x$Map=="Footprint (%)"] <- round(sum(st_area(footprint_sf()))/st_area(bnd())*100,1)
    #}
    x
    })

    ################################################################################################
    # Save features to a geopackage
    ################################################################################################

    output$downloadData <- downloadHandler(
        filename = function() { paste("data-", Sys.Date(), ".gpkg", sep="") },
        content = function(file) {
            #if (ii$val==1) {
            #    st_write(st_transform(selected_aoi$poly, 3579), dsn=file, layer='aoi')
                #st_write(footprint_sf(), dsn=file, layer='footprint', append=TRUE)
            #    st_write(st_transform(aoi_line$line, 3579), dsn=file, layer='sd_line', append=TRUE)
            #    st_write(st_transform(aoi_poly$poly, 3579), dsn=file, layer='sd_poly', append=TRUE)
            #} else if (ii$val==0) {
            #    st_write(st_transform(selected_fda$poly, 3579), dsn=file, layer='fda')
                #st_write(footprint_sf(), dsn=file, layer='footprint', append=TRUE)
            #    st_write(st_transform(fda_line$line, 3579), dsn=file, layer='sd_line', append=TRUE)
            #    st_write(st_transform(fda_poly$poly, 3579), dsn=file, layer='sd_poly', append=TRUE)
            #} else {
                st_write(st_transform(fda, 3579), dsn=file, layer='fda')
                #st_write(footprint_sf(), dsn=file, layer='footprint', append=TRUE)
                st_write(st_transform(line, 3579), dsn=file, layer='sd_line', append=TRUE)
                st_write(st_transform(poly, 3579), dsn=file, layer='sd_poly', append=TRUE)
            #}
        }
    )

}
shinyApp(ui, server)
addDisplayServer <- function(input, output, session, project, map, rv){
  
  ################################################################################################
  ################################################################################################
  # Add display elements
  ################################################################################################
  ################################################################################################
  ## Enable button
  ################################################################################################
  observe({
    cond_display <- !is.null(input$display1) ||
      !is.null(input$display2) ||
      !is.null(input$display3)
    
    cond_layers <- (input$display4a != "" && input$display4a != "Select a layer") ||
      (input$display4b != "" && input$display4b != "Select a layer") ||
      (input$display4c != "" && input$display4c != "Select a layer")
    
    cond_rast <- !is.null(input$rast1) || !is.null(input$rast2)
    
    if (cond_display || cond_layers || cond_rast) {
      shinyjs::enable("confExtra")
    } 
  })
  
  observe({
    req(input$display4)
    file <- input$display4$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "display4a", choices = c("Select a layer", layers))
  })
  observe({
    req(input$display4)
    file <- input$display4$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "display4b", choices = c("Select a layer", layers))
  })
  observe({
    req(input$display4)
    file <- input$display4$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "display4c", choices = c("Select a layer", layers))
  })
  
  # Display1
  display1_sf <- reactive({
    i <- NULL
    if(input$confExtra){
      req(input$confExtra)
      if(is.null(input$extraupload)){
        return(NULL)
      } else if (input$extraupload == "extrashp"){
        if(!is.null(input$display1)){
          req(input$display1)
          i <- read_shp_from_upload(input$display1) %>%
            dplyr::select(-any_of("fid")) %>%
            st_zm(drop = TRUE, what = "ZM")  %>%
            st_make_valid()
          
          shp_file <- input$display1$name[grepl("\\.shp$", input$display1$name)][1]
          name <- tools::file_path_sans_ext(shp_file)
          rv$display1_name(name)
          
          geom_type <- unique(sf::st_geometry_type(i))
          if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
            i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
          } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
            i <- suppressWarnings(sf::st_cast(i, "MULTILINESTRING"))
          } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
            i <- suppressWarnings(sf::st_cast(i, "POINT"))
          }
        }
      } else if (input$extraupload == "extragpkg"){
        req(input$display4)
        req(input$display4a)
        
        if(input$display4a != "Select a layer"){
          gpkg_path <- file.path(tempdir(), paste0("uploaded_", input$display4$name))
          file.copy(input$display4$datapath, gpkg_path, overwrite = TRUE)
          i <- st_read(gpkg_path, input$display4a, quiet = TRUE)
          name <- substr(input$display4a, 1, 25)
          rv$display1_name(name)
        }
      }
    }
    rv$display1_sf(i)
    return(i)
  })
  
  display2_sf <- reactive({
    i <- NULL
    
    if(input$confExtra){
      req(input$confExtra)
      if(is.null(input$extraupload)){
        return(NULL)
      } else if(input$extraupload == "extrashp"){
        if(!is.null(input$display2)){
          req(input$display2)
          i <- read_shp_from_upload(input$display2) %>%
            dplyr::select(-any_of("fid")) %>%
            st_zm(drop = TRUE, what = "ZM")  %>%
            st_make_valid()
          
          shp_file <- input$display2$name[grepl("\\.shp$", input$display2$name)][1]
          name <- tools::file_path_sans_ext(shp_file)
          rv$display2_name(name)
          
          geom_type <- unique(sf::st_geometry_type(i))
          if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
            i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
          } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
            i <- suppressWarnings(sf::st_cast(i, "MULTILINESTRING"))
          } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
            i <- suppressWarnings(sf::st_cast(i, "POINT"))
          }
        }
      } else if (input$extraupload == "extragpkg"){
        req(input$display4)
        req(input$display4b)
        if(input$display4b != "Select a layer"){
          gpkg_path <- file.path(tempdir(), paste0("uploaded_", input$display4$name))
          file.copy(input$display4$datapath, gpkg_path, overwrite = TRUE)
          i <- st_read(gpkg_path, input$display4b, quiet = TRUE)
          name <- substr(input$display4b, 1, 25)
          rv$display2_name(name)
        }
      }
    }
    rv$display2_sf(i)
    return(i)
  })  
  
  display3_sf <- reactive({
    i <- NULL
    
    if(input$confExtra){
      req(input$confExtra)
      if(is.null(input$extraupload)){
        return(NULL)
      } else if(input$extraupload == "extrashp"){
        if(!is.null(input$display3)){
          req(input$display3)
          i <- read_shp_from_upload(input$display3) %>%
            dplyr::select(-any_of("fid")) %>%
            st_zm(drop = TRUE, what = "ZM")  %>%
            st_make_valid()
          
          shp_file <- input$display3$name[grepl("\\.shp$", input$display3$name)][1]
          name <- tools::file_path_sans_ext(shp_file)
          rv$display3_name(name)
          
          geom_type <- unique(sf::st_geometry_type(i))
          if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
            i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
          } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
            i <- suppressWarnings(sf::st_cast(i, "MULTILINESTRING"))
          } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
            i <- suppressWarnings(sf::st_cast(i, "POINT"))
          }
        }
      } else if (input$extraupload == "extragpkg"){
        req(input$display4)
        req(input$display4c)
        
        if(input$display4c != "Select a layer"){
          gpkg_path <- file.path(tempdir(), paste0("uploaded_", input$display4$name))
          file.copy(input$display4$datapath, gpkg_path, overwrite = TRUE)
          i <- st_read(gpkg_path, input$display4c, quiet = TRUE)
          name <- substr(input$display4c, 1, 25)
          rv$display3_name(name)
        }
      }
    }
    rv$display3_sf(i)
    return(i)
  })  
  
  rast1 <- reactive({
    i <- NULL
    if(input$confExtra){
      req(input$confExtra)
      if(!is.null(input$rast1)){
        req(input$rast1)
        path <- input$rast1$datapath
        i <- raster::raster(path)  # Load raster using the raster package
        name <- sub("\\..*$", "", input$rast1$name)
        rv$rast1_name(name)
      }
    }
    return(i)
  }) 
  
  rast2 <- reactive({
    i <- NULL
    if(input$confExtra){
      req(input$confExtra)
      if(!is.null(input$rast2)){
        req(input$rast2)
        path <- input$rast2$datapath
        i <- raster::raster(path)  # Load raster using the raster package
        name <- sub("\\..*$", "", input$rast2$name)
        rv$rast2_name(name)
      }
    }
    return(i)
  }) 
  
  ###########################
  # Map extra layers
  observeEvent(input$confExtra,{ 
    
    map <- leafletProxy("map") %>%
      clearGroup(rv$display1_name()) %>%
      clearGroup(rv$display2_name()) %>%
      clearGroup(rv$display3_name()) %>%
      clearGroup(rv$rast1_name())
    
    add_names_new <- rv$add_names()
    
    if (isMappable(display1_sf())) { 
      display1 <- st_transform(display1_sf(), 4326)
      geom_type <- unique(sf::st_geometry_type(display1))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map <- map %>% addPolygons(data=display1,  fillColor='#EE6363', stroke=F, fill = T, fillOpacity = 0.8, group=rv$display1_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map <- map %>% addPolylines(data = display1, color = '#EE6363', weight = 2, group = rv$display1_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map <- map %>% addCircleMarkers(data = display1, color = '#EE6363', radius = 5, fillOpacity =1, group = rv$display1_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }
      
      add_names_new <- c(rv$display1_name(), add_names_new)
    } 
    
    if (isMappable(display2_sf())) { 
      display2 <- st_transform(display2_sf(), 4326)
      geom_type <- unique(sf::st_geometry_type(display2))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map <- map %>% addPolygons(data=display2,  fillColor='#330066', stroke=F, fill = T, fillOpacity = 0.7, group=rv$display2_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map <- map %>% addPolylines(data = display2, color = '#330066', weight = 2, group = rv$display2_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map <- map %>% addCircleMarkers(data = display2, color = '#330066', radius = 5, fillOpacity = 0.7, group = rv$display2_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }
      
      add_names_new <- c(rv$display2_name(), add_names_new)
    } 
    if (isMappable(display3_sf())) { 
      display3 <- st_transform(display3_sf(), 4326)
      geom_type <- unique(sf::st_geometry_type(display3))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map <- map %>% addPolygons(data=display3,  fillColor='#003333', stroke=F, fill = T, fillOpacity = 0.7, group=rv$display3_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map <- map %>% addPolylines(data = display3, color = '#003333', weight = 2, group = rv$display3_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map <- map %>% addCircleMarkers(data = display3, color = '#003333', radius = 5, fillOpacity = 0.7, group = rv$display3_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }
      add_names_new <- c(rv$display3_name(), add_names_new)
    } 
    
    if (!is.null(rast1())) { 
      rast1 <- raster::projectRaster(rast1(), crs = "EPSG:4326")
      rv$rast1(rast1)
      values_rast <- raster::values(rast1)
      values_rast <- values_rast[!is.na(values_rast)]  # remove NA values
      
      pal <- colorNumeric(palette = viridis::viridis(256), domain = values_rast, na.color = "transparent")
      map <- map %>% addRasterImage(rast1,  colors = pal, opacity = 0.5, group=rv$rast1_name(), options = leafletOptions(pane = "ground"))
      
      add_names_new <- c(rv$rast1_name(), add_names_new)
    }
    
    if (!is.null(rast2())) { 
      rast2 <- raster::projectRaster(rast2(), crs = "EPSG:4326")
      rv$rast2(rast2)
      values_rast <- raster::values(rast2)
      values_rast <- values_rast[!is.na(values_rast)]  # remove NA values
      
      pal <- colorNumeric(palette = colorRampPalette(c("#0000FF", "#FFFF00", "#FF0000"))(256), domain = values_rast, na.color = "transparent")
      map <- map %>% addRasterImage(rast2,  colors = pal, opacity = 0.5, group=rv$rast2_name(), options = leafletOptions(pane = "ground"))
      
      add_names_new <- c(rv$rast2_name(), add_names_new)
    }
    
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Studyarea", rv$dist_names(), add_names_new),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c(add_names_new))
    
    rv$add_names(add_names_new)
  })  
}  
  
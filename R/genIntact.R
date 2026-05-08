genIntactServer <- function(input, output, session, project, map, rv){
  
  ################################################################################################
  # Observe on buffer 
  observe({
    req(input$selectBuffer)
    if (input$selectBuffer=='custom_buffers') {
      updateTabsetPanel(getDefaultReactiveDomain(), "one", selected = "Custom buffers")
    } else {
      updateTabsetPanel(getDefaultReactiveDomain(), "one", selected = "Mapview")
    }
  })
  
  # Observe on tabset when generating footprint
  observeEvent(input$goButton,{
    updateTabsetPanel(getDefaultReactiveDomain(), "one", selected = "Mapview")
  })
  
  # Reactive for the attributes updated by `goButton`
  observeEvent(input$goButton, {
    aoi <- sum(st_area(rv$sa()))
    if (!is.null(footprintfire_sf())) {
      footprint <- st_union(footprintfire_sf()) 
    }else if (!is.null(footprint_sf())){
      footprint <- st_union(footprint_sf())
    } else{
      footprint <- NULL
    }
    
    rv$additionalAttributes(tibble(
      Attribute = c(
        "Undisturbed areas (%)",
        "Disturbed areas (%)"
      ),
      Value = c(
        if (is.null(intactness_sf())) 0 else as.numeric(round(st_area(st_union(intactness_sf())) / aoi * 100, 1)),
        if (is.null(footprint)) 0 else as.numeric(round(st_area(st_union(footprint)) / aoi * 100, 1))
      )
    ))
  })
  
  observe({
    req(input$selectInput)
    req(rv$layers_rv$fires)
    req(input$forcefire)
    
    # Update max upstream slider
    max_fire <- round(max(rv$layers_rv$fires$area_ha, na.rm = TRUE), -2)
    # Update the slider input with the max value
    updateSliderInput(session = getDefaultReactiveDomain(), inputId = "firesize", max = max_fire)
    
    # Update max upstream slider
    minyear <- min(rv$layers_rv$fires$YEAR, na.rm = TRUE)
    maxyear <- max(rv$layers_rv$fires$YEAR, na.rm = TRUE)
    # Update the slider input with the max value
    updateSliderInput(session = getDefaultReactiveDomain(), inputId = "fireyear", min = minyear , max = maxyear)
  })  
  
  ##############################################################################
  # Update fires
  ##############################################################################
  fires <- reactive({
    if(!is.null(rv$layers_rv$fires)){
      fire <- rv$layers_rv$fires %>%
        dplyr::filter(YEAR >= input$fireyear[1] & YEAR <= input$fireyear[2])
      rv$statslayers_rv$fires <- fire
      return(fire)
    }else{
      return(NULL)
    }
  }) 
  
  ##############################################################################
  # Buffer disturbances and calculate footprint and intactness
  ##############################################################################
  footprint_sf <- reactive({
    
    if(input$confSA[1]==0){
      showModal(modalDialog(
        title = "Missing source dataset confirmation",
        "Before proceeding, please confirm the source dataset in the Select study area section.",
        easyClose = TRUE,
        footer = NULL)
      )
    }
    
    req(input$confSA)
    
    if (!is.null(rv$layers_rv$poly) | !is.null(rv$layers_rv$line) | !is.null(fires()) | !is.null(rv$layers_rv$mines_all)  | !is.null(rv$other_linedist()) | !is.null(rv$other_polydist())) {
      if (!is.null(rv$layers_rv$poly) | !is.null(rv$layers_rv$line) ) {
        if(input$createMatrix) {
          industry_line <- input$lineindustry
          disttype_line <- input$linedisttype
          industry_poly <- input$polyindustry
          disttype_poly <- input$polydisttype
        }else{
          industry_line <- "TYPE_INDUSTRY"
          disttype_line <- "TYPE_DISTURBANCE"
          industry_poly <- "TYPE_INDUSTRY"
          disttype_poly <- "TYPE_DISTURBANCE"
        }
        
        if (is.null(input$selectBuffer) && (!is.null(rv$layers_rv$line) || !is.null(rv$layers_rv$poly))){
          showModal(modalDialog(
            title = "Missing buffer type confirmation",
            "Before proceeding, please confirm how disturbance layers will be buffered.",
            easyClose = TRUE,
            footer = NULL)
          )
          v1<- NULL
          v2 <- NULL
          
        } else if (input$selectBuffer== "custom_buffers") {
          if (!is.null(rv$layers_rv$line) && nrow(rv$layers_rv$line) > 0) {
            m1sub <- as_tibble(input$linear_buffers) %>% dplyr::select(any_of(c("TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE_M"))) %>%      
              mutate(BUFFER_SIZE_M=as.integer(BUFFER_SIZE_M))
            line <- rv$layers_rv$line %>%
              mutate(
                TYPE_INDUSTRY = if (!is.null(industry_line) && industry_line %in% colnames(.)) .[[industry_line]] else "NONE",
                TYPE_DISTURBANCE = if (!is.null(disttype_line) && disttype_line %in% colnames(.)) .[[disttype_line]] else "NONE"
              ) %>%
              left_join(m1sub, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE")) %>% 
              filter(!is.na(BUFFER_SIZE_M))
            v1 <- st_union(st_buffer(line, line$BUFFER_SIZE_M))
          } else { v1 <- NULL}
          if (!is.null(rv$layers_rv$poly) && nrow(rv$layers_rv$poly) > 0) {
            m2sub <- as_tibble(input$areal_buffers) %>% dplyr::select(any_of(c("TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE_M"))) %>% 
              mutate(BUFFER_SIZE_M=as.integer(BUFFER_SIZE_M))
            poly <- rv$layers_rv$poly %>%
              mutate(
                TYPE_INDUSTRY = if (!is.null(industry_line) && industry_line %in% colnames(.)) .[[industry_line]] else "NONE",
                TYPE_DISTURBANCE = if (!is.null(disttype_line) && disttype_line %in% colnames(.)) .[[disttype_line]] else "NONE"
              ) %>%
              left_join(m2sub, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE")) %>% 
              filter(!is.na(BUFFER_SIZE_M))
            
            v2 <- st_union(st_buffer(poly, poly$BUFFER_SIZE_M))
          } else { v2 <- NULL}
        }else {
          if (!is.null(rv$layers_rv$line) && nrow(rv$layers_rv$line) > 0) {
            v1 <- st_union(st_buffer(rv$layers_rv$line, input$buffer1)) %>%
              st_sf()
          } else { v1 <- NULL}
          if (!is.null(rv$layers_rv$poly) && nrow(rv$layers_rv$poly) > 0) {
            v2 <- st_union(st_buffer(rv$layers_rv$poly, input$buffer2)) %>%
              st_sf()
          } else { v2 <- NULL}
        } 
      } else{
        v1<- NULL
        v2 <- NULL
      }
      
      if(input$includeOthers) {
        if(!is.null(rv$other_linedist())){
          v3 <- st_union(st_buffer(rv$other_linedist(), input$otherlinesize)) %>% 
            st_sf()
        }else { v3 <- NULL}
        
        if(!is.null(rv$other_polydist())){
          v4 <- st_union(st_buffer(rv$other_polydist(), input$otherpolysize)) %>% 
            st_sf()
        } else { v4 <- NULL}
      } else { 
        v3 <- NULL
        v4 <- NULL
      }
      
      if(input$forceclaims & ('Quartz_Claims' %in% rv$lyr() | 'Placer_Claims' %in% rv$lyr() | 'Mining_Claims' %in% rv$lyr())) {
        if ('Quartz_Claims' %in% rv$lyr() & !'Placer_Claims' %in% rv$lyr()) {
          v5 <- st_union(st_buffer(rv$layers_rv$quartz, input$minesize)) %>% 
            st_sf()
        } else if ('Placer_Claims' %in% rv$lyr() & !'Quartz_Claims' %in% rv$lyr()) {
          v5 <- st_union(st_buffer(rv$layers_rv$placers, input$minesize)) %>% 
            st_sf()
        } else if ('Mining_Claims' %in% rv$lyr()) {
          v5 <- st_union(st_buffer(rv$layers_rv$mines, input$minesize)) %>% 
            st_sf()
        } else {
          v5a <- st_union(st_buffer(rv$layers_rv$placers, input$minesize)) %>% 
            st_sf()
          v5b <- st_union(st_buffer(rv$layers_rv$quartz, input$minesize)) %>% 
            st_sf()
          v5 <- st_union(v5a, v5b)
        }
      } else { v5 <- NULL}
      v_list <- list(v1, v2, v3, v4, v5)
      v_valid <- Filter(Negate(is.null), v_list)
      if (length(v_valid) > 0) {
        v_combined <- do.call(c, lapply(v_valid, st_geometry))
        v_combined_sf <- st_sf(geometry = v_combined)
        v_union <- st_union(v_combined_sf)
        v <- st_intersection(v_union, rv$sa()) %>% 
          st_sf()
      } else {
        v <- NULL
      }
      return(v)
    }
  })
  
  footprintfire_sf <- reactive({
    if(input$forcefire & 'fires' %in% rv$lyr()) {
      fires_sf <- rv$statslayers_rv$fires %>%
        dplyr::filter(area_ha > input$firesize)
      v6 <- st_union(rv$statslayers_rv$fires) %>% 
        st_sf()
      if(!is.null(footprint_sf())){
        v_union <- st_union(footprint_sf(), v6) %>% 
          st_sf()
      }else{
        v_union <- v6
      }
      v <- st_intersection(v_union, rv$sa())
    } else { v <- NULL}
    rv$footprintfire_sf(v)
    return(v)
  })
  
  intactness_sf <- reactive({
    req(rv$sa())
    
    footprint_names_init <- c()
    if (input$forcefire &  !is.null(footprintfire_sf())) {
      if(is.null(nrow(footprintfire_sf()))){
        ifl <- rv$sa()
        footprint_names_init <- "No disturbed areas"
      }else{
        ifl <- st_difference(rv$sa(), footprintfire_sf()) %>%
          dplyr::select(any_of(c("geom", "geometry")))
        footprint_names_init <- "Disturbed areas (human + fires)"
      }
    } else {
      if(nrow(footprint_sf())==0){
        ifl <- rv$sa()
        footprint_names_init <- "No disturbed areas"
      }else{
        ifl <- st_difference(rv$sa(), footprint_sf()) %>%
          dplyr::select(any_of(c("geom", "geometry")))
        footprint_names_init <- "Disturbed areas (human)"
      }
    }
    
    rv$footprint_names_new(footprint_names_init)
    x <- suppressWarnings(st_cast(ifl, "POLYGON"))
    x <- mutate(x, area_km2=as.numeric(st_area(x)/1000000))
    y <- filter(x, area_km2 > input$area1)
    rv$intactness_sf(y)
    return(y)
  })
  
  ##############################################################################
  # Update map with intactness/footprint
  ##############################################################################
  observeEvent(input$goButton,{
    
    shinyjs::show("save_stats")
    shinyjs::show("capture_map")
    shinyjs::enable("downloadData")
    
    if(input$confSA[1]==0){
      showModal(modalDialog(
        title = "Missing source dataset confirmation",
        "Before proceeding, please confirm the source dataset in the Select study area section.",
        easyClose = TRUE,
        footer = NULL)
      )
    }
    
    req(input$confSA)
    
    if(is.null(input$selectBuffer) && (!is.null(rv$layers_rv$line) || !is.null(rv$layers_rv$poly))){
      showModal(modalDialog(
        title = "Missing buffer type confirmation",
        "Before proceeding, please confirm how disturbance layers will be buffered.",
        easyClose = TRUE,
        footer = NULL)
      )
    }
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Generating footprint and intactness map. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    if(is.null(footprintfire_sf())){
      if(is.null(footprint_sf())){
        showModal(modalDialog(
          title = "No disturbance found inside the studyarea",
          easyClose = FALSE,
          footer = modalButton("OK")
        ))
        fp_sf <- NULL
      }else{
        fp_sf <- footprint_sf() %>% 
          st_as_sf() %>%
          st_transform(4326)
      }  
    }else{
      fp_sf <- footprintfire_sf() %>% 
        st_as_sf() %>%
        st_transform(4326) 
    }
    
    intact_sf <- st_transform(rv$intactness_sf(), 4326)
    
    map <- leafletProxy("map") %>%
      clearGroup('Undisturbed areas') %>%
      clearGroup(rv$footprint_names_old())
    
    if (isMappable(intact_sf)) { 
      map <- map %>% addPolygons(data=intact_sf, color='#336633', stroke=F, fillOpacity=0.5, group='Undisturbed areas', options = leafletOptions(pane = "top")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Studyarea", "Undisturbed areas", rv$dist_names(), rv$add_names()),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        showGroup("Undisturbed areas") %>%
        hideGroup(c(rv$footprint_names_new(), rv$group_names(), rv$add_names()))
    }  
    if (isMappable(fp_sf)) { 
      map <- map %>% addPolygons(data=fp_sf, color='black', stroke=F, fillOpacity=0.5, group=rv$footprint_names_new(), options = leafletOptions(pane = "top")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Studyarea", "Undisturbed areas",  rv$footprint_names_new(), rv$dist_names(), rv$add_names()),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c(rv$footprint_names_new(), rv$group_names(), rv$add_names()))
    } 
    
    # Close the modal once processing is done
    rv$footprint_names_old(rv$footprint_names_new())
    removeModal()

  })
  observeEvent(input$save_stats,{
    req(rv$additionalAttributes()) 
    
    aoi <- rv$aoiAttributes()
    base <- rv$baseAttributes()
    
    # Add additional attributes if the button has been pressed
    additional <- rv$additionalAttributes()
    if (!is.null(additional)) {
      all <- bind_rows(aoi, additional, base)
    } else{
      all <- bind_rows(aoi, base)
    }
    all_wide <- all %>%
      tidyr::pivot_wider(names_from = Attribute, values_from = Value)
    
    # set name
    all_wide$name <- "Studyarea"
    
    
    # set_custom
    if(is.null(rv$statslayers_rv$line) && is.null(rv$statslayers_rv$poly)){
      all_wide$set_custom <- "NA"
    } else if (is.null(rv$statslayers_rv$line) && !is.null(rv$statslayers_rv$poly)){
      all_wide$set_custom <- ifelse(input$selectBuffer=="custom_buffers", "TRUE", paste0("FALSE_line0_poly", as.character(input$buffer2)))
    } else if (!is.null(rv$statslayers_rv$line) && is.null(rv$statslayers_rv$poly)){
      all_wide$set_custom <- ifelse(input$selectBuffer=="custom_buffers", "TRUE", paste0("FALSE_line", as.character(input$buffer1),"_poly0"))
    } else{
      all_wide$set_custom <- ifelse(input$selectBuffer=="custom_buffers", "TRUE", paste0("FALSE_line", as.character(input$buffer1),"_poly", as.character(input$buffer2)))
    }
    
    # include_others
    if(input$includeOthers){
      
      if(is.null(rv$other_linedist()) && is.null(rv$other_polydist())){
        all_wide$set_includeOthers <- "FALSE"
      } else if (is.null(rv$other_linedist()) && !is.null(rv$other_polydist())){
        all_wide$set_includeOthers <- paste0("TRUE_otherline0_otherpoly", as.character(input$otherpolysize))
      } else if (!is.null(rv$other_linedist()) && is.null(rv$other_polydist())){
        all_wide$set_includeOthers <- paste0("TRUE_otherline", as.character(input$otherlinesize),"_otherpoly0")
      } else{
        all_wide$set_includeOthers <- paste0("TRUE_otherline", as.character(input$otherlinesize),"_otherpoly", as.character(input$otherpolysize))
      } 
    } else{
      all_wide$set_includeOthers <- "FALSE"
    }
    
    # include_mines
    if(input$forceclaims){
      all_wide$set_mines <- paste0("TRUE_", as.character(input$minesize))
    } else{
      all_wide$set_mines <- "FALSE"
    }
    
    # include_fires
    if(input$forcefire){
      all_wide$set_fires <- "TRUE"
    } else{
      all_wide$set_fires <- "FALSE"
    }
    
    colnames(all_wide) <- c("Area_km2", "Undisturbed_per", "Disturbed_per", "Lineardist_km", "Arealdist_km2",
                            "otherLinear_km", "otherAreal_km2", "Fires_per", "Mines_per", "PA2021_per", "IntactFL2000_per",
                            "IntactFL2020_per", "Name", "set_custom", "set_includeOthers", "set_mines", "set_fires")
    
    all_wide$signature <- paste(
      all_wide$Area_km2,
      all_wide$Undisturbed_per,
      all_wide$Disturbed_per,
      all_wide$Lineardist_km,
      all_wide$Arealdist_km2,
      all_wide$set_custom,
      all_wide$set_includeOthers,
      all_wide$set_fires,
      all_wide$set_mines,
      sep = "_"
    )
    existing <- rv$summaryStats()$signature
    if (!all_wide$signature %in% existing) {
      updated_tbl <- bind_rows(rv$summaryStats(), all_wide)
    } else {
      updated_tbl <- rv$summaryStats()
    }
    rv$summaryStats(updated_tbl)
  })
  
  
  output$stat_tab <- renderTable({
     rv$summaryStats() %>% dplyr::select(-signature)
  })
  
  
  observeEvent(c(rv$sa(), rv$statslayers_rv$poly, rv$statslayers_rv$line, rv$layers_rv$firesf, rv$layers_rv$mines_all, rv$statslayers_rv$pa2021), {
    rv$additionalAttributes(NULL)
  })
  
  observe({
    rv$visible_groups(input$map_groups)
  })
  ###############################################
  ###############################################
  # Reporting
  ###############################################
  ###############################################
  observeEvent(input$capture_map, {

    showModal(modalDialog(
      title = "Generating the report",
      "Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    # stats
    aoi <- rv$aoiAttributes()
    base <- rv$baseAttributes()
    additional <- rv$additionalAttributes()
    SAStats <- bind_rows(aoi, additional, base)
    
    # Save map
    final_map <- build_map(rv)

    map_file <- normalizePath(file.path("www", "final_map.png"))
    tmap::tmap_save(final_map, map_file, dpi = 300)
    
    legend_file <- normalizePath(file.path("www", "legend_report.png"))
    create_legend_png(
      legend_file,
      rv$visible_groups(),
      rv$display1_sf(),
      rv$display1_name(),
      rv$display2_sf(),
      rv$display2_name(),
      rv$display3_sf(),
      rv$display3_name(),
      rv$rast1_name(),
      rv$rast2_name()
    )
    
    rv$map_path(map_file)
    rv$legend_path(legend_file)
    
    # Parameters to pass to the Rmd
    params <- list(
      stats = SAStats,
      matrix_poly = rv$matrix_poly(),
      matrix_line = rv$matrix_line(),
      map_path = rv$map_path(),
      summaryStats = rv$summaryStats(),
      legend_path = rv$legend_path(),
      includeOthers = input$includeOthers,
      forceclaims = input$forceclaims,
      forcefire = input$forcefire,
      bufferType = input$selectBuffer,
      linearbuff = input$buffer1,
      arealbuff = input$buffer2,
      otherlinesize = input$otherlinesize,
      otherpolysize = input$otherpolysize,
      minebuff = input$minesize,
      firesize = input$firesize,
      fireyear = input$fireyear
    )
                           
    rmarkdown::render(
      input = "report.Rmd",
      output_format = "html_document",
      output_file = "preview.html",
      output_dir = "www",   
      params = params,
      quiet = TRUE,
      envir = new.env(parent = globalenv())
    )
    rv$report_ready(rv$report_ready() + 1)
    removeModal()
  })
  
  
}
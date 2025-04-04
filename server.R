server = function(input, output, session) {
  
  
  ################################################################################################
  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })
  
  ################################################################################################
  # Control overlayGroups
  group_names <- reactiveVal(c())
  
  # Function to add a new group to group_names
  addGroup <- function(new_group) {
    current_groups <- group_names()  # Retrieve the current value
    if (!(new_group %in% current_groups)) {  # Check if the group is not already in the vector
      updated_groups <- c(current_groups, new_group)  # Append the new group
      group_names(updated_groups)  # Update the reactiveVal
    }
  }
  
  removeGroup <- function(group_to_remove) {
    current_groups <- group_names()  # Retrieve the current value
    updated_groups <- current_groups[current_groups != group_to_remove]  # Filter out the specified group
    group_names(updated_groups)  # Update the reactiveVal
  }
  
  ################################################################################################
  # Observe on custom_buffers checkbox
  observe({
    if (input$custom_buffers) {
      # Disable the sliders when custom_buffers is checked
      disable("buffer1")
      disable("buffer2")
      updateTabsetPanel(getDefaultReactiveDomain(), "one", selected = "Custom buffers")
    } else {
      # Enable the sliders when custom_buffers is unchecked
      enable("buffer1")
      enable("buffer2")
      updateTabsetPanel(getDefaultReactiveDomain(), "one", selected = "Mapview")
    }
  })
  
  ##############################################################################
  # Read input data - REQUIRED
  ##############################################################################
  studyarea <- eventReactive({
    # Trigger only when these inputs change
    list(input$selectInput, input$upload_gpkg)
  }, {
    req(input$selectInput)  # Ensure `selectInput` is not NULL
    
    if (input$selectInput == "usedemo") {
      st_read('www/demo.gpkg', 'studyarea', quiet = TRUE)
    } else if (input$selectInput == "usegpkg") {
      # Ensure upload_gpkg is not NULL before reading the file
      req(input$upload_gpkg)
      st_read(input$upload_gpkg$datapath, 'studyarea', quiet = TRUE)
    }
  })
  
  line <- eventReactive(input$selectInput,{
    if (input$selectInput == 'usedemo') {
      # Load the demo dataset
      st_read('www/demo.gpkg', 'linear_disturbance', quiet = TRUE)
    } else if (!is.null(input$upload_gpkg)) {
      # Ensure a file is uploaded before trying to read it
      req(input$upload_gpkg)
      st_read(input$upload_gpkg$datapath, 'linear_disturbance', quiet = TRUE)
    }
  })
  
  poly <- eventReactive(input$selectInput,{
    if (input$selectInput=='usedemo') {
      st_read('www/demo.gpkg', 'areal_disturbance', quiet=T)
    } else if (!is.null(input$upload_gpkg)){
      st_read(input$upload_gpkg$datapath, 'areal_disturbance', quiet = TRUE)
    }
  })

  ################################################################################################
  # Observe mines to disable if NULL
  ################################################################################################
  observe({
    req(input$upload_gpkg)
    if (!is.null(input$upload_gpkg)){
      layers <- st_layers(input$upload_gpkg$datapath)$name
      if (!any(c("Mining_Claims", "Placer_Claims", "Quartz_Claims") %in% layers)){
        disable("forceclaims")
        div(
          style = "color: darkgrey;",
          updateCheckboxInput(session = getDefaultReactiveDomain(), "forceclaims", label = "Include mining claims", value = FALSE)
        )
      }
    }
  })
  
  ################################################################################################
  # Observe fires to disable if NULL
  ################################################################################################
  observe({
    req(input$upload_gpkg)
    if (!is.null(input$upload_gpkg)){
      layers <- st_layers(input$upload_gpkg$datapath)$name
      if (!"fires" %in% layers){
        disable("forcefire")
        div(
          style = "color: darkgrey;",
          updateCheckboxInput(session = getDefaultReactiveDomain(), "forcefire", label = "Include fires", value = FALSE)
        )
      }
    }
  })
  ################################################################################################
  # Set layers choices
  ################################################################################################
  observe({
    req(input$selectInput)
    req(input$upload_gpkg)
    updateSelectInput(session = getDefaultReactiveDomain(), "lineindustry", choices = colnames(line()), selected = "Please select")
    updateSelectInput(session = getDefaultReactiveDomain(), "linedisttype", choices = colnames(line()), selected = "Please select")
    updateSelectInput(session = getDefaultReactiveDomain(), "polyindustry", choices = colnames(poly()), selected = "Please select")
    updateSelectInput(session = getDefaultReactiveDomain(), "polydisttype", choices = colnames(poly()), selected = "Please select")
  })
  
  ################################################################################################
  # Update fire
  ################################################################################################
  observe({
    req(input$selectInput)
    req(fires())
    req(input$forcefire)
    
    # Update max upstream slider
    max_fire <- round(max(fires()$area_ha, na.rm = TRUE), -2)
    # Update the slider input with the max value
    updateSliderInput(session = getDefaultReactiveDomain(), inputId = "firesize", max = max_fire)
  })  
  ##############################################################################
  # Read input data - OPTIONAL
  ##############################################################################
  fires <- reactive({
    if (input$selectInput=='usedemo') {
      fi <-st_read('www/demo.gpkg', 'fires', quiet=T) %>%
        st_cast('MULTIPOLYGON') %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid() %>%
        mutate(area_ha = as.numeric(st_area(geom)/10000))
      addGroup("Fires")
      return(fi)
    } else if (!is.null(input$upload_gpkg)){
      layers <- st_layers(input$upload_gpkg$datapath)$name
      # Check if "fires" layer exists
      if ("fires" %in% layers) {
        # Read the "fires" layer from the uploaded file if it exists
        fi <-st_read(input$upload_gpkg$datapath, 'fires', quiet = TRUE) %>%
          st_cast('MULTIPOLYGON') %>% 
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid() %>%
          mutate(area_ha = as.numeric(st_area(geom)/10000))
        addGroup("Fires")
        return(fi)
      } else {
        removeGroup("Fires")
        return(NULL)  # or display a message, warning, etc.
      }
    }else{
      NULL
    }
  })
  
  ifl2000 <- reactive({
    if (input$selectInput=='usedemo') {
      la <-st_read('www/demo.gpkg', 'intactness_2000', quiet=T)
      addGroup("Intact FL 2000")
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      layers <- st_layers(input$upload_gpkg$datapath)$name
      # Check if "fires" layer exists
      if ("intactness_2000" %in% layers) {
        # Read the "fires" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'intactness_2000', quiet = TRUE)
        addGroup("Intact FL 2000")
        return(la)
      } else {
        # Optionally, handle the case where the "intactness_2000" layer is missing
        removeGroup("Intact FL 2000")
        return(NULL)  # or display a message, warning, etc.
      }
    }else{
      NULL
    }
  })
  
  ifl2020 <- reactive({
    if (input$selectInput=='usedemo') {
      la <-st_read('www/demo.gpkg', 'intactness_2020', quiet=T)
      addGroup("Intact FL 2020")
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      layers <- st_layers(input$upload_gpkg$datapath)$name
      
      # Check if "fires" layer exists
      if ("intactness_2020" %in% layers) {
        # Read the "fires" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'intactness_2020', quiet = TRUE)
        addGroup("Intact FL 2020")
        return(la)
      } else {
        # Optionally, handle the case where the "intactness_2020" layer is missing
        removeGroup("Intact FL 2020")
        return(NULL)  # or display a message, warning, etc.
      }
    }else{
      NULL
    }
  })
  
  pa2021 <- reactive({
    if (input$selectInput=='usedemo') {
      la <-st_read('www/demo.gpkg', 'protected_areas', quiet=T)
      addGroup("Protected areas")
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      layers <- st_layers(input$upload_gpkg$datapath)$name
      
      # Check if "protected_areas" layer exists
      if ("protected_areas" %in% layers) {
        # Read the "fires" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'protected_areas', quiet = TRUE)
        addGroup("Protected areas")
        return(la)
      } else {
        # Optionally, handle the case where the "fires" layer is missing
        removeGroup("Protected areas")
        return(NULL)  # or display a message, warning, etc.
      }
    }
  })
  
  placers <- reactive({
    if (input$selectInput=='usedemo') {
      la <-st_read('www/demo.gpkg', 'Placer_Claims', quiet=T)
      addGroup("Placer Claims")
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      
      layers <- st_layers(input$upload_gpkg$datapath)$name
      # Check if "fires" layer exists
      if ("Placer_Claims" %in% layers) {
        # Read the "fires" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'Placer_Claims', quiet = TRUE)
        addGroup("Placer Claims")
        return(la)
      } else {
        # Optionally, handle the case where the "Placer_Claims" layer is missing
        removeGroup("Placer Claims")
        return(NULL)  # or display a message, warning, etc.
      }
    }
  })
  
  quartz <- reactive({
    if (input$selectInput=='usedemo') {
      la <-st_read('www/demo.gpkg', 'Quartz_Claims', quiet=T)
      addGroup("Quartz Claims")
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      layers <- st_layers(input$upload_gpkg$datapath)$name
      # Check if "fires" layer exists
      if ("Quartz_Claims" %in% layers) {
        # Read the "fires" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'Quartz_Claims', quiet = TRUE)
        addGroup("Quartz Claims")
        return(la)
      } else {
        # Optionally, handle the case where the "Quartz_Claims" layer is missing
        removeGroup("Quartz Claims")
        return(NULL)  # or display a message, warning, etc.
      }
    }
  })
  
  mines <- reactive({
    if (input$selectInput=='usedemo') {
      removeGroup("Mining Claims")
      return(NULL)
    } else if (!is.null(input$upload_gpkg)){
      layers <- st_layers(input$upload_gpkg$datapath)$name
      # Check if "fires" layer exists
      if ("Mining_Claims" %in% layers) {
        # Read the "fires" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'Mining_Claims', quiet = TRUE)
        addGroup("Mining Claims")
        return(la)
      } else {
        # Optionally, handle the case where the "Placer_Claims" layer is missing
        removeGroup("Mining Claims")
        #disable("forceclaims")
        return(NULL)  # or display a message, warning, etc.
      }
    }
  })
  
  ##############################################################################
  # Uploaded data
  ##############################################################################
  lyr_names <- eventReactive(input$selectInput, {
    if (input$selectInput=='usedemo') {
      la <- st_layers('www/demo.gpkg')$name
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      file <- input$upload_gpkg$datapath
      ext <- tools::file_ext(file)
      if(ext == "gpkg"){
        la <- st_layers(file)$name
        return(la)
      }else {
        showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
      }
    }
  })
  
  observeEvent(input$distType, {
    
    if(input$selectInput =="usedemo"){
      industry_line <- "TYPE_INDUSTRY"
      disttype_line <- "TYPE_DISTURBANCE"
      industry_poly <- "TYPE_INDUSTRY"
      disttype_poly <- "TYPE_DISTURBANCE"
    }else{
      industry_line <- input$lineindustry
      disttype_line <- input$linedisttype
      industry_poly <- input$polyindustry
      disttype_poly <- input$polydisttype
    }
  
    # Build tibbles for line and poly feature
    line_tibble <- line() %>%
      st_drop_geometry() %>%
      select(any_of(c(industry_line, disttype_line))) %>%
      mutate(TYPE_FEATURE = "Linear",
             BUFFER_SIZE = input$buffer1,
             TYPE_INDUSTRY = if (industry_line %in% colnames(.)) .[[industry_line]] else NA,
             TYPE_DISTURBANCE = if (disttype_line %in% colnames(.)) .[[disttype_line]] else NA
      )
        
    # Summarize by TYPE_DISTURBANCE and TYPE_INDUSTRY to calculate the total length
    line_summary <- line() %>%
      mutate(length_km=st_length(line())/1000,
             TYPE_INDUSTRY = if (industry_line %in% colnames(.)) .[[industry_line]] else NA,
             TYPE_DISTURBANCE = if (disttype_line %in% colnames(.)) .[[disttype_line]] else NA) %>%
      st_drop_geometry() %>%
      group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
      summarize(LENGTH_KM = as.numeric(round(sum(length_km),2))) %>%
      ungroup()
    
    # Join the summary back to the original tibble
    line_tibble <- line_tibble %>%
      left_join(line_summary, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE"))
  
    mline <- unique(line_tibble) %>%
      select(any_of(c("TYPE_FEATURE", "TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE", "LENGTH_KM"))) %>%
      as.matrix()
    updateMatrixInput(session, 'linear_buffers', mline)
    
    # Create a new tibble with data from both layers
    poly_tibble <- poly() %>%
      st_drop_geometry() %>%
      select(any_of(c(industry_poly, disttype_poly))) %>%
      mutate(TYPE_FEATURE = "Areal",
             BUFFER_SIZE = input$buffer2,
             TYPE_INDUSTRY = if (industry_poly %in% colnames(.)) .[[industry_poly]] else NA,
             TYPE_DISTURBANCE = if (disttype_poly %in% colnames(.)) .[[disttype_poly]] else NA
      )
    # Summarize by TYPE_DISTURBANCE and TYPE_INDUSTRY to calculate the total length
    poly_summary <- poly() %>%
      mutate(area_km2=st_area(poly())/1000000,
             TYPE_INDUSTRY = if (industry_poly %in% colnames(.)) .[[industry_poly]] else NA,
             TYPE_DISTURBANCE = if (disttype_poly %in% colnames(.)) .[[disttype_poly]] else NA) %>%
      st_drop_geometry() %>%
      group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
      summarize(AREA_KM2 = round(as.numeric(sum(area_km2)),2)) %>%
      ungroup()
    
    # Join the summary back to the original tibble
    poly_tibble <- poly_tibble %>%
      left_join(poly_summary, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE"))
    
    mpoly <- unique(poly_tibble) %>%
      select(any_of(c("TYPE_FEATURE", "TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE", "AREA_KM2"))) %>%
      as.matrix()
    updateMatrixInput(session, 'areal_buffers', mpoly)
  
  })
  
  ##############################################################################
  # Buffer disturbances and calculate footprint and intactness
  ##############################################################################
  footprint_sf <- reactive({
    if (nrow(poly())>0 | nrow(line())>0) {
      aoi <- studyarea()
      if (input$custom_buffers==TRUE) {
        if (nrow(line())>0) {
          m1sub <- as_tibble(input$linear_buffers) %>% select(any_of(c("TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE"))) %>%      
            mutate(BUFFER_SIZE=as.integer(BUFFER_SIZE))
          line <- left_join(line(), m1sub) %>% filter(!is.na(BUFFER_SIZE))
          v1 <- st_union(st_buffer(line, line$BUFFER_SIZE))
        }
        if (nrow(poly())>0) {
          m2sub <- as_tibble(input$areal_buffers) %>% select(any_of(c("TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE"))) %>% 
            mutate(BUFFER_SIZE=as.integer(BUFFER_SIZE))
          poly <- left_join(poly(), m2sub) %>% filter(!is.na(BUFFER_SIZE))
          v2 <- st_union(st_buffer(poly, poly$BUFFER_SIZE))
        }
      } else {
        if (nrow(line())>0) {
          v1 <- st_union(st_buffer(line(), input$buffer1)) %>%
            st_sf()
        }
        if (nrow(poly())>0) {
          v2 <- st_union(st_buffer(poly(), input$buffer2)) %>%
            st_sf()
        }
      }
      
      if(input$forceclaims & ('Quartz_Claims' %in% lyr_names() | 'Placer_Claims' %in% lyr_names() | 'Mining_Claims' %in% lyr_names())) {
        
        if ('Quartz_Claims' %in% lyr_names() & !'Placer_Claims' %in% lyr_names()) {
          v3 <- st_union(st_buffer(quartz(), input$minesize)) %>% 
            st_sf()
        } else if ('Placer_Claims' %in% lyr_names() & !'Quartz_Claims' %in% lyr_names()) {
          v3 <- st_union(st_buffer(placers(), input$minesize)) %>% 
            st_sf()
        } else if ('Mining_Claims' %in% lyr_names()) {
          v3 <- st_union(st_buffer(mines(), input$minesize)) %>% 
            st_sf()
        } else {
          v3a <- st_union(st_buffer(placers(), input$minesize)) %>% 
            st_sf()
          v3b <- st_union(st_buffer(quartz(), input$minesize)) %>% 
            st_sf()
          v3 <- st_union(v3a, v3b)
        }
        v <- st_intersection(st_union(st_union(v1, v2),v3), aoi)
      } else {
        if (nrow(line())>0 & nrow(poly())==0) {
          v <- st_intersection(st_union(v1), aoi)
        } else if (nrow(poly())>0 & nrow(line())==0) {
          v <- st_intersection(st_union(v2), aoi)
        } else if (nrow(poly())>0 & nrow(line())>=0) {
          v <- st_intersection(st_union(v1, v2), aoi)
        } 
      }
      if(input$forcefire & 'fires' %in% lyr_names()) {
        fires_sf <- fires() %>%
          dplyr::filter(area_ha > input$firesize)
        
        v4 <- st_union(fires_sf) %>% 
          st_sf()
        v <- st_intersection(st_union(v, v4), aoi)
      }
      return(v)
    }
  })
  
  intactness_sf <- reactive({
    if (nrow(poly())>0 | nrow(line())>0) {
      aoi <- studyarea()
      ifl <- st_difference(aoi, footprint_sf())
      x <- st_cast(ifl, "POLYGON")
      x <- mutate(x, area_km2=as.numeric(st_area(x)/1000000))
      y <- filter(x, area_km2 > input$area1)
    }
  })
  
  ##############################################################################
  # View initial set of maps
  ##############################################################################
  output$map1 <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      fitBounds(lng1 = -121, lat1 = 44, lng2 = -65, lat2 = 78)%>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")%>% 
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                   options = layersControlOptions(collapsed = FALSE))
  })
  
  observe({
    req(input$distType)
    req(studyarea())
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Uploading geopackage layers. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    leafletProxy("map1") %>% 
      clearGroup("Study region") %>%
      clearGroup("Linear disturbances") %>%
      clearGroup("Areal disturbances") %>%
      clearGroup("Intactness") %>%
      clearGroup("Footprint") %>%
      clearGroup("Fires") %>%
      clearGroup("Protected areas") %>%
      clearGroup("Intact FL 2000") %>%
      clearGroup("Intact FL 2020") %>%
      clearGroup("Placer Claims") %>%
      clearGroup("Quartz Claims") %>%
      clearGroup("Mining Claims")

    sa <- st_transform(studyarea(), 4326)
    poly <- st_transform(poly(), 4326)
    line <- st_transform(line(), 4326)
    map_bounds1 <- sa %>% st_bbox() %>% as.character()
    
    leafletProxy("map1") %>%
        addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
        addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
        fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
        addPolygons(data=sa, color='black', fill=F, weight=3, group="Study region") %>%
        addPolylines(data=line, color='red', weight=2, group="Linear disturbances") %>%
        addPolygons(data=poly, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group="Areal disturbances") %>%
        addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Study region", "Linear disturbances", "Areal disturbances"),
                       options = layersControlOptions(collapsed = FALSE)) 
      
    # Optional
    fires <- isolate(fires())
    if(!is.null(fires)){
        fires <- st_transform(fires, 4326)
        leafletProxy("map1") %>% addPolygons(data=fires, fill=T, stroke=F, fillColor='orange', fillOpacity=0.5, group="Fires") 
    }
    ifl2000 <- isolate(ifl2000())
    if(!is.null(ifl2000)){
        ifl2000 <- st_transform(ifl2000, 4326)
        leafletProxy("map1") %>% addPolygons(data=ifl2000, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intact FL 2000") 
    }
    ifl2020 <- isolate(ifl2020())
    if(!is.null(ifl2020)){
        ifl2020 <- st_transform(ifl2020, 4326)
        leafletProxy("map1") %>% addPolygons(data=ifl2020, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intact FL 2020") 
    }
    pa2021 <- isolate(pa2021())
    if(!is.null(pa2021)){
        pa2021 <- st_transform(pa2021, 4326)
        leafletProxy("map1") %>% addPolygons(data=pa2021, fill=T, stroke=F, fillColor='#699999', fillOpacity=0.5,  group="Protected areas") 
    }
    placers <- isolate(placers())
    if(!is.null(placers)){
        placers <- st_transform(placers, 4326)
        leafletProxy("map1") %>% addPolygons(data=placers, color='red', fill=T, weight=1, group="Placer Claims") 
    }
    quartz <- isolate(quartz())
    if(!is.null(quartz)){
        quartz <- st_transform(quartz, 4326)
        leafletProxy("map1") %>% addPolygons(data=quartz, color='red', fill=T, weight=1, group="Quartz Claims") 
    }
    mines <- isolate(mines())
    if(!is.null(mines)){
      mines <- st_transform(mines, 4326)
      leafletProxy("map1") %>% addPolygons(data=mines, color='red', fill=T, weight=1, group="Mining Claims") 
    } 
    leafletProxy("map1") %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Study region", "Linear disturbances", "Areal disturbances", group_names()),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c(group_names()))
    
    # Close the modal once processing is done
    removeModal()
  })
  
  ##############################################################################
  # Update map with intactness/footprint
  ##############################################################################
  #mapFoot <- eventReactive(input$goButton,{
  observeEvent(input$goButton,{
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Generating footprint and intactness map. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    fp_sf <- st_transform(footprint_sf(), 4326)
    intact_sf <- st_transform(intactness_sf(), 4326)

    leafletProxy("map1") %>%
      clearGroup('Intactness') %>%
      clearGroup('Footprint') %>%
      addPolygons(data=intact_sf, color='darkblue', stroke=F, fillOpacity=0.5, group='Intactness') %>%
      addPolygons(data=fp_sf, color='black', stroke=F, fillOpacity=0.5, group='Footprint') %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Study region", "Intactness", "Footprint", "Linear disturbances", "Areal disturbances", group_names()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Footprint", group_names()))
    
    # Close the modal once processing is done
    removeModal()
  })
  
  ##############################################################################
  # Generate statistics table
  ##############################################################################
  # Reactive for the attributes updated by `usedemo` or `upload_gpkg`
  baseAttributes <- reactive({
    req(input$selectInput == "usedemo" || (input$selectInput == "usegpkg" && !is.null(input$upload_gpkg)))
    aoi <- sum(st_area(studyarea()))

    tibble(
      Attribute = c(
        "Area of interest (km2)",
        "Linear disturbances (km)",
        "Areal disturbances (km2)",
        "Fires (%)",
        "Protected areas (%)",
        "Intact FL 2000 (%)",
        "Intact FL 2020 (%)"
      ),
      Value = c(
        as.numeric(round(aoi / 1000000, 0)),
        as.numeric(round(sum(st_length(line())) / 1000, 1)),
        as.numeric(round(sum(st_area(poly())) / 1000000, 1)),
        if (is.null(fires())) 0 else as.numeric(round(sum(st_area(fires())) / aoi * 100, 1)),
        if (is.null(pa2021())) 0 else as.numeric(round(sum(st_area(pa2021())) / aoi * 100, 1)),
        if (is.null(ifl2000())) 0 else as.numeric(round(sum(st_area(ifl2000())) / aoi * 100, 1)),
        if (is.null(ifl2020())) 0 else as.numeric(round(sum(st_area(ifl2020())) / aoi * 100, 1))
      )
    )
  })
  
  # reactiveVal to store additional attributes
  additionalAttributes <- reactiveVal(NULL)
  
  # Reactive for the attributes updated by `goButton`
  observeEvent(input$goButton, {
    req(nrow(poly()) > 0, nrow(line()) > 0)
    
    aoi <- sum(st_area(studyarea()))
    if (is.null(fires())) {
      foot_fires <- footprint_sf()
    }else{
      foot_fires <- st_union(st_union(footprint_sf()), st_union(fires()))
    }
    
    additionalAttributes(tibble(
      Attribute = c(
        "Intactness (%)",
        "Footprint (%)",
        "Footprint + Fires (%)"
      ),
      Value = c(
        as.numeric(round(sum(st_area(intactness_sf())) / aoi * 100, 1)),
        as.numeric(round(sum(st_area(footprint_sf())) / aoi * 100, 1)),
        as.numeric(round(sum(st_area(foot_fires)) / aoi * 100, 1))
      )
    ))
  })
  
  # Reset additional attributes when studyarea or related inputs change
  observeEvent(c(studyarea(), poly(), line(), fires(), pa2021()), {
    additionalAttributes(NULL)
  })
  
  # Combine the two sets of attributes
  output$tab1 <- renderTable({
    req(input$distType)
    req(baseAttributes())  # Ensure `baseAttributes` is ready
    base <- baseAttributes()
    
    # Add additional attributes if the button has been pressed
    additional <- additionalAttributes()
    if (!is.null(additional)) {
      base <- bind_rows(base, additional)
    }
    base
  })
  
  ##############################################################################
  # Save features to a geopackage
  ##############################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("disturbance_explorer-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
        x <- data.frame(Area_km2 = baseAttributes()[1,2],
                        Lineardist_km  = baseAttributes()[2,2],
                        Arealdist_km2 = baseAttributes()[3,2],
                        Fires_per = baseAttributes()[4,2],
                        PA2021_per= baseAttributes()[5,2],
                        IntactFL2000_per = baseAttributes()[6,2],
                        IntactFL2020_per = baseAttributes()[7,2])
        colnames(x) <-c("Area_km2","Lineardist_km","Arealdist_km2","Fires_per", "PA2021_per","IntactFL2000_per","IntactFL2020_per")
        aoi <- cbind(st_union(studyarea()), x)
        st_write(aoi, dsn=file, layer='studyarea')
        st_write(line(), dsn=file, layer='linear_disturbance', append=TRUE)
        st_write(poly(), dsn=file, layer='areal_disturbance', append=TRUE)
        st_write(fires(), dsn=file, layer='fires', append=TRUE)
        st_write(pa2021(), dsn=file, layer='protected_areas', append=TRUE)
        if (input$goButton) {
          x <- data.frame(intactness_per = additionalAttributes()[1,2],
                          footprint_per  = additionalAttributes()[2,2],
                          footfire_per = additionalAttributes()[3,2])
          colnames(x) <-c("footprint_per","intactness_per","footfire_per")
          aoi <- cbind(aoi, x)
          st_write(aoi, dsn=file, layer='studyarea', append = FALSE)
          st_write(footprint_sf(), dsn=file, layer='footprint', append=TRUE)
          st_write(intactness_sf(), dsn=file, layer='intactness', append=TRUE)
        }
    }
  )
}

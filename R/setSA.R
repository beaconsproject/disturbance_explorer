setStudyareaServer <- function(input, output, session, project, map, rv){
  
  ##############################################################################
  # Observe on layers names in gpkg
  sourceData <- reactiveVal()
  
  lyr_names <- reactive({
    file <- NULL
    
    if (isTRUE(input$selectInput == 'usedemo')) {
      file <- 'www/demo.gpkg'
    } else if (!is.null(input$upload_gpkg)) {
      file <- input$upload_gpkg$datapath
      ext <- tools::file_ext(file)
      if (ext != "gpkg") {
        showModal(modalDialog(
          title = "Wrong file type, must be a geopackage (.gpkg)",
          easyClose = TRUE,
          footer = NULL)
        ) 
        return()
      }
    } else {
      return(NULL)
    }
    
    # Extract layer names
    sourceData(file)
    layers <- st_layers(file)$name
    rv$lyr(layers)
    return(layers)
  })
  
  ################################################################################################
  # Observe on selectInput
  ################################################################################################
  observe({
    req(input$upload_gpkg)
    file <- input$upload_gpkg$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "saLayer", choices = c("Select a layer", layers))
  })
  
  ################################################################################################
  # Observe on Others disturbances 
  observe({
    disable("includeOthers")
    div(
      style = "color: darkgrey;",
      updateCheckboxInput(session = getDefaultReactiveDomain(), "includeOthers", label = "Include others disturbances", value = FALSE)
    )
    req(!is.null(input$upload_lineothers) || !is.null(input$upload_polyothers))
    enable("includeOthers")
    enable("confUpload")
  })
 
  observe({
    if(is.null(lyr_names())){
      disable("createMatrix")
    }else{
      req(!is.null(lyr_names())) 
      if(!is.null(rv$layers_rv$line) || !is.null(rv$layers_rv$poly)){
        enable("createMatrix")
      } else {
        disable("createMatrix")
      }
    }
  })
  
  observe({
    disable("otherpolysize")
    req(!is.null(input$upload_polyothers))
    enable("otherpolysize")
  })
  observe({
    disable("otherlinesize")
    req(!is.null(input$upload_lineothers))
    enable("otherlinesize")
  })
  ################################################################################################
  # Observe on Mining claims
  observe({
    req(rv$sa())
    if (is.null(rv$layers_rv$quartz) && is.null(rv$layers_rv$placers) && is.null(rv$layers_rv$mines)) {
      disable("forceclaims")
      div(
        style = "color: darkgrey;",
        updateCheckboxInput(session = getDefaultReactiveDomain(), "forceclaims", label = "Include mining claims", value = FALSE)
      )
    }else{
      enable("forceclaims")
    }
  })
  
  ################################################################################################
  # Observe on Fires (checkboxInput and slider)
  observe({
    req(rv$sa())
    if (is.null(rv$layers_rv$fires)){
      disable("forcefire")
      div(
        style = "color: darkgrey;",
        updateCheckboxInput(session = getDefaultReactiveDomain(), "forcefire", label = "Include fires", value = FALSE)
      )
    }else{
      enable("forcefire")
    }
  })
  
  ################################################################################################
  # Observe on linear disturbances - disable if missing
  observe({
    req(rv$sa())
    if (!("linear_disturbance") %in% lyr_names()){
      disable("buffer1")
    }else{
      enable("buffer1")
    }
  })
  
  ################################################################################################
  # Observe on areal disturbances - disable if missing
  observe({
    req(rv$sa())
    if (!("areal_disturbance") %in% lyr_names()){
      disable("buffer2")
    }else{
      enable("buffer2")
    }
  })
  ################################################################################################
  # Observe on linear and areal for custom buffering - disable if both missing
  observe({
    req(rv$sa())
    if (!any(c("linear_disturbance", "areal_disturbance") %in% lyr_names())){
      disable("selectBuffer")
    }else{
      enable("selectBuffer")
    }
  })
  

  ################################################################################################
  # Render UI for the selection of column name for disturbances 
  ################################################################################################
  observe({
    if(input$createMatrix){
      shinyjs::enable("confClassify")
    } else{
      shinyjs::disable("confClassify")
    }
  })
  
  output$lineIndustryUI <- renderUI({
    req(input$createMatrix == TRUE)
    
    req(rv$layers_rv$line)  # only show if line() is available
    div(
      style = "margin-top: -30px;",  
      selectInput("lineindustry", 
                  label = div(style = "font-size:13px;margin-top: -10px;", ""), 
                  choices = c("--industry type--",colnames(rv$layers_rv$line)), 
                  selected = "--industry type--")
    )
  })
  
  output$lineDistTypeUI <- renderUI({
    req(input$createMatrix == TRUE)
    req(rv$layers_rv$line)
    div(
      style = "margin-top: -30px;",
      selectInput("linedisttype", 
                  label = div(style = "font-size:13px;", ""), 
                  choices = c("--disturbance type--", colnames(rv$layers_rv$line)), 
                  selected = "--disturbance type--")
    )
  })
  
  output$polyIndustryUI <- renderUI({
    req(input$createMatrix == TRUE)
    req(rv$layers_rv$poly)
    div(
      style = "margin-top: -30px;",
      selectInput("polyindustry", 
                  label = div(style = "font-size:13px;", ""), 
                  choices = c("--industry type--",colnames(rv$layers_rv$poly)), 
                  selected = "--industry type--")
    )
  })
  
  output$polyDistTypeUI <- renderUI({
    req(input$createMatrix == TRUE)
    req(rv$layers_rv$poly)
    div(
      style = "margin-top: -30px;",
      selectInput("polydisttype", 
                  label = div(style = "font-size:13px;margin: 0px;", ""), 
                  choices = c("--disturbance type--",colnames(rv$layers_rv$poly)), 
                  selected = "--disturbance type--")
    )
  })
  
  mines_all <- reactive({
    geoms <- list(rv$layers_rv$quartz, rv$layers_rv$placers, rv$layers_rv$mines) |>
      purrr::compact() |>                # removes NULLs
      purrr::map(sf::st_geometry)       # extract just the geometries
    
    # If no layers, return NULL
    if (length(geoms) == 0) return(NULL)
    
    # Combine geometries into one sf object
    li <- sf::st_as_sf(sf::st_union(do.call(c, geoms)))
    rv$layers_rv$mines_all <- li
    return(li)
  })
  
  ########################################################
  output$linear_matrix_ui <- renderUI({
    if (is.null(rv$layers_rv$line)) {
      tags$p("NONE", style = "color: gray; font-style: italic;")
    } else {
      # Build matrix
      industry_line <- rv$industry_line()
      disttype_line <- rv$disttype_line()
      
      line_tibble <- rv$statslayers_rv$line %>%
        st_drop_geometry() %>%
        {
          data <- .
          industry_col <- if (!is.null(industry_line) && industry_line %in% colnames(data)) data[[industry_line]] else "NONE"
          dist_col <- if (!is.null(disttype_line) && disttype_line %in% colnames(data)) data[[disttype_line]] else "NONE"
          
          data %>%
            mutate(
              TYPE_FEATURE = "Linear",
              BUFFER_SIZE_M = input$buffer2,
              TYPE_INDUSTRY = industry_col,
              TYPE_DISTURBANCE = dist_col
            )
        }
      line_summary <- if (is.null(rv$statslayers_rv$line)) {
        tibble(
          TYPE_INDUSTRY = "NONE",
          TYPE_DISTURBANCE = "NONE",
          AREA_KM2 = 0
        )
      } else {
        data <- rv$statslayers_rv$line
        data <- data %>%
          mutate(
            length_km = st_length(rv$statslayers_rv$line) / 1000,
            TYPE_INDUSTRY = if (!is.null(industry_line) && industry_line %in% colnames(data)) data[[industry_line]] else "NONE",
            TYPE_DISTURBANCE = if (!is.null(disttype_line) && disttype_line %in% colnames(data)) data[[disttype_line]] else "NONE"
          ) %>%
          st_drop_geometry() %>%
          group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
          summarize(LENGTH_KM = as.numeric(round(sum(length_km), 2)))
      }
      
      line_tibble <- line_tibble %>%
        left_join(line_summary, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE"))
      
      mline <- unique(line_tibble) %>%
        dplyr::select(any_of(c("TYPE_FEATURE", "TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE_M", "LENGTH_KM"))) %>%
        as.matrix()
      
      rv$matrix_line(mline)
      
      # Return the matrixInput to UI
      matrixInput("linear_buffers",
                  value = rv$matrix_line(),
                  rows = list(names = FALSE, extend = TRUE),
                  cols = list(names = TRUE))
    }
  })  
  
  output$areal_matrix_ui <- renderUI({
    if (is.null(rv$statslayers_rv$poly)) {
      tags$p("NONE", style = "color: gray; font-style: italic;")
    } else {
      #req(industry_poly, disttype_poly, input$buffer2)
      industry_poly <- rv$industry_poly()
      disttype_poly <- rv$disttype_poly()
      
      poly_tibble <- rv$statslayers_rv$poly %>%
        st_drop_geometry() %>%
        {
          data <- .
          industry_col <- if (!is.null(industry_poly) && industry_poly %in% colnames(data)) data[[industry_poly]] else "NONE"
          dist_col <- if (!is.null(disttype_poly) && disttype_poly %in% colnames(data)) data[[disttype_poly]] else "NONE"
          
          data %>%
            mutate(
              TYPE_FEATURE = "Areal",
              BUFFER_SIZE_M = input$buffer2,
              TYPE_INDUSTRY = industry_col,
              TYPE_DISTURBANCE = dist_col
            )
        }
      
      poly_summary <- if (is.null(rv$statslayers_rv$poly)) {
        tibble(
          TYPE_INDUSTRY = "NONE",
          TYPE_DISTURBANCE = "NONE",
          AREA_KM2 = 0
        )
      } else {
        data <- rv$statslayers_rv$poly
        data <- data %>%
          mutate(
            area_km2 = as.numeric(st_area(data)) / 1e6,
            TYPE_INDUSTRY = if (!is.null(industry_poly) && industry_poly %in% colnames(data)) data[[industry_poly]] else "NONE",
            TYPE_DISTURBANCE = if (!is.null(disttype_poly) && disttype_poly %in% colnames(data)) data[[disttype_poly]] else "NONE"
          ) %>%
          st_drop_geometry() %>%
          group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
          summarize(AREA_KM2 = round(sum(area_km2, na.rm = TRUE), 5), .groups = "drop")
      }
      
      poly_tibble <- poly_tibble %>%
        left_join(poly_summary, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE"))
      
      mpoly <- unique(poly_tibble) %>%
        dplyr::select(any_of(c("TYPE_FEATURE", "TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE_M", "AREA_KM2"))) %>%
        as.matrix()
      
      rv$matrix_poly(mpoly)
      matrixInput("areal_buffers",
                  value = mpoly,
                  rows = list(names = FALSE, extend = TRUE),
                  cols = list(names = TRUE))
    }
  })    
  
  
  ##############################################################################
  # Read input data - REQUIRED
  ##############################################################################
  studyarea <- reactive({
    la <- NULL
  
    # Trigger only when these inputs change
    if(!is.null(sourceData())){
      if (input$selectInput == "usedemo") {
        la <- st_read('www/demo.gpkg', 'studyarea', quiet = TRUE)
      } else if (input$selectInput == "usegpkg") {
        req(input$upload_gpkg)
        
        if(input$saLayer != "Select a layer" && input$saLayer != ""){
          gpkg_path <- file.path(tempdir(), paste0("uploaded_", input$upload_gpkg$name))
          file.copy(input$upload_gpkg$datapath, gpkg_path, overwrite = TRUE)
          la <- st_read(gpkg_path, input$saLayer, quiet = TRUE)
        }
      }
    }
    rv$sa(la)
    return(la)
  })
  
  observeEvent(input$confSA,{
    req(studyarea())
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Uploading layers. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    shinyjs::hide("save_stats")
    shinyjs::hide("capture_map")
    
    if ("linear_disturbance" %in% lyr_names()) {
      line_sf <- st_read(sourceData(), "linear_disturbance", quiet = TRUE)
      rv$layers_rv$line <- line_sf
      rv$industry_line("TYPE_INDUSTRY")
      rv$disttype_line("TYPE_DISTURBANCE")
      la <- line_sf %>%
        st_intersection(rv$sa()) %>%
        st_make_valid() %>%
        st_collection_extract("LINESTRING") %>%
        st_cast("MULTILINESTRING")
      if(nrow(la)>0){
        rv$statslayers_rv$line <- la
      }else{
        rv$statslayers_rv$line <- NULL
      }
      # Build matrix
      industry_line <- rv$industry_line()
      disttype_line <- rv$disttype_line()
      
      line_tibble <- rv$statslayers_rv$line %>%
        st_drop_geometry() %>%
        {
          data <- .
          industry_col <- if (!is.null(industry_line) && industry_line %in% colnames(data)) data[[industry_line]] else "NONE"
          dist_col <- if (!is.null(disttype_line) && disttype_line %in% colnames(data)) data[[disttype_line]] else "NONE"
          
          data %>%
            mutate(
              TYPE_FEATURE = "Linear",
              BUFFER_SIZE_M = input$buffer2,
              TYPE_INDUSTRY = industry_col,
              TYPE_DISTURBANCE = dist_col
            )
        }
      line_summary <- if (is.null(rv$statslayers_rv$line)) {
        tibble(
          TYPE_INDUSTRY = "NONE",
          TYPE_DISTURBANCE = "NONE",
          AREA_KM2 = 0
        )
      } else {
        data <- rv$statslayers_rv$line
        data <- data %>%
          mutate(
            length_km = st_length(rv$statslayers_rv$line) / 1000,
            TYPE_INDUSTRY = if (!is.null(industry_line) && industry_line %in% colnames(data)) data[[industry_line]] else "NONE",
            TYPE_DISTURBANCE = if (!is.null(disttype_line) && disttype_line %in% colnames(data)) data[[disttype_line]] else "NONE"
          ) %>%
          st_drop_geometry() %>%
          group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
          summarize(LENGTH_KM = as.numeric(round(sum(length_km), 2)))
      }
      
      line_tibble <- line_tibble %>%
        left_join(line_summary, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE"))
      
      mline <- unique(line_tibble) %>%
        dplyr::select(any_of(c("TYPE_FEATURE", "TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE_M", "LENGTH_KM"))) %>%
        as.matrix()
      
      rv$matrix_line(mline)
    }
    
    if ("areal_disturbance" %in% lyr_names()) {
      poly_sf <- st_read(sourceData(), "areal_disturbance", quiet = TRUE)
      rv$layers_rv$poly <- poly_sf
      rv$industry_poly("TYPE_INDUSTRY")
      rv$disttype_poly("TYPE_DISTURBANCE")
      la <- poly_sf %>%
        st_intersection(rv$sa()) %>%
        st_make_valid() %>%
        st_collection_extract("POLYGON") %>%
        st_cast("MULTIPOLYGON")
      if(nrow(la)>0){
        rv$statslayers_rv$poly <- la
      }else{
        rv$statslayers_rv$poly <- NULL
      }
      industry_poly <- rv$industry_poly()
      disttype_poly <- rv$disttype_poly()
      
      poly_tibble <- rv$statslayers_rv$poly %>%
        st_drop_geometry() %>%
        {
          data <- .
          industry_col <- if (!is.null(industry_poly) && industry_poly %in% colnames(data)) data[[industry_poly]] else "NONE"
          dist_col <- if (!is.null(disttype_poly) && disttype_poly %in% colnames(data)) data[[disttype_poly]] else "NONE"
          
          data %>%
            mutate(
              TYPE_FEATURE = "Areal",
              BUFFER_SIZE_M = input$buffer2,
              TYPE_INDUSTRY = industry_col,
              TYPE_DISTURBANCE = dist_col
            )
        }
      
      poly_summary <- if (is.null(rv$statslayers_rv$poly)) {
        tibble(
          TYPE_INDUSTRY = "NONE",
          TYPE_DISTURBANCE = "NONE",
          AREA_KM2 = 0
        )
      } else {
        data <- rv$statslayers_rv$poly
        data <- data %>%
          mutate(
            area_km2 = as.numeric(st_area(data)) / 1e6,
            TYPE_INDUSTRY = if (!is.null(industry_poly) && industry_poly %in% colnames(data)) data[[industry_poly]] else "NONE",
            TYPE_DISTURBANCE = if (!is.null(disttype_poly) && disttype_poly %in% colnames(data)) data[[disttype_poly]] else "NONE"
          ) %>%
          st_drop_geometry() %>%
          group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
          summarize(AREA_KM2 = round(sum(area_km2, na.rm = TRUE), 5), .groups = "drop")
      }
      
      poly_tibble <- poly_tibble %>%
        left_join(poly_summary, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE"))
      
      mpoly <- unique(poly_tibble) %>%
        dplyr::select(any_of(c("TYPE_FEATURE", "TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE_M", "AREA_KM2"))) %>%
        as.matrix()
      
      rv$matrix_poly(mpoly)
    }
    
    if ("fires" %in% lyr_names()) {
      fi <-st_read(sourceData(), 'fires', quiet=T) %>%
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid() %>%
        mutate(area_ha = as.numeric(st_area(.)/10000))
      rv$layers_rv$fires <- fi
      la <- fi %>%
        st_intersection(rv$sa()) %>%
        suppressWarnings() %>%
        st_cast('MULTIPOLYGON') %>% 
        st_make_valid() %>%
        mutate(area_ha = as.numeric(st_area(geom)/10000))
      if(nrow(la)>0){
        rv$statslayers_rv$fires <- la
      }else{
        rv$statslayers_rv$fires <- NULL
      }
    }
    if ("Intact_FL_2000" %in% lyr_names()) {
      la <-st_read(sourceData(), 'Intact_FL_2000', quiet=T)
      rv$layers_rv$ifl2000 <- la
      li <- la %>%
        st_intersection(rv$sa())
      if(nrow(li)>0){
        rv$statslayers_rv$ifl2000 <- li
      }else{
        rv$statslayers_rv$ifl2000 <- NULL
      }
    }
    if ("Intact_FL_2020" %in% lyr_names()) {
      la <-st_read(sourceData(), 'Intact_FL_2020', quiet=T)
      rv$layers_rv$ifl2020 <- la
      li <- la %>%
        st_intersection(rv$sa())
      if(nrow(li)>0){
        rv$statslayers_rv$ifl2020 <- li
      }else{
        rv$statslayers_rv$ifl2020 <- NULL
      }
    }
    if ("protected_areas" %in% lyr_names()) {
      la <-st_read(sourceData(), 'protected_areas', quiet=T)
      rv$layers_rv$pa2021 <- la
      li <- la %>%
        st_intersection(rv$sa())
      if(nrow(li)>0){
        rv$statslayers_rv$pa2021 <- li
      }else{
        rv$statslayers_rv$pa2021 <- NULL
      }
    }
    if ("Placer_Claims" %in% lyr_names()) {
      la <-st_read(sourceData(), 'Placer_Claims', quiet=T)
      rv$layers_rv$placers <- la
      li <- la %>%
        st_intersection(rv$sa())
      if(nrow(li)>0){
        rv$statslayers_rv$placers <- li
      }else{
        rv$statslayers_rv$placers <- NULL
      }
    }
    if ("Quartz_Claims" %in% lyr_names()) {
      la <-st_read(sourceData(), 'Quartz_Claims', quiet=T)
      rv$layers_rv$quartz <- la
      li <- la %>%
        st_intersection(rv$sa())
      if(nrow(li)>0){
        rv$statslayers_rv$quartz <- li
      }else{
        rv$statslayers_rv$quartz <- NULL
      }
    }
    if ("Mining_Claims" %in% lyr_names()) {
      la <-st_read(sourceData(), 'Mining_Claims', quiet=T)
      rv$layers_rv$mines <- la
      li <- la %>%
        st_intersection(rv$sa())
      if(nrow(li)>0){
        rv$statslayers_rv$mines <- li
      }else{
        rv$statslayers_rv$mines <- NULL
      }
    }
    
  })
  
  observeEvent(input$confClassify, {
    if (input$selectInput == "Please select"){
      showModal(modalDialog(
        title = "Missing studyarea",
        "Before proceeding, please select a studyarea.",
        easyClose = TRUE,
        footer = NULL)
      )
    }else{
      req(rv$sa())
      
      if (!is.null(rv$layers_rv$line)) {
        if(input$createMatrix){
          rv$industry_line(input$lineindustry)
          rv$disttype_line(input$linedisttype)
          
          # Build matrix
          industry_line <- rv$industry_line()
          disttype_line <- rv$disttype_line()
            
          line_tibble <- rv$statslayers_rv$line %>%
            st_drop_geometry() %>%
            {
              data <- .
              industry_col <- if (!is.null(industry_line) && industry_line %in% colnames(data)) data[[industry_line]] else "NONE"
              dist_col <- if (!is.null(disttype_line) && disttype_line %in% colnames(data)) data[[disttype_line]] else "NONE"
                
              data %>%
                mutate(
                  TYPE_FEATURE = "Linear",
                  BUFFER_SIZE_M = input$buffer2,
                  TYPE_INDUSTRY = industry_col,
                  TYPE_DISTURBANCE = dist_col
                )
            }
          line_summary <- if (is.null(rv$statslayers_rv$line)) {
            tibble(
              TYPE_INDUSTRY = "NONE",
              TYPE_DISTURBANCE = "NONE",
              AREA_KM2 = 0
            )
          } else {
            data <- rv$statslayers_rv$line
            data <- data %>%
              mutate(
                length_km = st_length(rv$statslayers_rv$line) / 1000,
                TYPE_INDUSTRY = if (!is.null(industry_line) && industry_line %in% colnames(data)) data[[industry_line]] else "NONE",
                TYPE_DISTURBANCE = if (!is.null(disttype_line) && disttype_line %in% colnames(data)) data[[disttype_line]] else "NONE"
              ) %>%
              st_drop_geometry() %>%
              group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
              summarize(LENGTH_KM = as.numeric(round(sum(length_km), 2)))
          }
            
          line_tibble <- line_tibble %>%
            left_join(line_summary, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE"))
            
          mline <- unique(line_tibble) %>%
            dplyr::select(any_of(c("TYPE_FEATURE", "TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE_M", "LENGTH_KM"))) %>%
            as.matrix()
            
          rv$matrix_line(mline)
        }
      }
      if (!is.null(rv$layers_rv$poly)) {
        if(input$createMatrix){
          rv$industry_poly(input$polyindustry)
          rv$disttype_poly(input$polydisttype)
          
          industry_poly <- rv$industry_poly()
          disttype_poly <- rv$disttype_poly()
            
          poly_tibble <- rv$statslayers_rv$poly %>%
            st_drop_geometry() %>%
            {
              data <- .
              industry_col <- if (!is.null(industry_poly) && industry_poly %in% colnames(data)) data[[industry_poly]] else "NONE"
              dist_col <- if (!is.null(disttype_poly) && disttype_poly %in% colnames(data)) data[[disttype_poly]] else "NONE"
                
              data %>%
                mutate(
                  TYPE_FEATURE = "Areal",
                  BUFFER_SIZE_M = input$buffer2,
                  TYPE_INDUSTRY = industry_col,
                  TYPE_DISTURBANCE = dist_col
                )
            }
            
          poly_summary <- if (is.null(rv$statslayers_rv$poly)) {
            tibble(
              TYPE_INDUSTRY = "NONE",
              TYPE_DISTURBANCE = "NONE",
              AREA_KM2 = 0
            )
          } else {
            data <- rv$statslayers_rv$poly
            data <- data %>%
                mutate(
                area_km2 = as.numeric(st_area(data)) / 1e6,
                TPE_INDUSTRY = if (!is.null(industry_poly) && industry_poly %in% colnames(data)) data[[industry_poly]] else "NONE",
                  TYPE_DISTURBANCE = if (!is.null(disttype_poly) && disttype_poly %in% colnames(data)) data[[disttype_poly]] else "NONE"
              ) %>%
              st_drop_geometry() %>%
              group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
              summarize(AREA_KM2 = round(sum(area_km2, na.rm = TRUE), 5), .groups = "drop")
          }
            
          poly_tibble <- poly_tibble %>%
            left_join(poly_summary, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE"))
            
          mpoly <- unique(poly_tibble) %>%
            dplyr::select(any_of(c("TYPE_FEATURE", "TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE_M", "AREA_KM2"))) %>%
            as.matrix()
            
          rv$matrix_poly(mpoly)
      
        }
      }
    }
  })
  ##############################################################################
  # Read input data - OTHERS
  ##############################################################################
  other_linedist <- reactive({
    layer <- FALSE
    
    if(!is.null(input$upload_lineothers)){
      required_extensions <- c("shp", "shx", "dbf", "prj")
      infile <- input$upload_lineothers
      file_extensions <- tools::file_ext(infile$name)
      
      # Check if all required extensions are present
      if (!all(required_extensions %in% file_extensions)) {
        showModal(modalDialog(
          title = "Missing shapefile component for other linear disturbances.",
          "Please upload all required shapefile components: .shp, .shx, .dbf, and .prj.",
          easyClose = FALSE,
          footer = modalButton("OK")
        ))
        req(FALSE)
      }else{
        # Proceed if all components are present
        dir <- unique(dirname(infile$datapath))
        outfiles <- file.path(dir, infile$name)
        name <- tools::file_path_sans_ext(infile$name[1])
        
        purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y))
        layer <- sf::st_read(file.path(dir,paste0(name, ".shp")), quiet = TRUE) %>% 
          dplyr::select(-any_of("fid")) %>%
          sf::st_transform(st_crs(rv$sa())) %>%
          sf::st_intersection(rv$sa())
        
        if(nrow(layer)==0){
          showModal(modalDialog(
            title = "Other linear disturbances selected do not fall within the chosen sudyarea.",
            "The layer won't be uploaded",
            easyClose = FALSE,
           footer = modalButton("OK")
          ))
          return(FALSE)
        }else{
          # Handle cases where 'geometry' might not be named correctly
          if (!"geometry" %in% names(layer) && "geom" %in% names(layer)) {
            layer$geometry <- layer$geom
          }
          
          layer <- layer %>%
            sf::st_set_geometry("geometry") %>%
            sf::st_zm(drop = TRUE, what = "ZM")
        }
      }
    }
    rv$other_linedist(layer)
    return(layer)
  })
  
  other_polydist <- reactive({
    layer <- FALSE
    if(!is.null(input$upload_polyothers)){
      required_extensions <- c("shp", "shx", "dbf", "prj")
      infile <- input$upload_polyothers
      file_extensions <- tools::file_ext(infile$name)
      
      # Check if all required extensions are present
      if (!all(required_extensions %in% file_extensions)) {
        showModal(modalDialog(
          title = "Missing shapefile component for other areal disturbances.",
          "Please upload all required shapefile components: .shp, .shx, .dbf, and .prj.",
          easyClose = FALSE,
          footer = modalButton("OK")
        ))
        req(FALSE)
      } else {
        # Proceed if all components are present
        dir <- unique(dirname(infile$datapath))
        outfiles <- file.path(dir, infile$name)
        name <- tools::file_path_sans_ext(infile$name[1])
        
        purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y))
        layer <- sf::st_read(file.path(dir,paste0(name, ".shp")), quiet = TRUE) %>% 
          dplyr::select(-any_of("fid")) %>%
          sf::st_transform(st_crs(rv$sa())) %>%
          st_intersection(rv$sa())
        
        if(nrow(layer)==0){
          showModal(modalDialog(
            title = "Other areal disturbances selected do not fall within the chosen studyarea.",
            "The layer won't be uploaded",
            easyClose = FALSE,
            footer = modalButton("OK")
          ))
          return(FALSE)
        }else{
          # Handle cases where 'geometry' might not be named correctly
          if (!"geometry" %in% names(layer) && "geom" %in% names(layer)) {
            layer$geometry <- layer$geom
          }
          
          layer <- layer %>%
            sf::st_set_geometry("geometry") %>%
            sf::st_zm(drop = TRUE, what = "ZM")
        }
      }
    }
    rv$other_polydist(layer)
    return(layer)
  })
  
  #################################################################
  ##   LEAFLET
  #################################################################
  observeEvent(input$confSA,{
    req(studyarea())
    
    leafletProxy("map") %>% 
      clearGroup("Linear disturbances") %>%
      clearGroup("Areal disturbances") %>%
      clearGroup("Other linear disturbances") %>%
      clearGroup("Other areal disturbances") %>%
      clearGroup("Undisturbed areas") %>%
      clearGroup("Fires") %>%
      clearGroup("Protected areas") %>%
      clearGroup("Intact FL 2000") %>%
      clearGroup("Intact FL 2020") %>%
      clearGroup("Mining Claims") %>%
      clearGroup("Placer Claims") %>%
      clearGroup("Quartz Claims")
    
    sa <- st_transform(rv$sa(), 4326)
    map_bounds1 <- sa %>% st_bbox() %>% as.character()
    
    dist_names_new <- c(rv$dist_names())
    add_names_new <- c(rv$add_names())
    
 
    leafletProxy("map") %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=sa, color='#663399', fill=F, opacity = 1, weight=3, group="Studyarea", options = leafletOptions(pane = "top")) 

    #Mining
    placers <- isolate(rv$layers_rv$placers)
    if(!is.null(placers)){
      placers <- st_transform(placers, 4326)
      leafletProxy("map") %>% addPolygons(data=placers, color= '#333333', fill=T, fillColor='#333333', weight=3, fillOpacity = 1, group="Placer Claims", options = leafletOptions(pane = "ground")) 
      dist_names_new <- c(dist_names_new, "Placer Claims")
    }
    quartz <- isolate(rv$layers_rv$quartz)
    if(!is.null(quartz)){
      quartz <- st_transform(quartz, 4326)
      leafletProxy("map") %>% addPolygons(data=quartz, color = '#999999', fill=T, fillColor='#999999', weight=1, fillOpacity = 1, group="Quartz Claims", options = leafletOptions(pane = "ground")) 
      dist_names_new <- c(dist_names_new, "Quartz Claims")
    }
    
    mines <- isolate(rv$layers_rv$mines)
    if(!is.null(mines)){
      mines <- st_transform(mines, 4326)
      leafletProxy("map") %>% addPolygons(data=mines, color='#666666', fill=T, fillColor='#666666', weight=1, fillOpacity = 1, group="Mining Claims", options = leafletOptions(pane = "ground")) 
      dist_names_new <- c(dist_names_new, "Mining Claims")
    }       
    # Disturbance
    poly <- isolate(rv$layers_rv$poly)
    if(!is.null(poly)){
      poly <- st_transform(poly, 4326)
      leafletProxy("map") %>% addPolygons(data=poly, color = '#660000', fill=T, fillColor='#660000', fillOpacity=0.8, weight=1, group="Areal disturbances", options = leafletOptions(pane = "top")) 
      dist_names_new <- c("Areal disturbances", dist_names_new)
    }
    line <- isolate(rv$layers_rv$line)
    if(!is.null(line)){
      line <- st_transform(line, 4326)
      leafletProxy("map") %>% addPolylines(data=line, color = "#CC3333",  weight=2, group="Linear disturbances", options = leafletOptions(pane = "top")) 
      dist_names_new <- c("Linear disturbances", dist_names_new)
    }

    fires <- isolate(rv$layers_rv$fires)
    if(!is.null(fires)){
      fires <- st_transform(fires, 4326)
      
      fires$CAUSE_LABEL <- dplyr::case_when(
        fires$CAUSE == "Natural" ~ "Lightning",
        fires$CAUSE == "Human" ~ "Human",
        is.na(fires$CAUSE) | fires$CAUSE == "Undetermined" ~ "Unknown",
        TRUE ~ "Unknown"  # Catch any other unexpected cases
      )
      pal <- colorFactor(
        palette = c("#996633", "#663300", "pink"),
        domain = c("Lightning", "Human", "Unknown"),
      )
      
      leafletProxy("map") %>% addPolygons(data=fires, fill=T, stroke=F, fillColor=~pal(CAUSE_LABEL), fillOpacity=0.8, group="Fires", options = leafletOptions(pane = "top")) 
      dist_names_new <- c("Fires", dist_names_new)
    }
    pa2021 <- isolate(rv$layers_rv$pa2021)
    if(!is.null(pa2021)){
      pa2021 <- st_transform(pa2021, 4326)
      leafletProxy("map") %>% addPolygons(data=pa2021, fill=T, stroke=F, fillColor='#699999', fillOpacity=1,  group="Protected areas", options = leafletOptions(pane = "ground")) 
      add_names_new <- c(add_names_new, "Protected areas")
    }
    ifl2000 <- isolate(rv$layers_rv$ifl2000)
    if(!is.null(ifl2000)){
      ifl2000 <- st_transform(ifl2000, 4326)
      leafletProxy("map") %>% addPolygons(data=ifl2000, fill=T, stroke=F, fillColor='#3366FF', fillOpacity=0.5, group="Intact FL 2000", options = leafletOptions(pane = "ground")) 
      add_names_new <- c(add_names_new, "Intact FL 2000")
    }
    ifl2020 <- isolate(rv$layers_rv$ifl2020)
    if(!is.null(ifl2020)){
      ifl2020 <- st_transform(ifl2020, 4326)
      leafletProxy("map") %>% addPolygons(data=ifl2020, fill=T, stroke=F, fillColor='#000066', fillOpacity=0.5, group="Intact FL 2020", options = leafletOptions(pane = "ground")) 
      add_names_new <- c(add_names_new, "Intact FL 2020")
    }
    
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Studyarea", dist_names_new, add_names_new),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c(add_names_new))
    
    # Close the modal once processing is done
    rv$dist_names(dist_names_new)
    rv$add_names(add_names_new)
    
    ################################################################################################
    # Add modal if layers are missing
    if(!any(c('linear_disturbance', 'areal_disturbance', 'fires', 'Placer_Claims', 'Quartz_Claims', 'Mining_Claims') %in% lyr_names())){
      showModal(modalDialog(
        title = "No disturbance provided in the GPKG.",
        "Please provide a GPKG that has either 'linear_disturbance', 'areal_disturbance', 'fires' or 'Mining_Claims' as a layer",
        easyClose = FALSE,
        footer = modalButton("OK")
      ))
      return()  # Stop further execution  
    }
    
    
    if(!any(c('linear_disturbance', 'areal_disturbance', 'Placer_Claims', 'Quartz_Claims', 'Mining_Claims') %in% lyr_names()) &&
       'fires' %in% lyr_names() &&
       is.null(rv$other_linedist()) &&
       is.null(rv$other_polydist())  
    ){
      showModal(modalDialog(
        title = "Only wildfires will be used to generate the footprint and intactness layers",
        "You can proceed with wildfires. ",
        easyClose = FALSE,
        footer =  modalButton("OK"))
      )  
    }
    
    if(!all(c('linear_disturbance', 'areal_disturbance', 'fires') %in% lyr_names()) & any(c('Placer_Claims', 'Quartz_Claims', 'Mining_Claims') %in% lyr_names())){
      showModal(modalDialog(
        title = "Only mining claims will be used to generate the footprint and intactness layers",
        easyClose = FALSE,
        footer =  modalButton("OK"))
      )  
    }
    
    removeModal()
  })
  
  output$acronym_definitions <- renderUI({
    req(input$confSA)
    tagList(
      tags$div(
        style = "font-size: 0.85em; color: grey;",
        tags$em("*"), "Intact Forest Landscape 2000"
      ),
      tags$div(
        style = "font-size: 0.85em; color: grey;",
        tags$em("**"), "Intact Forest Landscape 2020"
      )
    )
  })
  
  output$legend_definitions <- renderUI({
    req(input$confSA)
    tagList(
      tags$div(
        tags$h5("Map Legend", style = "text-align: left; font-weight: bold; margin-bottom: 0px;"),  # Title above the image
        tags$img(src = "legend.png", width = "60%")
      ),
      tags$div(
        style = "font-size: 0.85em; color: grey;",
        "Elements in Map Legend appear on the map if present in the GeoPackage (gpkg).",
        tags$p("\u00B9",
               tags$strong("Disturbed areas"), " are regions of the study area that have been altered by human disturbance (i.e., mapped linear and areal features) and associated zone of influence (i.e., buffer). Disturbed areas may also include natural disturbances such as fire and/or potential future human disturbances by including mining claims, for example."
        ), 
        tags$p("\u00B2",
               tags$strong("Undisturbed areas"), " are regions within the study area that are not intersected by disturbed areas."
        )
      )
    )
  })
  
  observeEvent(input$confUpload,{ 
    req(!identical(other_polydist(), FALSE) || !identical(other_linedist(), FALSE))
    
    if (is.null(other_polydist()) && is.null(other_linedist())){
      showModal(modalDialog(
        title = "Missing upload of other disturbances",
        "Please upload other linear or areal disturbances before confirming the upload.",
        easyClose = TRUE,
        footer = NULL)
      )
    }
    
    leafletProxy("map") %>% 
      clearGroup("Other linear disturbances") %>%
      clearGroup("Other areal disturbances")
    
    dist_names_new <- rv$dist_names()
    
    other_polydist <- isolate(other_polydist())
    if(!is.null(other_polydist)){
      other_polydist <- st_transform(other_polydist, 4326)
      leafletProxy("map") %>% addPolylines(data = other_polydist, color = "#FF9966", fill=T, fillColor='#FF9966', fillOpacity=0.8, weight=1, group="Other areal disturbances")
      dist_names_new <- c(dist_names_new, "Other areal disturbances")
    }
    other_linedist <- isolate(other_linedist())
    if(!is.null(other_linedist)){
      other_linedist <- st_transform(other_linedist, 4326)
      leafletProxy("map") %>% addPolylines(data = other_linedist, color = "#FF6600",  weight=2 , group="Other linear disturbances")
      dist_names_new <- c(dist_names_new, "Other linear disturbances")
    }
    
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Studyarea", dist_names_new, rv$add_names()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c(rv$add_names()))
    
    rv$dist_names(dist_names_new)
  }) 
  
  ##############################################################################
  # Generate statistics table
  ##############################################################################
  # Reactive for the attributes 
  aoiAttributes <- reactive({
    aoi <- sum(st_area(rv$sa()))
    
    tbl <- tibble(Attribute = "Studyarea (km2)", Value = as.numeric(round(aoi / 1000000, 0)))
    rv$aoiAttributes(tbl)
    return(tbl)
  })
  
  baseAttributes <- reactive({
    req(rv$sa())
    
    if(input$confSA && input$saLayer =="Select a layer"){
      showModal(modalDialog(
        title = "Please select the layer name representing the study area before proceeding.",
        easyClose = TRUE,
        footer = NULL)
      ) 
      return()
    }
    req(studyarea())
    aoi <- sum(st_area(rv$sa()))
    
    # Other dist --Default to NA
    other_linevalue <- NA
    other_linelabel <- "Other linear disturbances (km)"
    other_polyvalue <- NA
    other_polylabel <- "Other areal disturbances (km2)"
    
    # If the user uploaded a shapefile via `other_dist()`
    if (!isFALSE(rv$other_linedist())) {
      other_linedist <- rv$other_linedist() %>% st_union()
      other_linevalue <- round(st_length(other_linedist) / 1000, 1)
      other_linelabel <- "Other linear disturbances (km)"
    }
    if (!isFALSE(rv$other_polydist())) {
      other_polydist <- rv$other_polydist() %>% st_union()
      other_polyvalue <- round(st_area(other_polydist) / 1e6, 1)
      other_polylabel <- "Other areal disturbances (km2)"
    }
    
    tbl <- tibble(
      Attribute = c(
        "Linear disturbances (km)",
        "Areal disturbances (km2)",
        other_linelabel,
        other_polylabel,
        "Fires (%)",
        "Mining claims (%)",
        "Protected areas (%)",
        "Intact FL 2000 (%)*",
        "Intact FL 2020 (%)**"
      ),
      Value = c(
        if (is.null(rv$statslayers_rv$line) || nrow(rv$statslayers_rv$line) == 0) 0 else as.numeric(round(st_length(st_union(rv$statslayers_rv$line)) / 1000, 1)),
        if (is.null(rv$statslayers_rv$poly) || nrow(rv$statslayers_rv$poly) == 0) 0 else as.numeric(round(st_area(st_union(rv$statslayers_rv$poly)) / 1000000, 1)),
        other_linevalue,
        other_polyvalue,
        if (is.null(rv$statslayers_rv$fires) || nrow(rv$statslayers_rv$fires)== 0) 0 else as.numeric(round(st_area(st_union(rv$statslayers_rv$fires)) / aoi * 100, 1)),
        if (is.null(mines_all()) || nrow(mines_all())== 0) 0 else as.numeric(round(st_area(st_union(mines_all())) / aoi * 100, 1)),
        if (is.null(rv$statslayers_rv$pa2021) || nrow(rv$statslayers_rv$pa2021)== 0) 0 else as.numeric(round(st_area(st_union(rv$statslayers_rv$pa2021)) / aoi * 100, 1)),
        if (is.null(rv$statslayers_rv$ifl2000) || nrow(rv$statslayers_rv$ifl2000)== 0) 0 else as.numeric(round(st_area(st_union(rv$statslayers_rv$ifl2000)) / aoi * 100, 1)),
        if (is.null(rv$statslayers_rv$ifl2020) || nrow(rv$statslayers_rv$ifl2020)== 0) 0 else as.numeric(round(st_area(st_union(rv$statslayers_rv$ifl2020)) / aoi * 100, 1))
      )
    )
    rv$baseAttributes(tbl)
    return(tbl)
  })  
  
  # Combine the two sets of attributes
  output$tab1 <- renderTable({
    req(input$confSA)
    req(baseAttributes())  # Ensure `baseAttributes` is ready
    aoi <- aoiAttributes()
    base <- baseAttributes()
    
    # Add additional attributes if the button has been pressed
    additional <- rv$additionalAttributes()
    if (!is.null(additional)) {
      all <- bind_rows(aoi, additional, base)
    } else{
      all <- bind_rows(aoi, base)
    }
    all
  })
  
}
  
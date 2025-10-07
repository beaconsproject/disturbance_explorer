server = function(input, output, session) {
  
  
  ################################################################################################
  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })
  
  ################################################################################################
  # Control overlayGroups
  group_names <- reactiveVal(c())
  dist_names <- reactiveVal(c())
  footprint_names_old <- reactiveVal(c())
  footprint_names_new <- reactiveVal(c())
  industry_line <- reactiveVal(NULL)
  disttype_line <- reactiveVal(NULL)
  industry_poly <- reactiveVal(NULL)
  disttype_poly <- reactiveVal(NULL)
  display1_name <- reactiveVal()
  display2_name <- reactiveVal()
  display3_name <- reactiveVal()
  
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
  
  # read_shp_from_upload: read a shapefile from fileInput
  read_shp_from_upload <- function(upload_input) {
    req(upload_input)
    required_extensions <- c("shp", "shx", "dbf", "prj")
    infile <- upload_input
    file_extensions <- tools::file_ext(infile$name)
    if (all(required_extensions %in% file_extensions)) {
      dir <- unique(dirname(infile$datapath))
      outfiles <- file.path(dir, infile$name)
      name <- tools::file_path_sans_ext(infile$name[1])
      purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y))
      shp_path <- file.path(dir, paste0(name, ".shp"))
      if (file.exists(shp_path)) {
        #return(sf::st_read(shp_path))
        shp <- sf::st_read(shp_path)
        assign(name, shp)
        return(shp)
      } else {
        showModal(modalDialog(
          title = "Shapefile (.shp) is missing.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }
    } else {
      showModal(modalDialog(
        title = "Extension file is missing",
        "Please upload all necessary files for the shapefile (.shp, .shx, .dbf and .prj).",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
  }

  ################################################################################################
  # Observe on selectInput
  ################################################################################################
  observe({
    req(input$upload_gpkg)
    file <- input$upload_gpkg$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "saLayer", choices = c("Select a layer", layers))
  })
  
  ##############################################################################
  # Observe on layers names in gpkg
  lyr_names <- reactive({
    file <- NULL
    if (input$selectInput == 'usedemo') {
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
    layers <- st_layers(file)$name
    return(layers)
  })
  ################################################################################################
  # Observe on Others disturbances 
  observe({
    disable("includeOthers")
    div(
      style = "color: darkgrey;",
      updateCheckboxInput(session = getDefaultReactiveDomain(), "includeOthers", label = "Include others disturbances", value = FALSE)
    )
    req(input$selectInput == "usegpkg" && (!is.null(input$upload_lineothers) || !is.null(input$upload_polyothers)))
    enable("includeOthers")

  })
  
  observe({
    if (!is.null(line()) || !is.null(poly())) {
      enable("createMatrix")
    } else {
      disable("createMatrix")
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
    req(input$upload_gpkg)
    if (!is.null(input$upload_gpkg)){
      if (!any(c("Mining_Claims", "Placer_Claims", "Quartz_Claims") %in% lyr_names())){
        disable("forceclaims")
        div(
          style = "color: darkgrey;",
          updateCheckboxInput(session = getDefaultReactiveDomain(), "forceclaims", label = "Include mining claims", value = FALSE)
        )
      }else{
        enable("forceclaims")
      }
    }
  })
  
  ################################################################################################
  # Observe on Fires (checkboxInput and slider)
  observe({
    req(input$upload_gpkg)
    if (!is.null(input$upload_gpkg)){
      if (!("fires") %in% lyr_names()){
        disable("forcefire")
        div(
          style = "color: darkgrey;",
          updateCheckboxInput(session = getDefaultReactiveDomain(), "forcefire", label = "Include fires", value = FALSE)
        )
      }
    }
  })
  
  observe({
    req(input$selectInput)
    req(fire_sf())
    req(input$forcefire)
    
    # Update max upstream slider
    max_fire <- round(max(fire_sf()$area_ha, na.rm = TRUE), -2)
    # Update the slider input with the max value
    updateSliderInput(session = getDefaultReactiveDomain(), inputId = "firesize", max = max_fire)
    
    # Update max upstream slider
    minyear <- min(fire_sf()$YEAR, na.rm = TRUE)
    maxyear <- max(fire_sf()$YEAR, na.rm = TRUE)
    # Update the slider input with the max value
    updateSliderInput(session = getDefaultReactiveDomain(), inputId = "fireyear", min = minyear , max = maxyear)
  })  
  
  ################################################################################################
  # Observe on linear disturbances - disable if missing
  observe({
    req(input$upload_gpkg)
    if (!is.null(input$upload_gpkg)){
      if (!("linear_disturbance") %in% lyr_names()){
        disable("buffer1")
      }else{
        enable("buffer1")
      }
    }
  })

  ################################################################################################
  # Observe on areal disturbances - disable if missing
  observe({
    req(input$upload_gpkg)
    if (!is.null(input$upload_gpkg)){
      if (!("areal_disturbance") %in% lyr_names()){
        disable("buffer2")
      }else{
        enable("buffer2")
      }
    }
  })
  ################################################################################################
  # Observe on linear and areal for custom buffering - disable if both missing
  observe({
    req(input$upload_gpkg)
    if (!is.null(input$upload_gpkg)){
      if (!any(c("linear_disturbance", "areal_disturbance") %in% lyr_names())){
        disable("selectBuffer")
      }else{
        enable("selectBuffer")
      }
    }
  })
  ################################################################################################
  # Observe on Extra layers
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
  ##############################################################################
  # Read input data - REQUIRED
  ##############################################################################
  studyarea <- reactive({
    # Trigger only when these inputs change
    req(input$selectInput)
    
    if (input$selectInput == "usedemo") {
      st_read('www/demo.gpkg', 'studyarea', quiet = TRUE)
    } else if (input$selectInput == "usegpkg") {
      req(input$upload_gpkg)

      if(input$saLayer != "Select a layer" && input$saLayer != ""){
        st_read(input$upload_gpkg$datapath, input$saLayer, quiet = TRUE)
      }else{
        return(NULL)
      }
    }
  })
  
  line <- reactive({
    req(input$selectInput)

    if (input$selectInput == "usedemo") {
      line_sf <- st_read("www/demo.gpkg", "linear_disturbance", quiet = TRUE)
      return(line_sf)
    }
    if (input$selectInput == "usegpkg") {
      req(input$upload_gpkg)
      if ("linear_disturbance" %in% lyr_names()) {
        line_sf <- st_read(input$upload_gpkg$datapath, "linear_disturbance", quiet = TRUE)
        return(line_sf)
      } else {
        return(NULL)  
      }
    }
    NULL
  })
  
  poly <- reactive({
    req(input$selectInput)

    if (input$selectInput == "usedemo") {
      poly_sf <- st_read("www/demo.gpkg", "areal_disturbance", quiet = TRUE)
      return(poly_sf)
    }
    if (input$selectInput == "usegpkg") {
      req(input$upload_gpkg)

      if ("areal_disturbance" %in% lyr_names()) {
        poly_sf <- st_read(input$upload_gpkg$datapath, "areal_disturbance", quiet = TRUE)
        return(poly_sf)
      } else {
        return(NULL)  
      }
    }
    NULL
  })
  
  ##############################################################################
  # Read input data - OTHERS
  ##############################################################################
  other_linedist <- reactive({
    layer <- NULL
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
        layer <- sf::st_read(file.path(dir,paste0(name, ".shp")), quiet = TRUE)
        
        # Handle cases where 'geometry' might not be named correctly
        if (!"geometry" %in% names(layer) && "geom" %in% names(layer)) {
          layer$geometry <- layer$geom
        }
        
        layer <- layer %>%
          sf::st_set_geometry("geometry") %>%
          sf::st_zm(drop = TRUE, what = "ZM") %>%
          sf::st_transform(st_crs(studyarea()))
      }
    }
    return(layer)
  })
  
  other_polydist <- reactive({
    layer <- NULL
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
        layer <- sf::st_read(file.path(dir,paste0(name, ".shp")), quiet = TRUE)
        
        # Handle cases where 'geometry' might not be named correctly
        if (!"geometry" %in% names(layer) && "geom" %in% names(layer)) {
          layer$geometry <- layer$geom
        }
        
        layer <- layer %>%
          sf::st_set_geometry("geometry") %>%
          sf::st_zm(drop = TRUE, what = "ZM") %>%
          sf::st_transform(st_crs(studyarea()))
      }
    }
    
    return(layer)
  })
  
  ################################################################################################
  ## extra layers
  ################################################################################################
  # Display1
  display1_sf <- eventReactive(input$confExtra,{
    req(input$confExtra)  
    i <- NULL
    
    if(input$extraupload == "extrashp"){
      if(!is.null(input$display1)){
        req(input$display1)
        i <- read_shp_from_upload(input$display1) %>%
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid()
        
        shp_file <- input$display1$name[grepl("\\.shp$", input$display1$name)][1]
        name <- tools::file_path_sans_ext(shp_file)
        display1_name(name)
        
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
        i <- st_read(input$display4$datapath, layer = input$display4a, quiet = TRUE) 
        name <- substr(input$display4a, 1, 25)
        display1_name(name)
      }
    }
    return(i)
  })
  
  display2_sf <- eventReactive(input$confExtra,{
    req(input$confExtra)  
    i <- NULL
    
    if(input$extraupload == "extrashp"){
      if(!is.null(input$display2)){
        req(input$display2)
        i <- read_shp_from_upload(input$display2) %>%
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid()
        
        shp_file <- input$display2$name[grepl("\\.shp$", input$display2$name)][1]
        name <- tools::file_path_sans_ext(shp_file)
        display2_name(name)
        
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
        i <- st_read(input$display4$datapath, layer = input$display4b, quiet = TRUE)
        name <- substr(input$display4b, 1, 25)
        display2_name(name)
      }
    }
    return(i)
  })  
  
  display3_sf <- eventReactive(input$confExtra,{
    req(input$confExtra)  
    i <- NULL
    
    if(input$extraupload == "extrashp"){
      if(!is.null(input$display3)){
        req(input$display3)
        i <- read_shp_from_upload(input$display3) %>%
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid()
        
        shp_file <- input$display3$name[grepl("\\.shp$", input$display3$name)][1]
        name <- tools::file_path_sans_ext(shp_file)
        display3_name(name)
        
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
        i <- st_read(input$display4$datapath, layer = input$display4c, quiet = TRUE)
        name <- substr(input$display4c, 1, 25)
        display3_name(name)
      }
    }
    return(i)
  })  
  
  ################################################################################################
  # Render UI for the selection of column name for disturbances 
  ################################################################################################
  output$lineIndustryUI <- renderUI({
    req(input$createMatrix == TRUE)
    req(line())  # only show if line() is available
    div(
      style = "margin-top: -30px;",  
      selectInput("lineindustry", 
                label = div(style = "font-size:13px;margin-top: -10px;", ""), 
                choices = c("--industry type--",colnames(line())), 
                selected = "--industry type--")
    )
  })
  
  output$lineDistTypeUI <- renderUI({
    req(input$createMatrix == TRUE)
    req(line())
    div(
      style = "margin-top: -30px;",
      selectInput("linedisttype", 
                label = div(style = "font-size:13px;", ""), 
                choices = c("--disturbance type--", colnames(line())), 
                selected = "--disturbance type--")
    )
  })
  
  output$polyIndustryUI <- renderUI({
    req(input$createMatrix == TRUE)
    req(poly())
    div(
      style = "margin-top: -30px;",
      selectInput("polyindustry", 
                label = div(style = "font-size:13px;", ""), 
                choices = c("--industry type--",colnames(poly())), 
                selected = "--industry type--")
    )
  })
  
  output$polyDistTypeUI <- renderUI({
    req(input$createMatrix == TRUE)
    req(poly())
    div(
      style = "margin-top: -30px;",
      selectInput("polydisttype", 
                label = div(style = "font-size:13px;margin: 0px;", ""), 
                choices = c("--disturbance type--",colnames(poly())), 
                selected = "--disturbance type--")
    )
  })

  ##############################################################################
  # Read input data - OPTIONAL
  ##############################################################################
  fire_sf <- reactive({
    if (input$selectInput=='usedemo') {
      fi <-st_read('www/demo.gpkg', 'fires', quiet=T) %>%
        suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid() %>%
        mutate(area_ha = as.numeric(st_area(geom)/10000))
      addGroup("Fires")
      return(fi)
    } else if (!is.null(input$upload_gpkg)){
      if ("fires" %in% lyr_names()) {
        # Read the "fires" layer from the uploaded file if it exists
        fi <-st_read(input$upload_gpkg$datapath, 'fires', quiet = TRUE) %>%
          suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
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
      return(NULL)
    }
  })
  
  fires <- reactive({
    if(!is.null(fire_sf())){
      fire <- fire_sf() %>%
        dplyr::filter(YEAR >= input$fireyear[1] & YEAR <= input$fireyear[2])
      return(fire)
    }else{
      return(NULL)
    }
  }) 
  
  ifl2000 <- reactive({
    if (input$selectInput=='usedemo') {
      la <-st_read('www/demo.gpkg', 'Intact_FL_2000', quiet=T)
      addGroup("Intact FL 2000")
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      if ("Intact_FL_2000" %in% lyr_names()) {
        # Read the "intactness_2000" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'Intact_FL_2000', quiet = TRUE)
        addGroup("Intact FL 2000")
        return(la)
      } else {
        # Optionally, handle the case where the "Intact_FL_2000" layer is missing
        removeGroup("Intact FL 2000")
        return(NULL)  # or display a message, warning, etc.
      }
    }else{
      NULL
    }
  })
  
  ifl2020 <- reactive({
    if (input$selectInput=='usedemo') {
      la <-st_read('www/demo.gpkg', 'Intact_FL_2020', quiet=T)
      addGroup("Intact FL 2020")
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      if ("Intact_FL_2020" %in% lyr_names()) {
        # Read the "intactness_2020" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'Intact_FL_2020', quiet = TRUE)
        addGroup("Intact FL 2020")
        return(la)
      } else {
        # Optionally, handle the case where the "Intact_FL_2020" layer is missing
        removeGroup("Intact FL 2020")
        return(NULL)  
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
      if ("protected_areas" %in% lyr_names()) {
        # Read the "protected_areas" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'protected_areas', quiet = TRUE)
        addGroup("Protected areas")
        return(la)
      } else {
        # Optionally, handle the case where the "protected_areas" layer is missing
        removeGroup("Protected areas")
        return(NULL)  
      }
    }
  })
  
  herds <- reactive({
    if (input$selectInput=='usedemo') {
      return(NULL)  
      #la <-st_read('www/demo.gpkg', 'Caribou_Herds', quiet=T)
      #addGroup("Caribou Herds")
      #return(la)
    } else if (!is.null(input$upload_gpkg)){
      if ("Caribou_Herds" %in% lyr_names()) {
        # Read the "Caribou_Herds" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'Caribou_Herds', quiet = TRUE)
        addGroup("Caribou Herds")
        return(la)
      } else {
        # Optionally, handle the case where the "Caribou_Herds" layer is missing
        removeGroup("Caribou Herds")
        return(NULL)  
      }
    }
  })
  
  placers <- reactive({
    if (input$selectInput=='usedemo') {
      la <-st_read('www/demo.gpkg', 'Placer_Claims', quiet=T)
      addGroup("Placer Claims")
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      if ("Placer_Claims" %in% lyr_names()) {
        # Read the "Placer_Claims" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'Placer_Claims', quiet = TRUE)
        addGroup("Placer Claims")
        return(la)
      } else {
        # Optionally, handle the case where the "Placer_Claims" layer is missing
        removeGroup("Placer Claims")
        return(NULL)  
      }
    }
  })
  
  quartz <- reactive({
    if (input$selectInput=='usedemo') {
      la <-st_read('www/demo.gpkg', 'Quartz_Claims', quiet=T)
      addGroup("Quartz Claims")
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      if ("Quartz_Claims" %in% lyr_names()) {
        # Read the "Quartz_Claims" layer from the uploaded file if it exists
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
      if ("Mining_Claims" %in% lyr_names()) {
        # Read the "Mining_Claims" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'Mining_Claims', quiet = TRUE)
        addGroup("Mining Claims")
        return(la)
      } else {
        # Optionally, handle the case where the "Mining_Claims" layer is missing
        removeGroup("Mining Claims")
        return(NULL)  # or display a message, warning, etc.
      }
    }
  })
  
  mines_all <- reactive({
    geoms <- list(quartz(), placers(), mines()) |>
    purrr::compact() |>                # removes NULLs
    purrr::map(sf::st_geometry)       # extract just the geometries

  # If no layers, return NULL
  if (length(geoms) == 0) return(NULL)

  # Combine geometries into one sf object
  sf::st_as_sf(sf::st_union(do.call(c, geoms)))
  })
  
  ########################################################
 observeEvent(input$distType, {
   
    if (is.null(input$selectInput)){
      showModal(modalDialog(
        title = "Missing type of source dataset",
        "Before proceeding, please select the source dataset.",
        easyClose = TRUE,
        footer = NULL)
      )
    } else if (input$selectInput == "usedemo") {
      if (!is.null(line())) {
        industry_line("TYPE_INDUSTRY")
        disttype_line("TYPE_DISTURBANCE")
      }
      if (!is.null(poly())) {
        industry_poly("TYPE_INDUSTRY")
        disttype_poly("TYPE_DISTURBANCE")
      }
    }else{
      req(!is.null(input$upload_gpkg))
      req(input$createMatrix == TRUE)
      
      if (!is.null(line())) {
        industry_line(input$lineindustry)
        disttype_line(input$linedisttype)
      }
      if (!is.null(poly())) {
        industry_poly(input$polyindustry)
        disttype_poly(input$polydisttype)
      }
    }
  })
  
  observeEvent(input$distType, {
    output$linear_matrix_ui <- renderUI({
        if (is.null(line())) {
          tags$p("NONE", style = "color: gray; font-style: italic;")
        } else {
          # Build matrix
          
          #req(industry_line(), disttype_line(), input$buffer1)
          industry_line <- industry_line()
          disttype_line <- disttype_line()
          
          line_tibble <- line() %>%
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
         line_summary <- if (is.null(line())) {
            tibble(
              TYPE_INDUSTRY = "NONE",
              TYPE_DISTURBANCE = "NONE",
              AREA_KM2 = 0
            )
          } else {
            data <- line()
            data <- data %>%
              mutate(
                length_km = st_length(line()) / 1000,
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
        
          # Return the matrixInput to UI
          matrixInput("linear_buffers",
                      value = mline,
                      rows = list(names = FALSE, extend = TRUE),
                      cols = list(names = TRUE))
        }
      })
      
      output$areal_matrix_ui <- renderUI({
        if (is.null(poly())) {
          tags$p("NONE", style = "color: gray; font-style: italic;")
        } else {
          #req(industry_poly, disttype_poly, input$buffer2)
          industry_poly <- industry_poly()
          disttype_poly <- disttype_poly()
          
          poly_tibble <- poly() %>%
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
          
          poly_summary <- if (is.null(poly())) {
            tibble(
              TYPE_INDUSTRY = "NONE",
              TYPE_DISTURBANCE = "NONE",
              AREA_KM2 = 0
            )
          } else {
            data <- poly()
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
          
          matrixInput("areal_buffers",
                      value = mpoly,
                      rows = list(names = FALSE, extend = TRUE),
                      cols = list(names = TRUE))
        }
      })
  })
  
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
  ##############################################################################
  # Buffer disturbances and calculate footprint and intactness
  ##############################################################################
  footprint_sf <- reactive({
    
    if(input$distType[1]==0){
      showModal(modalDialog(
        title = "Missing source dataset confirmation",
        "Before proceeding, please confirm the source dataset in the Select study area section.",
        easyClose = TRUE,
        footer = NULL)
      )
    }
    
    req(input$distType)
    
    if (!is.null(poly()) | !is.null(line()) | !is.null(fires()) | !is.null(mines_all())  | !is.null(other_linedist()) | !is.null(other_polydist())) {
      if (!is.null(poly()) | !is.null(line()) ) {
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
        
        if (is.null(input$selectBuffer)){
          showModal(modalDialog(
            title = "Missing buffer type confirmation",
            "Before proceeding, please confirm how disturbance layers will be buffered.",
            easyClose = TRUE,
            footer = NULL)
          )
          v1<- NULL
          v2 <- NULL
          #return()
        } else if (input$selectBuffer== "custom_buffers") {
          if (!is.null(line())) {
            m1sub <- as_tibble(input$linear_buffers) %>% dplyr::select(any_of(c("TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE_M"))) %>%      
              mutate(BUFFER_SIZE_M=as.integer(BUFFER_SIZE_M))
            line <- line() %>%
              mutate(
                #TYPE_INDUSTRY = !!sym(industry_line),
                #TYPE_DISTURBANCE = !!sym(disttype_line)
                TYPE_INDUSTRY = if (!is.null(industry_line) && industry_line %in% colnames(.)) .[[industry_line]] else "NONE",
                TYPE_DISTURBANCE = if (!is.null(disttype_line) && disttype_line %in% colnames(.)) .[[disttype_line]] else "NONE"
              ) %>%
              left_join(m1sub, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE")) %>% 
              filter(!is.na(BUFFER_SIZE_M))
            v1 <- st_union(st_buffer(line, line$BUFFER_SIZE_M))
          } else { v1 <- NULL}
          if (!is.null(poly())) {
            m2sub <- as_tibble(input$areal_buffers) %>% dplyr::select(any_of(c("TYPE_INDUSTRY", "TYPE_DISTURBANCE", "BUFFER_SIZE_M"))) %>% 
              mutate(BUFFER_SIZE_M=as.integer(BUFFER_SIZE_M))
            poly <- poly() %>%
              mutate(
                #TYPE_INDUSTRY = !!sym(industry_poly),
                #TYPE_DISTURBANCE = !!sym(disttype_poly)
                TYPE_INDUSTRY = if (!is.null(industry_line) && industry_line %in% colnames(.)) .[[industry_line]] else "NONE",
                TYPE_DISTURBANCE = if (!is.null(disttype_line) && disttype_line %in% colnames(.)) .[[disttype_line]] else "NONE"
              ) %>%
              left_join(m2sub, by = c("TYPE_INDUSTRY", "TYPE_DISTURBANCE")) %>% 
              filter(!is.na(BUFFER_SIZE_M))
          
            v2 <- st_union(st_buffer(poly, poly$BUFFER_SIZE_M))
          } else { v2 <- NULL}
        }else {
          if (!is.null(line())) {
            v1 <- st_union(st_buffer(line(), input$buffer1)) %>%
              st_sf()
          } else { v1 <- NULL}
          if (!is.null(poly())) {
            v2 <- st_union(st_buffer(poly(), input$buffer2)) %>%
              st_sf()
          } else { v2 <- NULL}
        } 
      } else{
        v1<- NULL
        v2 <- NULL
      }
      
      if(input$includeOthers) {
        if(!is.null(other_linedist())){
          v3 <- st_union(st_buffer(other_linedist(), input$otherlinesize)) %>% 
            st_sf()
        }else { v3 <- NULL}
        
        if(!is.null(other_polydist())){
          v4 <- st_union(st_buffer(other_polydist(), input$otherpolysize)) %>% 
            st_sf()
        } else { v4 <- NULL}
      } else { 
        v3 <- NULL
        v4 <- NULL
      }
      
      if(input$forceclaims & ('Quartz_Claims' %in% lyr_names() | 'Placer_Claims' %in% lyr_names() | 'Mining_Claims' %in% lyr_names())) {
        if ('Quartz_Claims' %in% lyr_names() & !'Placer_Claims' %in% lyr_names()) {
          v5 <- st_union(st_buffer(quartz(), input$minesize)) %>% 
            st_sf()
        } else if ('Placer_Claims' %in% lyr_names() & !'Quartz_Claims' %in% lyr_names()) {
          v5 <- st_union(st_buffer(placers(), input$minesize)) %>% 
            st_sf()
        } else if ('Mining_Claims' %in% lyr_names()) {
          v5 <- st_union(st_buffer(mines(), input$minesize)) %>% 
            st_sf()
        } else {
          v5a <- st_union(st_buffer(placers(), input$minesize)) %>% 
            st_sf()
          v5b <- st_union(st_buffer(quartz(), input$minesize)) %>% 
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
        v <- st_intersection(v_union, studyarea())
      } else {
        v <- NULL
      }
      return(v)
    }
  })
  
  footprintfire_sf <- reactive({
    if(input$forcefire & 'fires' %in% lyr_names()) {
    #if('fires' %in% lyr_names()) {
        fires_sf <- fires() %>%
        dplyr::filter(area_ha > input$firesize)
      v5 <- st_union(fires_sf) %>% 
        st_sf()
      if(!is.null(footprint_sf())){
        v_union <- st_union(footprint_sf(), v5)
      }else{
        v_union <- v5
      }
      v <- st_intersection(v_union, studyarea())
    } else { v <- NULL}
    
    return(v)
  })
  
  intactness_sf <- reactive({
    footprint_names_init <- c()
    if (input$forcefire &  !is.null(footprintfire_sf())) {
      ifl <- st_difference(studyarea(), footprintfire_sf())
      footprint_names_init <- "Disturbed areas (human + fires)"
    } else {
      req(footprint_sf())
      ifl <- st_difference(studyarea(), footprint_sf())
      footprint_names_init <- "Disturbed areas (human)"
    }
    
    footprint_names_new(footprint_names_init)
    x <- suppressWarnings(st_cast(ifl, "POLYGON"))
    x <- mutate(x, area_km2=as.numeric(st_area(x)/1000000))
    y <- filter(x, area_km2 > input$area1)
    return(y)
  })
  
  
  ######################################################
  ##  SERVE USER GUIDE SECTION
  ######################################################
  output$guide_ui <- renderUI({
    req(input$tabs)  # 'sidebar' is the id of your sidebarMenu
    
    guide_file <- switch(input$tabs,
                         "select" = "docs/select_guide.md",
                         "buffer" = "docs/buffer_guide.md",
                         "download" = "docs/dwd_guide.md",
                         NULL
    )
    
    if (!is.null(guide_file) && file.exists(guide_file)) {
      includeMarkdown(guide_file)
    } else {
      tags$p("No user guide available for this section.")
    }
  })
  
  
  ##############################################################################
  # View initial set of maps
  ##############################################################################
  output$map1 <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      fitBounds(lng1 = -121, lat1 = 44, lng2 = -65, lat2 = 78) %>%
      addMapPane(name = "ground", zIndex=380) %>%
      addMapPane(name = "top", zIndex=420) %>%
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap")%>% 
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                   options = layersControlOptions(collapsed = FALSE))
  })
  
  observeEvent(input$distType,{
    req(input$distType)
    req(studyarea())
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Uploading geopackage layers. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    leafletProxy("map1") %>% 
      clearGroup("Study area") %>%
      clearGroup("Linear disturbance") %>%
      clearGroup("Areal disturbance") %>%
      clearGroup("Other linear disturbances") %>%
      clearGroup("Other areal disturbances") %>%
      clearGroup("Undisturbed areas") %>%
      clearGroup("Fires") %>%
      clearGroup("Protected areas") %>%
      clearGroup("Intact FL 2000") %>%
      clearGroup("Intact FL 2020") %>%
      clearGroup("Mining Claims") %>%
      clearGroup("Placer Claims") %>%
      clearGroup("Quartz Claims") %>%
      clearGroup("Caribou Herds")
     
    sa <- st_transform(studyarea(), 4326)
    map_bounds1 <- sa %>% st_bbox() %>% as.character()
    
    leafletProxy("map1") %>%
        fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
        addPolygons(data=sa, color='black', fill=F, opacity = 1, weight=2, group="Study area", options = leafletOptions(pane = "top")) %>%
      
        addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Study area"),
                       options = layersControlOptions(collapsed = FALSE)) 
      
    dist_names_new <- c()
    group_names_new <- c()
    # Disturbance
    poly <- isolate(poly())
    if(!is.null(poly)){
      poly <- st_transform(poly, 4326)
      leafletProxy("map1") %>% addPolygons(data=poly, color = '#660000', fill=T, fillColor='#660000', fillOpacity=0.8, weight=1, group="Areal disturbances", options = leafletOptions(pane = "top")) 
      dist_names_new <- c(dist_names_new,"Areal disturbances")
    }
    line <- isolate(line())
    if(!is.null(line)){
      line <- st_transform(line, 4326)
      leafletProxy("map1") %>% addPolylines(data=line, color = "#CC3333",  weight=2, group="Linear disturbances", options = leafletOptions(pane = "top")) 
      dist_names_new <- c(dist_names_new,"Linear disturbances")
    }
    other_polydist <- isolate(other_polydist())
    if(!is.null(other_polydist) && input$selectInput =="usegpkg"){
      other_polydist <- st_transform(other_polydist, 4326)
      leafletProxy("map1") %>% addPolylines(data = other_polydist, color = "#FF9966", fill=T, fillColor='#FF9966', fillOpacity=0.8, weight=1, group="Other areal disturbances")
      dist_names_new <- c(dist_names_new, "Other areal disturbances")
    }
    other_linedist <- isolate(other_linedist())
    if(!is.null(other_linedist) && input$selectInput =="usegpkg"){
      other_linedist <- st_transform(other_linedist, 4326)
      leafletProxy("map1") %>% addPolylines(data = other_linedist, color = "#FF6600",  weight=2 , group="Other linear disturbances")
      dist_names_new <- c(dist_names_new, "Other linear disturbances")
    }
    # Optional
    fires <- isolate(fires())
    if(!is.null(fires)){
        fires <- st_transform(fires, 4326)
        
        fires$CAUSE_LABEL <- dplyr::case_when(
          fires$CAUSE == "L" ~ "Lightning",
          fires$CAUSE == "H" ~ "Human",
          is.na(fires$CAUSE) | fires$CAUSE == "" ~ "Unknown",
          TRUE ~ "Unknown"  # Catch any other unexpected cases
        )
        pal <- colorFactor(
          palette = c("#996633", "#663300", "pink"),
          domain = c("Lightning", "Human", "Unknown"),
        )
        
        leafletProxy("map1") %>% addPolygons(data=fires, fill=T, stroke=F, fillColor=~pal(CAUSE_LABEL), fillOpacity=0.8, group="Fires", options = leafletOptions(pane = "top")) 
        group_names_new <- c(group_names_new, "Fires")
    }
    ifl2000 <- isolate(ifl2000())
    if(!is.null(ifl2000)){
      ifl2000 <- st_transform(ifl2000, 4326)
      leafletProxy("map1") %>% addPolygons(data=ifl2000, fill=T, stroke=F, fillColor='#3366FF', fillOpacity=0.5, group="Intact FL 2000", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Intact FL 2000")
    }
    ifl2020 <- isolate(ifl2020())
    if(!is.null(ifl2020)){
      ifl2020 <- st_transform(ifl2020, 4326)
      leafletProxy("map1") %>% addPolygons(data=ifl2020, fill=T, stroke=F, fillColor='#000066', fillOpacity=0.5, group="Intact FL 2020", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Intact FL 2020")
    }
    pa2021 <- isolate(pa2021())
    if(!is.null(pa2021)){
      pa2021 <- st_transform(pa2021, 4326)
      leafletProxy("map1") %>% addPolygons(data=pa2021, fill=T, stroke=F, fillColor='#699999', fillOpacity=1,  group="Protected areas", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Protected areas")
    }
    herds <- isolate(herds())
    if(!is.null(herds)){
      herds <- st_transform(herds, 4326)
      leafletProxy("map1") %>% addPolygons(data=herds, color= '#666666', fill=T, fillColor='#666666', weight=1, fillOpacity = 1, group="Caribou Herds", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Caribou Herds")
    }
    placers <- isolate(placers())
    if(!is.null(placers)){
      placers <- st_transform(placers, 4326)
      leafletProxy("map1") %>% addPolygons(data=placers, color= '#666666', fill=T, fillColor='#666666', weight=1, fillOpacity = 1, group="Placer Claims", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Placer Claims")
    }
    quartz <- isolate(quartz())
    if(!is.null(quartz)){
      quartz <- st_transform(quartz, 4326)
      leafletProxy("map1") %>% addPolygons(data=quartz, color = '#CCCCCC', fill=T, fillColor='#CCCCCC', weight=1, fillOpacity = 1, group="Quartz Claims", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Quartz Claims")
    }
    
    mines <- isolate(mines())
    if(!is.null(mines)){
      mines <- st_transform(mines, 4326)
      leafletProxy("map1") %>% addPolygons(data=mines, color='#666666', fill=T, fillColor='#666666', weight=1, fillOpacity = 1, group="Mining Claims", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Mining Claims")
    } 
    
    leafletProxy("map1") %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Study area", dist_names_new, group_names_new),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c(group_names()))
    
    # Close the modal once processing is done
    dist_names(dist_names_new)
    group_names(group_names_new)
    removeModal()
    
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
         is.null(other_linedist()) &&
         is.null(other_polydist())
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
  })
  
  ###########################
  # Map extra layers
  observeEvent(input$confExtra,{ 
    
    map1 <- leafletProxy("map1") %>%
      clearGroup(display1_name()) %>%
      clearGroup(display2_name()) %>%
      clearGroup(display3_name())
    
    group_names_new <- group_names()
    
    if (isMappable(display1_sf())) { 
      display1 <- st_transform(display1_sf(), 4326)
      geom_type <- unique(sf::st_geometry_type(display1))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map1 <- map1 %>% addPolygons(data=display1,  fillColor='#663300', stroke=F, fill = T, fillOpacity = 0.5, group=display1_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map1 <- map1 %>% addPolylines(data = display1, color = '#663300', weight = 2, group = display1_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map1 <- map1 %>% addCircleMarkers(data = display1, color = '#663300', radius = 5, fillOpacity = 0.7, group = display1_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }

      group_names_new <- c(group_names_new,display1_name())
    } 
    if (isMappable(display2_sf())) { 
      display2 <- st_transform(display2_sf(), 4326)
      geom_type <- unique(sf::st_geometry_type(display2))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map1 <- map1 %>% addPolygons(data=display2,  fillColor='#330066', stroke=F, fill = T, fillOpacity = 0.5, group=display2_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map1 <- map1 %>% addPolylines(data = display2, color = '#330066', weight = 2, group = display2_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map1 <- map1 %>% addCircleMarkers(data = display2, color = '#330066', radius = 5, fillOpacity = 0.7, group = display2_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }

      group_names_new <- c(group_names_new,display2_name())
    } 
    if (isMappable(display3_sf())) { 
      display3 <- st_transform(display3_sf(), 4326)
      geom_type <- unique(sf::st_geometry_type(display3))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map1 <- map1 %>% addPolygons(data=display3,  fillColor='#003333', stroke=F, fill = T, fillOpacity = 0.5, group=display3_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map1 <- map1 %>% addPolylines(data = display3, color = '#003333', weight = 2, group = display3_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map1 <- map1 %>% addCircleMarkers(data = display3, color = '#003333', radius = 5, fillOpacity = 0.7, group = display3_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }
      group_names_new <- c(group_names_new,display3_name())
    } 
    group_names(group_names_new)
    
    leafletProxy("map1") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Study area", group_names_new),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments", group_names_new))
  })  
  
  ##############################################################################
  # Update map with intactness/footprint
  ##############################################################################
  observeEvent(input$goButton,{
    
    if(input$distType[1]==0){
      showModal(modalDialog(
        title = "Missing source dataset confirmation",
        "Before proceeding, please confirm the source dataset in the Select study area section.",
        easyClose = TRUE,
        footer = NULL)
      )
    }
    
    req(input$distType)
    
    if(is.null(input$selectBuffer)){
      showModal(modalDialog(
        title = "Missing buffer type confirmation",
        "Before proceeding, please confirm how disturbance layers will be buffered.",
        easyClose = TRUE,
        footer = NULL)
      )
    }
    
    req(input$selectBuffer)
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Generating footprint and intactness map. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )

    if(is.null(footprintfire_sf())){
      fp_sf <- st_transform(footprint_sf(), 4326)
    }else{
      fp_sf <- st_transform(footprintfire_sf(), 4326)
    }
    intact_sf <- st_transform(intactness_sf(), 4326)
    
    leafletProxy("map1") %>%
      clearGroup('Undisturbed areas') %>%
      clearGroup(footprint_names_old()) %>% 
      addPolygons(data=intact_sf, color='#336633', stroke=F, fillOpacity=0.5, group='Undisturbed areas', options = leafletOptions(pane = "top")) %>%
      addPolygons(data=fp_sf, color='black', stroke=F, fillOpacity=0.5, group=footprint_names_new(), options = leafletOptions(pane = "top")) 

    leafletProxy("map1") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Study area", "Undisturbed areas", footprint_names_new(), dist_names(), group_names()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c(footprint_names_new(), group_names()))
    
    # Close the modal once processing is done
    footprint_names_old(footprint_names_new())
    removeModal()
    
  })
  
  ##############################################################################
  # Generate statistics table
  ##############################################################################
  # Reactive for the attributes updated by `usedemo` or `upload_gpkg`
  aoiAttributes <- reactive({
    req(input$selectInput == "usedemo" || (input$selectInput == "usegpkg" && !is.null(input$upload_gpkg)))
    aoi <- sum(st_area(studyarea()))

    tibble(Attribute = "Study area (km2)", Value = as.numeric(round(aoi / 1000000, 0)))
  })

  # Reactive for the attributes updated by `usedemo` or `upload_gpkg`
  baseAttributes <- reactive({
    req(input$selectInput == "usedemo" || (input$selectInput == "usegpkg" && !is.null(input$upload_gpkg)))
    aoi <- sum(st_area(studyarea()))
    
    # Other dist --Default to NA
    other_linevalue <- NA
    other_linelabel <- "Other linear disturbances (km)"
    other_polyvalue <- NA
    other_polylabel <- "Other areal disturbances (km2)"
    
    # If the user uploaded a shapefile via `other_dist()`
    if (!is.null(other_linedist()) && input$includeOthers) {
      other_linevalue <- round(sum(st_length(other_linedist())) / 1000, 1)
      other_linelabel <- "Other linear disturbances (km)"
    }
    if (!is.null(other_polydist()) && input$includeOthers) {
      other_polyvalue <- round(sum(st_area(other_polydist())) / 1e6, 1)
      other_polylabel <- "Other areal disturbances (km2)"
    }
    
    tibble(
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
        if (is.null(line())) 0 else as.numeric(round(sum(st_length(line())) / 1000, 1)),
        if (is.null(poly())) 0 else as.numeric(round(sum(st_area(poly())) / 1000000, 1)),
        other_linevalue,
        other_polyvalue,
        if (is.null(fires())) 0 else as.numeric(round(sum(st_area(fires())) / aoi * 100, 1)),
        if (is.null(mines_all())) 0 else as.numeric(round(sum(st_area(mines_all())) / aoi * 100, 1)),
        if (is.null(pa2021())) 0 else as.numeric(round(sum(st_area(pa2021())) / aoi * 100, 1)),
        if (is.null(ifl2000())) 0 else as.numeric(round(sum(st_area(ifl2000())) / aoi * 100, 1)),
        if (is.null(ifl2020())) 0 else as.numeric(round(sum(st_area(ifl2020())) / aoi * 100, 1))
      )
    )
  })  
  output$acronym_definitions <- renderUI({
    req(input$distType)
    tagList(
      tags$div(
        style = "font-size: 0.85em; color: grey;",
        tags$em("*"), "Intact Forest Landscape 2000"
      ),
      tags$div(
        style = "font-size: 0.85em; color: grey;",
        tags$em("**"), "Intact Forest Landscape 2020"
      ),
      tags$br(),
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
  
  # reactiveVal to store additional attributes
  additionalAttributes <- reactiveVal(NULL)
  
  # Reactive for the attributes updated by `goButton`
  observeEvent(input$goButton, {
    aoi <- sum(st_area(studyarea()))
    if (!is.null(footprintfire_sf())) {
      footprint <- st_union(footprintfire_sf()) 
    }else if (!is.null(footprint_sf())){
      footprint <- st_union(footprint_sf())
    } else{
      footprint <- NULL
    }
    
    additionalAttributes(tibble(
      Attribute = c(
        "Undisturbed areas (%)",
        "Disturbed areas (%)"
      ),
      Value = c(
        if (is.null(intactness_sf())) 0 else as.numeric(round(sum(st_area(intactness_sf())) / aoi * 100, 1)),
        if (is.null(footprint)) 0 else as.numeric(round(sum(st_area(footprint)) / aoi * 100, 1))
      )
    ))
  })
  
  # Reset additional attributes when studyarea or related inputs change
  observeEvent(c(studyarea(), poly(), line(), fires(), mines_all(), pa2021()), {
    additionalAttributes(NULL)
  })
  
  # Combine the two sets of attributes
  output$tab1 <- renderTable({
    req(input$distType)
    req(baseAttributes())  # Ensure `baseAttributes` is ready
    aoi <- aoiAttributes()
    base <- baseAttributes()
    
    # Add additional attributes if the button has been pressed
    additional <- additionalAttributes()
    if (!is.null(additional)) {
      all <- bind_rows(aoi, additional, base)
    } else{
      all <- bind_rows(aoi, base)
    }
    all
  })
  
  ##############################################################################
  # Save features to a geopackage
  ##############################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("disturbance_explorer-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
        x <- data.frame(Area_km2 = aoiAttributes()[1,2],
                        Lineardist_km  = baseAttributes()[1,2],
                        Arealdist_km2 = baseAttributes()[2,2],
                        otherLinear_km  = baseAttributes()[3,2],
                        otherAreal_km2  = baseAttributes()[4,2],
                        Fires_per = baseAttributes()[5,2],
                        Mine_per = baseAttributes()[6,2],
                        PA2021_per= baseAttributes()[7,2],
                        IntactFL2000_per = baseAttributes()[8,2],
                        IntactFL2020_per = baseAttributes()[9,2])
        colnames(x) <-c("Area_km2","Lineardist_km","Arealdist_km2","otherLinear_km","otherAreal_km2","Fires_per", "Mines_per", "PA2021_per","IntactFL2000_per","IntactFL2020_per")
        aoi <- cbind(st_union(studyarea()), x)
        st_write(aoi, dsn=file, layer='studyarea')
        if (!is.null(line())) st_write(line(), dsn=file, layer='linear_disturbance', append=TRUE)
        if (!is.null(poly())) st_write(poly(), dsn=file, layer='areal_disturbance', append=TRUE)
        if (!is.null(fires())) st_write(fires(), dsn=file, layer='fires', append=TRUE)
        if (!is.null(other_linedist())) st_write(other_linedist(), dsn=file, layer='other_linear_disturbances', append=TRUE)
        if (!is.null(other_polydist())) st_write(other_polydist(), dsn=file, layer='other_areal_disturbances', append=TRUE)
        if (!is.null(pa2021())) st_write(pa2021(), dsn=file, layer='protected_areas', append=TRUE)
        if (!is.null(placers())) st_write(placers(), dsn=file, layer='Placer_Claims', append=TRUE)
        if (!is.null(quartz())) st_write(quartz(), dsn=file, layer='Quartz_Claims', append=TRUE)
        if (!is.null(mines())) st_write(mines(), dsn=file, layer='mining_claims', append=TRUE)
        if (!is.null(mines())) st_write(herds(), dsn=file, layer='Caribou_Herds', append=TRUE)
        if (!is.null(display1_sf())) st_write(display1_sf(), dsn=file, layer=display1_name(), append=TRUE)
        if (!is.null(display2_sf())) st_write(display2_sf(), dsn=file, layer=display2_name(), append=TRUE)
        if (!is.null(display3_sf())) st_write(display3_sf(), dsn=file, layer=display3_name(), append=TRUE)

        if (input$goButton) {
          x <- data.frame(Undisturbed_per = additionalAttributes()[1,2],
                          Disturbed_per  = additionalAttributes()[2,2])
          colnames(x) <-c("Undisturbed_per", "Disturbed_per")
          aoi <- cbind(aoi, x)
          st_write(aoi, dsn=file, layer='studyarea', append=FALSE)
          st_write(intactness_sf(), dsn=file, layer='undisturbed', append=TRUE)
          if (!is.null(footprintfire_sf())){
            st_write(footprintfire_sf(), dsn=file, layer='disturbed', append=TRUE)
          } else{
            st_write(footprint_sf(), dsn=file, layer='disturbed', append=TRUE)
          }
        }
    }
  )
}

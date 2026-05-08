downloadServer <- function(input, output, session, project, rv){
  
  ##############################################################################
  # Save buffer matrix
  ##############################################################################
  output$downloadMatrix <- downloadHandler(
    filename = function() { paste("Disturbance_explorer_buffer-", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      line <- as_tibble(input$linear_buffers)
      poly <- as_tibble(input$areal_buffers)
      matrix <- dplyr::bind_rows(line, poly)
      
      write.csv(matrix, file, row.names = FALSE)
    }
  )
  
  ##############################################################################
  # Save features to a geopackage
  ##############################################################################
  output$downloadStats <- downloadHandler(
    filename = function() { paste("Disturbance_explorer_stats-", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      
      write.csv(rv$summaryStats(), file, row.names = FALSE)
    }
  )
  
  ##############################################################################
  # Save features to a geopackage
  ##############################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("Disturbance_explorer-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
      
      if(input$goButton ==0){
        showModal(modalDialog(
          title = "Missing disturbed and undisturbed layers.",
          "You need to run the disturbance analysis prior to download the data. ",
          easyClose = TRUE,
          footer = NULL)
        ) 
        return()
      }
      req(input$goButton >0)
      x <- data.frame(Area_km2 = rv$aoiAttributes()[1,2],
                      Lineardist_km  = rv$baseAttributes()[1,2],
                      Arealdist_km2 = rv$baseAttributes()[2,2],
                      otherLinear_km  = rv$baseAttributes()[3,2],
                      otherAreal_km2  = rv$baseAttributes()[4,2],
                      Fires_per = rv$baseAttributes()[5,2],
                      Mine_per = rv$baseAttributes()[6,2],
                      PA2021_per= rv$baseAttributes()[7,2],
                      IntactFL2000_per = rv$baseAttributes()[8,2],
                      IntactFL2020_per = rv$baseAttributes()[9,2])
      colnames(x) <-c("Area_km2","Lineardist_km","Arealdist_km2","otherLinear_km","otherAreal_km2","Fires_per", "Mines_per", "PA2021_per","IntactFL2000_per","IntactFL2020_per")
      aoi <- cbind(st_union(rv$sa()), x)
      if (!is.null(rv$layers_rv$line)) st_write(rv$layers_rv$line, dsn=file, layer='linear_disturbance', append=TRUE)
      if (!is.null(rv$layers_rv$poly)) st_write(rv$layers_rv$poly, dsn=file, layer='areal_disturbance', append=TRUE)
      if (!is.null(rv$layers_rv$fires)) st_write(rv$layers_rv$fires, dsn=file, layer='fires', append=TRUE)
      if (!isFALSE(rv$other_linedist())) st_write(rv$other_linedist(), dsn=file, layer='other_linear_disturbances', append=TRUE)
      if (!isFALSE(rv$other_polydist())) st_write(rv$other_polydist(), dsn=file, layer='other_areal_disturbances', append=TRUE)
      if (!is.null(rv$layers_rv$pa2021)) st_write(rv$layers_rv$pa2021, dsn=file, layer='protected_areas', append=TRUE)
      if (!is.null(rv$layers_rv$placers)) st_write(rv$layers_rv$placers, dsn=file, layer='Placer_Claims', append=TRUE)
      if (!is.null(rv$layers_rv$quartz)) st_write(rv$layers_rv$quartz, dsn=file, layer='Quartz_Claims', append=TRUE)
      if (!is.null(rv$layers_rv$mines)) st_write(rv$layers_rv$mines, dsn=file, layer='mining_claims', append=TRUE)
      if (!is.null(rv$layers_rv$herds)) st_write(rv$layers_rv$herds, dsn=file, layer='Caribou_Herds', append=TRUE)
      if (!is.null(rv$display1_sf())) st_write(rv$display1_sf(), dsn=file, layer=rv$display1_name(), append=TRUE)
      if (!is.null(rv$display2_sf())) st_write(rv$display2_sf(), dsn=file, layer=rv$display2_name(), append=TRUE)
      if (!is.null(rv$display3_sf())) st_write(rv$display3_sf(), dsn=file, layer=rv$display3_name(), append=TRUE)
      
      if (input$goButton) {
        x <- data.frame(Undisturbed_per = rv$additionalAttributes()[1,2],
                        Disturbed_per  = rv$additionalAttributes()[2,2])
        colnames(x) <-c("Undisturbed_per", "Disturbed_per")
        aoi <- cbind(aoi, x)
        st_write(aoi, dsn=file, layer='studyarea', append=FALSE)
        st_write(rv$intactness_sf(), dsn=file, layer='undisturbed', append=TRUE)
        if (!is.null(rv$footprintfire_sf())){
          st_write(rv$footprintfire_sf(), dsn=file, layer='disturbed', append=TRUE)
        } else{
          st_write(rv$footprint_sf(), dsn=file, layer='disturbed', append=TRUE)
        }
      }
    }
  )
  
  # Download report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Disturbance_explorer_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      
      tmp_html <- tempfile(fileext = ".html")
      
      file.copy(file.path("www", "preview.html"), tmp_html, overwrite = TRUE)
      
      req(file.exists(tmp_html))
      
      pagedown::chrome_print(
        input  = normalizePath(tmp_html, winslash = "/", mustWork = TRUE),
        output = file
      )
    }
  )
  
}
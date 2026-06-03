options(shiny.maxRequestSize=500*1024^2) 

library(leaflet)
library(tidyverse)
library(cli)
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(shinyjs)
library(markdown)
library(dplyr)
library(tidyr)
library(sf)
library(shinyMatrix)
library(utils)
library(tmap)
library(pagedown)
library(kableExtra)
library(knitr)

# Define the last update date (git last commit)
app_version_date <- as.Date(readLines("app_version.txt"))

# Read the Markdown file
overview_md <- readLines("docs/overview.md", encoding = "UTF-8")

# Replace placeholder in the Markdown
overview_md <- c(
  paste0('<div style="text-align: right; font-size:0.9em; color: gray;">Last update: ', app_version_date, '</div>'),
  overview_md
)

# Convert to a single string for rendering
overview_md_text <- paste(overview_md, collapse = "\n")

r_files <- list.files(
  file.path(getwd(), "R"),
  pattern = "\\.[rR]$",
  full.names = TRUE
)

invisible(lapply(r_files, source))

isMappable <- function(x) {
  !is.null(x) && inherits(x, "sf") && nrow(x) > 0
}

stats <- tibble(Name = character(),
                Area_km2 = numeric(),
                Undisturbed_per = numeric(),
                Disturbed_per = numeric(),
                Lineardist_km = numeric(),
                Arealdist_km2 = numeric(),
                otherLinear_km = numeric(),
                otherAreal_km2 = numeric(),
                Fires_per = numeric(),
                Mines_per = numeric(),
                PA2021_per = numeric(),
                IntactFL2000_per = numeric(),
                IntactFL2020_per = numeric(),
                set_custom = character(),
                set_includeOthers = character(),
                set_mines = character(),
                set_fires = character()
)


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

#viridis
gradient_rast1 <- "display:inline-block; width:80px; height:15px; 
                 background: linear-gradient(to right, #440154, #31688e, #35b779, #fde725);
                 margin-right:8px; border:1px solid #000;"

#heat
gradient_rast2 <- "display:inline-block; width:80px; height:15px; 
                 background: linear-gradient(to right, #0000FF, #FFFF00, #FF0000);
                 margin-right:8px; border:1px solid #000;"

add_tm_layer <- function(map, obj, color, alpha = 1) {
  if (is.null(obj)) return(map)
  geom_type <- unique(sf::st_geometry_type(obj))
  
  if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    map <- map +
      tm_shape(obj) +
      tm_polygons(col = color, alpha = alpha,  border.col = NA, lwd=0)
  } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
    map <- map +
      tm_shape(obj) +
      tm_lines(col = color, lwd = 2)
  } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
    map <- map +
      tm_shape(obj) +
      tm_symbols(col = color, fill = color, size = 0.8, shape = 21)
  }
  map
}

build_map <- function(rv){
  tmap_mode("plot")
  
  visible <- rv$visible_groups()

  # palette 
  fire_pal <- c(
    "Lightning" = "#996633",
    "Human" = "#663300",
    "Unknown" = "pink"
  )
  
  sa <- rv$sa()
  sa_buff <- sf::st_buffer(sa, dist = 20000)
  bb <- sf::st_bbox(sa_buff)
  
  # start map
  map <- tm_shape(sa) +
    tm_polygons(
      col = '#663399',
      lwd = 3
    ) +
    tm_view(
      bbox = bb
    )
  
  # Protected areas
  if ("Protected areas" %in% visible) {
    poly <- rv$statslayers_rv$pa2021
    map <- map +
      tm_shape(poly) +
      tm_polygons(
        col = '#699999',
        border.col = NA,
        alpha = 1,
        lwd = 0,
        border.col = '#699999'
      )
  }
  
  # Intact FL 2000
  if ("Intact FL 2000" %in% visible) {
    poly <- rv$statslayers_rv$ifl2000
    map <- map +
      tm_shape(poly) +
      tm_polygons(
        col = '#3366FF',
        border.col = NA,
        lwd= 0,
        alpha = 1
      )
  }
  
  # Intact FL 2020
  if ("Intact FL 2020" %in% visible) {
    poly <- rv$statslayers_rv$ifl2020
    map <- map +
      tm_shape(poly) +
      tm_polygons(
        col = '#000066',
        border.col = NA,
        lwd= 0,
        alpha = 1
      )
  }
  
  if (!is.null(rv$display1_name()) && rv$display1_name() %in% visible) {
    display1_sf <- rv$display1_sf()
    map <- add_tm_layer(map, sf::st_transform(display1_sf, 4326),  "#EE6363")
  }
  
  if (!is.null(rv$display2_name()) && rv$display2_name() %in% visible) {
    display2_sf <- rv$display2_sf()
    map <- add_tm_layer(map, sf::st_transform(display2_sf, 4326),  "#33006689")
  }
  
  if (!is.null(rv$display3_name()) && rv$display3_name() %in% visible) {
    display3_sf <- rv$display3_sf()
    map <- add_tm_layer( map, sf::st_transform(display3_sf, 4326), "#003333")
  }
  
  if (!is.null(rv$rast1_name()) && rv$rast1_name() %in% visible) {
    rast1 <- rv$rast1()
    map <- map +
      tm_shape(rast1) +
      tm_raster(palette = viridis::viridis(256), alpha = 0.5)
  }
  
  if (!is.null(rv$rast2_name()) && rv$rast2_name() %in% visible) {
    rast2 <- rv$rast2()
    map <- map +
      tm_shape(rast2) +
      tm_raster(palette = colorRampPalette(c("#0000FF", "#FFFF00", "#FF0000"))(256), alpha = 0.5)
  }
#  # Lakes
#  if ("Lakes" %in% visible) {
#    map <- map +
#      tm_shape(lakes) +
#      tm_polygons(col = "#97FFFF", alpha = 0, border.col = "#97FFFF", lwd = 2)
#  }
  
#  # Rivers
#  if ("Rivers" %in% visible) {
#    map <- map +
#      tm_shape(rivers) +
#      tm_lines(col = "#97FFFF", lwd = 2)
#  }
  
  # Placers Claims
  if ("Placer claims" %in% visible) {
    placers <- rv$layers_rv$placers
    map <- map +
      tm_shape(placers) +
      tm_polygons(
        col = '#333333',
        border.col = NA,
        lwd= 0,
        alpha = 1
      )
  }
  
  # Quartz Claims
  if ("Quartz claims" %in% visible) {
    quartz <- rv$layers_rv$quartz
    map <- map +
      tm_shape(quartz) +
      tm_polygons(
        col = '#999999',
        border.col = NA,
        lwd= 0,
        alpha = 1
      )
  }
  
  # Mining Claims
  if ("Mining claims" %in% visible) {
    mines <- rv$statslayers_rv$mines
    map <- map +
      tm_shape(mines) +
      tm_polygons(
        col = '#666666',
        border.col = NA,
        lwd= 0,
        alpha = 1
      )
  }
  
  # Areal disturbances
  if ("Areal disturbances" %in% visible) {
    poly <- rv$statslayers_rv$poly
    map <- map +
      tm_shape(poly) +
      tm_polygons(
        col = '#660000',
        border.col = NA,
        lwd= 0,
        alpha = 1
      )
  }
  
  # Linear disturbances
  if ("Linear disturbances" %in% visible) {
    line <- rv$statslayers_rv$line
    map <- map +
      tm_shape(line) +
      tm_lines(col = "#CC3333", lwd = 1)
  }
  
  # Fires
  if ("Fires" %in% visible) {
    fires <- rv$statslayers_rv$fires
    fires$CAUSE_LABEL <- dplyr::case_when(
      fires$CAUSE == "Natural" ~ "Lightning",
      fires$CAUSE == "Human" ~ "Human",
      is.na(fires$CAUSE) | fires$CAUSE == "Undetermined" ~ "Unknown",
      TRUE ~ "Unknown"  # Catch any other unexpected cases
    )
    
    map <- map +
      tm_shape(fires) +
      tm_polygons(
        col = "CAUSE_LABEL",
        border.col = NA,
        lwd= 0,
        palette = fire_pal,
        alpha = 1,
        legend.show = FALSE
      )
  }
  
  #######################
  # Footprint
  #######################
  if ("Disturbed areas (human)" %in% visible) {
    footprint_sf <- rv$footprint_sf()
    map <- map +
      tm_shape(footprint_sf) +
      tm_polygons(
        col = 'black',
        border.col = NA,
        lwd= 0,
        alpha = 0.5
      )
  }
  if ("Disturbed areas (human + fires)" %in% visible) {
    footprintfire_sf <- rv$footprintfire_sf()
    map <- map +
      tm_shape(footprintfire_sf) +
      tm_polygons(
        col = 'black',
        border.col = NA,
        lwd= 0,
        alpha = 0.5
      )
  }
  if ("Undisturbed areas" %in% visible) {
    intactness_sf <- rv$intactness_sf()
    map <- map +
      tm_shape(intactness_sf) +
      tm_polygons(
        col = '#336633',
        border.col = NA,
        lwd= 0,
        alpha = 0.5
      )
  }
  
  #######################
  # Other dist
  #######################
  if ("Other linear disturbances" %in% visible) {
    other_line <- rv$other_linedist()
    map <- map +
      tm_shape(other_line) +
      tm_lines(col = "#FF6600", lwd = 1)
  }
  if ("Other areal disturbances" %in% visible) {
    other_poly <- rv$other_polydist()
    map <- map +
      tm_shape(other_poly) +
      tm_polygons(col = "#FF9966", 
                  border.col = NA,
                  lwd= 0,
                  alpha = 0.5
      )
  }
  
  # final layout
  final_map <- map +
    tm_layout(
      frame = FALSE,
      legend.outside = TRUE,
      main.title.position = "center"
    )
}

square_box <- function(x, y, size_in_in = 0.25, col) {
  # Convert from inches to user coordinates
  x1 <- grconvertX(x, "ndc", "user")
  y1 <- grconvertY(y, "ndc", "user")
  w  <- grconvertX(size_in_in, "inches", "user") - grconvertX(0, "inches", "user")
  h  <- grconvertY(size_in_in, "inches", "user") - grconvertY(0, "inches", "user")
  
  rect(x1, y1, x1 + w, y1 + h, col = col, border = NA)
}


get_geom_type <- function(x) {
  g <- unique(sf::st_geometry_type(x))
  
  if (any(g %in% c("POLYGON", "MULTIPOLYGON"))) return("polygon")
  if (any(g %in% c("LINESTRING", "MULTILINESTRING"))) return("line")
  if (any(g %in% c("POINT", "MULTIPOINT"))) return("point")
  
  return(NA)
}

create_legend_png <- function(path, visible_layers, display1, display1_name, display2, display2_name, display3, display3_name, rast1_name, rast2_name) {
  legend_report <- data.frame(
    group = c(
      "Undisturbed areas",
      "Disturbed areas (human)", 
      "Areal disturbances", 
      "Linear disturbances", 
      "Other areal disturbances",
      "Other linear disturbances",
      "Fires", "Fires", "Fires",
      "Quartz claims", 
      "Placer claims", 
      "Mining claims",
      "Protected areas", 
      "Intact FL 2000", 
      "Intact FL 2020"
    ),
    label = c("Undisturbed areas\u00B9", 
              "Disturbed areas\u00B2", 
              "Areal disturbances", 
              "Linear disturbances", 
              "Other areal disturbances",
              "Other linear disturbances",
              "Fires (Lightning)", 
              "Fires (Human)", 
              "Fires (Unknown)",
              "Quartz claims", 
              "Placer claims", 
              "Mining claims",
              "Protected areas", 
              "Intact FL 2000", 
              "Intact FL 2020"
    ),
    color = c("#33663380", 
              "#333333", 
              "#660000", 
              "#CC3333", 
              "#FF9966",
              "#FF6600",
              "#996633", 
              "#663300", 
              "pink",
              "#999999",
              "#666666", 
              "#999999",
              "#699999",
              "#3366FF", 
              "#000066"
    ),
    type = c("polygon", 
             "polygon", 
             "polygon", 
             "line", 
             "polygon", 
             "line",
             "polygon", 
             "polygon", 
             "polygon", 
             "polygon", 
             "polygon", 
             "polygon", 
             "polygon", 
             "polygon", 
             "polygon"
    ),
    stringsAsFactors = FALSE
  )
  
  legend_report <- legend_report %>%
    dplyr::filter(group %in% visible_layers)
  
  extra_rows <- list()
  
  if (!is.null(display1) && display1_name %in% visible_layers) {
    extra_rows[[length(extra_rows) + 1]] <- data.frame(
      group = "display1",
      label = display1_name,
      color = "#EE6363",
      type = get_geom_type(display1)
    )
  }
  
  if (!is.null(display2) && display2_name %in% visible_layers) {
    extra_rows[[length(extra_rows) + 1]] <- data.frame(
      group = "display2",
      label = display2_name,
      color = "#33006689",
      type = get_geom_type(display2)
    )
  }
  
  if (!is.null(display3) && display3_name %in% visible_layers) {
    extra_rows[[length(extra_rows) + 1]] <- data.frame(
      group = "display3",
      label = display3_name,
      color = "#003333",
      type = get_geom_type(display3)
    )
  }
  
  extra_rows <- do.call(rbind, extra_rows)
  legend_report <- rbind(legend_report, extra_rows)
  n <- nrow(legend_report)
  
  png(path, width = 600, height = 800, res = 120)
  par(mar = c(1, 1, 1, 1), oma = c(0, 0, 0, 0))
  plot(NULL, xlim = c(0, 1), ylim = c(0, n+2), xaxt = "n", yaxt = "n",
       xlab = "", ylab = "", bty = "n")
  
  box_size <- 0.5   # square size in row units
  
  for (i in seq_len(n)) {
    # Row center
    y <- n - i + 0.5
    
    # Draw square
    if (legend_report$type[i] == "polygon") {
      rect(0.05, y - box_size/2, 0.2,  y + box_size/2, col = legend_report$color[i], border = NA)
    } else if (legend_report$type[i] == "line") {
      segments(0.05, y, 0.2, y, col = legend_report$color[i], lwd = 3)
    }else{
      points(0.125, y, pch = 21,  bg = legend_report$color[i], col = "black", cex = 3)
    }
    
    # Draw text (bigger font OK)
    text(0.25, y, legend_report$label[i], pos = 4, cex = 1.4)
  }
  
  dev.off()
}

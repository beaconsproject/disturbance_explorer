

# Check and install packages if missing
required_packages <- c("leaflet", "tidyverse", "cli","shinydashboard", "shinycssloaders", "shiny", "shinyjs",
   "markdown", "dplyr", "tidyr", "sf", "shinyMatrix"
)

options(shiny.maxRequestSize=500*1024^2) 

# Install any missing packages
#missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
#if (length(missing_packages) > 0) {
#  install.packages(missing_packages)
#}

# Load the packages
invisible(lapply(required_packages, library, character.only = TRUE))

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

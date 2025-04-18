library(sf)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(shinyjs)
library(markdown)
library(shinyMatrix)

options(shiny.maxRequestSize=100*1024^2) 
fda_list <- c("fda10ab","fda10ad")
m1 <- as.matrix(read_csv('docs/cas.csv')[42:66,2:4]) #%>% filter(TYPE_DISTURBANCE %in% x1))
m2 <- as.matrix(read_csv('docs/cas.csv')[1:41,2:4]) #%>% filter(TYPE_DISTURBANCE %in% x2))


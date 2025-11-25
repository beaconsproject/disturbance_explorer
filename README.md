# Disturbance Explorer

April 24, 2025

Disturbance Explorer is an R shiny app that enables users to generate undisturbed areas maps for past, present, and future landscapes using human surface disturbance and/or natural disturbance data. The app includes the following functionality:

-   Upload existing and potential future human disturbances (linear and areal), natural disturbances (e.g., wildfire), and an area of interest (AOI) as a geopackage
-   Identify undisturbed areas with the option to apply user-defined buffers of influence around human linear or areal disturbances
-   Reconstruct historic landscapes using aged human disturbance datasets
-   Explore future landscapes by incorporating potential future human disturbances e.g., mining claims, forest harvest plans
-   Generate and compare statistics describing disturbance for a range of disturbance scenarios e.g., area and length of human disturbances by disturbance type, area burned, area of mining claimings, etc.
-   Export statistics (csv table) and geopackage of undisturbed and disturbed areas maps for use in QGIS or ArcGIS

**Demo Dataset**

The App includes an embedded demo dataset for a watershed in the southeast Yukon.   

## Running the App

The app is located at: <https://beaconsproject.shinyapps.io/disturbance_explorer>

The app can also be run from a local machine using the following steps (note, the first 2 steps only need to be run once):

1.  Install R (download from [r-project.org](https://www.r-project.org/) and follow instructions)
2.  Install the following additional packages:

```         
install.packages(c("sf","leaflet","tidyverse","shinydashboard","shinycssloaders","shinyjs","markdown","shinyMatrix"))
```

3.  Start the Shiny app:

```         
shiny::runGitHub("beaconsproject/disturbance_explorer")
```

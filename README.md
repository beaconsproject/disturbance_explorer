# Welcome to Disturbance Explorer

May 08, 2026

Understanding human activity is essential for effective conservation planning, land management, and ecological research. Undisturbed areas, often referred to as 
intact landscapes, are critical for maintaining biodiversity, supporting ecosystem services, and preserving ecological integrity. Conversely, identifying disturbed 
areas — such as those affected by roads, industrial activities, or fires — helps quantify human impact and guides conservation efforts.

**Disturbance Explorer** is an R shiny application that enables users to generate undisturbed areas maps using human surface disturbance and/or natural disturbance data. 
The application includes the following functionality:

-   Upload an area of interest (AOI) and spatial datasets representing existing or potential human disturbances (linear and areal) and natural disturbances (e.g., wildfire) as a GeoPackage.
-   Identify undisturbed areas with the option to apply user-defined zones of influence (buffers) around human linear and areal disturbances.
-   Explore future landscape scenarios by incorporating potential future disturbances such as mining claims, proposed road networks, or forest harvest plans.
-   Generate and compare statistics describing landscape disturbance under different scenarios, including the area and length of disturbances by type, burned area, mining claim extent, and other disturbance metrics.
-   Generate a report summarizing the analysis parameters and outputs used to create the undisturbed areas map.
-   Export summary statistics (CSV format) and GeoPackages containing undisturbed and disturbed area maps for use in GIS software such as QGIS or ArcGIS.

The landing page of the application provides an overview of the appllication and its key functionalities. The application also has a dedicated **Dataset Requirements** section 
describing the expected spatial data structure, including required layers, naming conventions, and attribute specifications for use in the **Disturbance 
Explorer**. In addition, a built-in demo dataset (representing watershed 10AB in southeast Yukon) is provided to allow users to familiarize themselves with 
the tool and explore its outputs.

## Running the App

The application can be accessed online at: <https://beaconsproject.shinyapps.io/disturbance_explorer>

Alternatively, the application can be run locally by downloading and running the applicayion directly from GitHub. To run the application locally
(note, the first 2 steps only need to be run once):

1.  Install R (download from [r-project.org](https://www.r-project.org/) and follow instructions)
2.  Install the required R packages:
```         
install.packages(c("sf","leaflet","tidyverse","shinydashboard","shinycssloaders","shinyjs","markdown","shinyMatrix", "cli", "shiny", "dplyr", "tidyr","utils", "tmap", "pagedown", "kableExtra", "knitr"))

```
4.  Open the project in R or RStudio
5.  Start the Shiny app:

```         
shiny::runGitHub("beaconsproject/disturbance_explorer")
```


**Citation**

BEACONs. 2025. Disturbance Explorer. BEACONs Project, University of Alberta, Edmonton, AB. and Yukon University, Whitehorse, YT. https://beaconsproject.github.io/



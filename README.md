## Regional Disturbance Mapping (RDM) Explorer

RDMExplorer is an R shiny app that enables users to generate human footprint or landscape intactness maps using regional-scale anthropogenic surface disturbance data. The app includes the following functionality:

  1. View the distribution of natural and anthropogenic surface disturbances within a fundamental drainage area (watershed);
  2. Calculate the regional human footprint (or landscape intactness) applying user-defined buffers of influence around linear or areal disturbances;
  3. Explore the effects of anthropogenic disturbances on the distribution and abundance of terrestrial ecosystems; and
  4. Explore the effects of anthropogenic disturbances on indicators of hydrological integrity and connectivity.

The app is located at: https::/beaconsproject.shinyapps.io/RDMExplorer

The app can also be run from a local machine using the following steps:

  1. Install R (download from r-project.org and follow instructions)
  2. Install the following additional packages:

    install.packages(c("sf","dplyr","terra","leaflet","shiny","shinydashboard", "rhandsontable", "shinyWidgets", ))

  3. Start the Shiny app:

    shiny::runGitHub("beaconsproject/RDMExplorer")

# Regional Disturbance Explorer

## Introduction

The purpose of the Regional Disturbance Explorer app is to enable users to interactively create a landscape intactness map or, conversely, a human footprint map. The app also allows users to assess the influence of buffer size and minimum patch size on the resulting intactness and footprint maps. Currently, the app can only be used using the demo dataset for a watershed in the southeast Yukon that is included. The dataset is described below. A future release will enable users to use their own regional disturbance data and will include instructions on how to do so. 

<center>
<img src="app.jpg" width="80%">
<br>
Figure 1. Shiny-based disturbance explorer app.
</center>

## Functionality

The app consists of four sections:

**Overview**:
  
  - Provides a description of the app, its functionality, and the demo datasets.

**Select an area of interest**:
  
  - Select an existing fundamental drainage area (FDA)
  - Upload an area of interest (AOI) polygon as a geopackage ('.gpkg')
  
**Calculate footprint/intactness**:
  
  - view linear and areal anthropogenic surface disturbances, forest fires, and mining claims
  - view a summary of the length or area of each disturbance type that occurs in the region - with or without buffers
  - compare regional estimates of intactness to those provided by Intact Forest Landscapes for the years 2000 and 2020

**Save footprint and intactness maps**:
  
  - view a map of streams, rivers, and lakes
  - view a table showing length and areas of streams, rivers, and lakes
  - explore the proportion of streams, rivers, and lakes are within a user-defined distance of anthropogenic disturbances

## Input data

### Regional disturbance map

The key input data is the regional disturbance dataset. Currently, only open source file geopackages ("gpkg") can be used. The demo dataset (fda9.gpkg) is comprised of 9 fundamental drainage areas (watersheds) located in the Yukon and British Columbia. They include the following layers. A vignette will soon be available describing how users can create a similar dataset for another region.

The **fda9.gpkg** geopackage includes the following layers:

  - FDA
  - IFL_2000
  - IFL_2020
  - Fire_History
  - Quartz_Claims
  - Areal_Features
  - Linear_Features

The **Areal_Features** layer includes the following attributes:

- TYPE_INDUSTRY
- TYPE_DISTURBANCE
- CREATED_BY
- IMAGE_DATA
- Area_ha

The **Linear_Features** layer includes the following attributes:

- TYPE_INDUSTRY
- TYPE_DISTURBANCE
- CREATED_BY
- IMAGE_DATA
- Length_km

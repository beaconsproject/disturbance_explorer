# Regional Disturbance Mapping Explorer

## Introduction

The purpose of the Regional Disturbance Mapping (RDM) Explorer app is to enable users to interactively create a landscape intactness map or, conversely, a human footprint map. The app also allows users to assess the influence of buffer size and minimum patch size on the resulting intactness and footprint maps. Currently, the app can only be used using the demo dataset for a watershed in the southeast Yukon that is included. The dataset is described below. A future release will enable users to use their own regional disturbance data and will include instructions on how to do so. 

<center>
<img src="app.jpg" width="50%">
<br>
Figure 1. Shiny-based disturbance explorer app.
</center>

## Functionality

The app consists of five sections:

1. **Overview**. Provides a description of the app, its functionality, and the demo dataset.

1. **Footprint/intactness**. What is the distribution and abundance of anthropogenic disturbances in my region (watershed)? How does the estimated abundance (cumulative impact or footprint) of disturbances change when I include a buffer of influence around linear and areal features? How do global and national footprint and intactness estimates compare to my regional estimates? What is the "human footprint" of my planning region? How does the estimated footprint vary with assumptions about about area of influence and weights?
  - View all linear, all areal, both, and individual disturbance types (linear and areal) - with or without buffers
  - Compare regional estimates to well known estimates (HFP global and national, and IFL) using similar parameters to the IFL maps (default settings)

2. **Effects on landcover**. What proportion of land cover types are within X metres of anthropogenic disturbances?
  - Table showing proportions of landscover types impacted; landcover map
  - Select landcover type to view a map showing its distribution along with buffered disturbances

3. **Effects on hydrology**. What proportion of streams, rivers, and lakes are within X metres of anthropogenic disturbances?
  - Map of streams, rivers, and lakes; table showing areas impacted

4. **Sensitivity analysis**. How do assumptions about the zone of influence of disturbances (measured using buffer sizes) affect the estimated footprint and intactness of the watershed, including the proportion of terrestrial and aquatic ecosystems that are impacted?
  - Pre-calculated sensitivity analysis with demo data; or run analysis with own data
  - Plot inverse relationship

## Input data

### Regional disturbance map

The key input data is the regional disturbance dataset. Currently, only open source file geopackages ("gpkg") can be used. The demo data (fda_10ab.gpkg) is from a watershed in the southeast Yukon. It includes the following layers. A vignette will soon be available describing how users can create a similar dataset for another region.

The **fda_10ab.gpkg** geopackage includes the following layers:

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

### Landcover map

**Land cover 2019**. High-resolution (30m) annual forest land cover maps for Canada's forested ecosystems. Land cover maps for the years 1984-2019 can be downloaded from https://opendata.nfis.org/mapserver/nfis-change_eng.html

### Hydrology data

**Fundamental drainage areas**. Consists of many hydrology layers including streams, rivers, and lakes for each fundamental drainage area (FDA) in Canada. The FDA data can be downloaded from https://open.canada.ca/data/en/dataset/a4b190fe-e090-4e6d-881e-b87956c07977
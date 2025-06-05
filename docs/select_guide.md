---
format: md
---

## Select study area

The **Select study area** panel offers two options:

- **Use demo dataset** This dataset is embedded in the app and is for a watershed located in the southeast Yukon. It includes all spatial layers required to run the app.
- **Upload a custom GeoPackage (gpkg)** Users are responsible for ensuring that all required spatial layers (and associated attributes) are included and that all layers share a consistent coordinate reference system.
Refer to the **Dataset Requirements** tab for a detailed description of required and optional layers, as well as their respective data structure. 

If a custom GeoPackage is uploaded, the app provides the option to classify linear and areal disturbances by industry (e.g., transporation) and/or disturbance (e.g., highway) type. This classification generates a table summarizing the industry and/or disturbance types and their associated length (km) or area (km2). This table is also used in the next step to assign custom buffer sizes for each industry/disturbance type combination.
<br><br>
<center><img src="pics/SelectSA.png" width="600"><br>Figure 1. Mapping study area and underlying dataset with the option to classify disturbances per industry/type.</center>
<br><br>

It is also possible for the user to upload additional (or other) linear and areal disturbances as shapefiles which can be included in the mapping workflow or used solely for visualization.

Pressing the **Confirm button** loads the spatial layers, allowing the user to toggle individual layers on and off in the map legend.

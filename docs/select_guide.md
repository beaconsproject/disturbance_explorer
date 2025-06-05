---
format: md
---

## Select study area

In this step, the user uploads spatial data into the App. 

**Select source dataset** offers two options:

1. **Use demo dataset** - This dataset is embedded in the app and is for a watershed located in the southeast Yukon. It includes all spatial layers required to run the app. If selected, click the "Confirm" button to load the demo dataset. Once loaded, the spatial layers will appear in the map. From here, move on to **Buffer features**.
   
2. **Upload a custom GeoPackage (gpkg)** - If selected, the panel options will expand. Use the drop down menu to navigate to the GeoPackage for the study area.

For option 2, users are responsible for ensuring that all required spatial layers (and associated attributes) are included in the Geopackage and that all layers share a consistent coordinate reference system.
Refer to the **Dataset Requirements** tab for a detailed description of required and optional layers, as well as their respective data structure.



Select 

If a **custom GeoPackage** is uploaded, the app provides the option to classify linear and areal disturbances by industry (e.g., transporation) and/or disturbance (e.g., highway) type. This classification generates a table summarizing the industry and/or disturbance types and their associated length (km) or area (km2). This table is also used in the next step to assign custom buffer sizes for each industry and/or disturbance type.
<br><br>
<center><img src="pics/SelectSA.png" width="600"><br>Figure 1. Mapping study area and underlying dataset with the option to classify disturbances per industry/type.</center>
<br><br>

It is also possible for the user to upload additional (or other) linear and areal disturbances as shapefiles which can be included in the mapping workflow or used solely for visualization.

Pressing the **Confirm button** loads the spatial layers, allowing the user to toggle individual layers on and off in the map legend.

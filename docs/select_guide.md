---
format: md
---

## Select study area

In this step, the user uploads spatial data into Disturbance Explorer.

**Select source dataset** offers two options:

1. **Use demo dataset** - This dataset is embedded in the app and is for a watershed located in the southeast Yukon. It includes all spatial layers required to run the app. If selected, click the **Confirm** button to load the demo dataset. Once loaded, the spatial layers will appear in a map and can be turned on and off. From here, move on to **Buffer features**.
   
2. **Upload a custom GeoPackage (gpkg)** - If selected, the panel options will expand.

   **Upload a Geopackage**: Browse to the GeoPackage for the study area and click "Open".

   Note: Users are responsible for ensuring that all required spatial layers (and associated attributes) are included in the Geopackage and that all layers share a consistent coordinate reference system.
   Refer to the **Dataset Requirements** tab for a detailed description of required and optional layers, as well as their respective data structure.

   **Define classification per disturbance type**: Click this option to classify linear and areal disturbances by industry (e.g., transporation) and/or disturbance (e.g., highway) type. This classification generates a table summarizing the industry and/or disturbance types and their associated length (km) or area (km2).    The table is located under the **Custom buffers** tab and is required in the next step if custom buffer sizes are to be applied to industry and/or disturbance types. If selected, use the drop-down menus to identify the industry and/or disturbance attributes for linear and areal disturbance features in the GeoPackage.    If attributes are not assigned, the table will summarize the features as "linear" and "areal".

   **Upload other linear disturbances (shp)** and **Upload other areal disturbances (shp)**: Here, the user has the option to upload additional linear and areal disturbances (e.g., proposed development, insect outbreaks, etc.) as shapefiles which can be included in the mapping workflow or used solely for visualization. Use the drop-down menus to browse to the shapefile. Shapefiles are comprised of multiple files, and all files must be selected (e.g., shp, shx, dbf, prj, etc.). Once selected, click "Open".

   Press the **Confirm button** to load the spatial layers. The layers will appear in a map where the user can turn layers on and off in the map legend. From here, move on to **Buffer features**.




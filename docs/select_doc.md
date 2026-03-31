---
format: md
---

## Select study area

In this step, you load the spatial data used in Disturbance Explorer.

Start by **Select source dataset**. You have two options:

1. **Use demo dataset** - This option loads a built-in dataset representing a watershed in southeast Yukon. It includes all required spatial layers, making it a quick way to explore the app.
The layers will appear on the map and can be toggled on or off. 
   
2. **Upload a custom GeoPackage (gpkg)** - Selecting this option expands additional settings for uploading your own data.

   Browse to the GeoPackage and click "Open". Then, select the studyarea layers found within the GeoPackage. 

📌 Ensure that your GeoPackage: Contains all required spatial layers and attributes, uses a consistent coordinate reference system across all layers

Refer to the Dataset Requirements tab for detailed specifications on required and optional data.

<br>   

**(OPTIONAL) Change disturbance layer classification**

Disturbances are classified by default by INDUSTRY_TYPE (e.g., transporation) and DISTURBANCE_TYPE (e.g., highway). 
This classification generates a table summarizing the industry and/or disturbance types and their associated length (km) or area (km2). 
The table is found under the **Custom buffers** tab located across the top. To change the classification, check the box and select the drop-down menus to identify 
the industry and/or disturbance attributes for linear and areal disturbance features in the GeoPackage. If attributes are not assigned, the table will simply summarize the features as "linear" and "areal".
   
*Note*: The option will be disable if the uploaded GeoPackage only include fires. 
   
Press **Submit and continue** to apply the new classification
 
<br>

**(OPTIONAL) Upload other disturbances**

Here, the user has the option to upload additional linear and areal disturbances (e.g., proposed development, insect outbreaks, etc.) as shapefiles which can be included in the mapping workflow or used solely for 
visualization. Use the drop-down menus to browse to the shapefile. Shapefiles are comprised of multiple files, and all files must be selected (e.g., shp, shx, dbf, prj, etc.). Once selected, click "Open".

Press the **Confirm upload** button to load the spatial layers. The layers will appear in a map where the user can turn layers on and off. 
   
From here, you can proceed to **Add display elements (OPTIONAL)** to include additional visualization layers, or move directly to **Buffer features** to continue the analysis.



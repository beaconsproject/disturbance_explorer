---
format: md
---

## User Guide

This step defines the key inputs to configure the analysis, including loading spatial data, disturbance classifications, and any additional user-provided data. 

### Step 1. Select study area

Start with **Select source dataset**. You have two options:

1. **Use demo dataset** - This option loads a built-in dataset representing a watershed in southeast Yukon. It includes all required spatial layers, making it a quick way to explore the app.
The layers will appear on the map and can be toggled on or off. 
   
2. **Upload a custom GeoPackage (gpkg)** - Selecting this option expands additional settings for uploading your own data.

   Browse to the GeoPackage and click "Open". Then, select the studyarea layers found within the GeoPackage. 

📌 Ensure that your GeoPackage: Contains all required spatial layers and attributes, uses a consistent coordinate reference system across all layers

Refer to the Dataset Requirements tab (in the Welcome section from the main menu) for detailed specifications on required and optional data.

Click **Confirm study area** after selecting a dataset.

<br>   

### Step 2. Change disturbance layer classification (OPTIONAL)

Disturbances are classified by default by INDUSTRY_TYPE (e.g., transportation) and DISTURBANCE_TYPE (e.g., access road). 
This classification generates a table summarizing industry and/or disturbance types in the dataset and their associated length (km) or area (km2). 
The table is found under the **Custom buffers** tab located across the top. To change the classification, check the box and select the drop-down menus to identify 
the industry and/or disturbance attributes for linear and areal disturbance features in the GeoPackage. If attributes are not assigned, the table will simply summarize the features as "linear" and "areal".
   
*Note*: This option will be disabled if the uploaded GeoPackage includes only fires as disturbance. 
   
Press **Submit and continue** to apply the new classification.
 
<br>

## Step 3.  Upload other disturbances (OPTIONAL)

Here, the user has the option to upload additional linear and areal disturbances (e.g., proposed development, insect outbreaks, etc.) as shapefiles which can be included in the mapping workflow or used solely for 
visualization. Use the drop-down menus to browse to the shapefile. Shapefiles are comprised of multiple files, and all files must be selected (e.g., shp, shx, dbf, prj, etc.). Once selected, click "Open".

Press the **Confirm upload** button to load the spatial layers. The layers will appear in a map where the user can turn layers on and off. 
   
From here, users can proceed to **Add display elements (OPTIONAL)** to include additional visualization layers, or move directly to **Buffer features** to continue the analysis.



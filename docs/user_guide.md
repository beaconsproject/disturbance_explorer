---
format: md
---

# User guide

Understanding human activity is essential for effective conservation planning, land management, and ecological research. Undisturbed areas, often referred to as 
intact landscapes, are critical for maintaining biodiversity, supporting ecosystem services, and preserving ecological integrity. Conversely, identifying disturbed 
areas—such as those affected by roads, industrial activities, or fires—helps quantify human impact and guides conservation efforts.

**Disturbance Explorer** produces undisturbed and disturbed area maps for human disturbances as well as natural disturbances if desired. 
Users can explore the influence of buffer size and minimum patch size on the resulting undisturbed and disturbed area maps. 
The **User Guide** describes the step-by-step process for creating these maps.

The **Welcome** section includes the **Overview** landing page that provides a concise introduction to the app and its functionality. It also includes this guide and **Dataset Requirements** 
which outlines the description, naming convention, and data structure of the required spatial layers used by the **Disturbance Explorer** app. 
<br><br>

To get started, click 'Select study area' on the left-side panel. 


### Select study area

In this step, the user uploads spatial data into Disturbance Explorer.

**Select source dataset** offers two options:

1. **Use demo dataset** - This dataset is embedded in the app and is for a watershed located in the southeast Yukon. It includes all spatial layers required to run the app. If selected, click the **Confirm** button to load the demo dataset. Once loaded, the spatial layers will appear in a map and can be turned on and off. From here, move on to **Buffer features**.
   
2. **Upload a custom GeoPackage (gpkg)** - If selected, the panel options will expand.

   **Upload a Geopackage**: Browse to the GeoPackage for the study area and click "Open".

  📌 Users are responsible for ensuring that all required spatial layers (and associated attributes) are included in the Geopackage and that all layers share a consistent coordinate reference system.
   Refer to the **Dataset Requirements** tab for a detailed description of required and optional layers, as well as their respective data structure.

   **Define classification per disturbance type**: Click this option to classify linear and areal disturbances by industry (e.g., transporation) and/or disturbance (e.g., highway) type. This classification generates a table summarizing the industry and/or disturbance types and their associated length (km) or area (km2).    The table is located under the **Custom buffers** tab across the top and is required in the next step if custom buffer sizes are to be applied to industry and/or disturbance types. Use the drop-down menus to identify the industry and/or disturbance attributes for linear and areal disturbance features in the GeoPackage. If attributes are not assigned, the table will simply summarize the features as "linear" and "areal".

   *Note*: The option will be disable if the uploaded GeoPackage only include fires
   
   **Upload other linear disturbances (shp)** and **Upload other areal disturbances (shp)**: Here, the user has the option to upload additional linear and areal disturbances (e.g., proposed development, insect outbreaks, etc.) as shapefiles which can be included in the mapping workflow or used solely for visualization. Use the drop-down menus to browse to the shapefile. Shapefiles are comprised of multiple files, and all files must be selected (e.g., shp, shx, dbf, prj, etc.). Once selected, click "Open".

   Press the **Confirm** button to load the spatial layers. The layers will appear in a map where the user can turn layers on and off. From here, move on to **Buffer features**.
<br><br>


### Add display elements (OPTIONAL)

This section allows users to add a maximum of three layers to the map for visualization purposes only. Additional layers can be 
uploaded as Shapefiles or as layers from a GeoPackage. These can be vector data (points, lines, or polygons) but 
cannot be rasters. Layers will appear on the map using their original names, and colors are assigned as indicated in the side panel. They aren't considered in the analysis. 

<br><br>

### Buffer features

In this step, the user specifies the disturbance layers and associated buffers for creating the undisturbed and disturbed areas maps. The disturbance layer options include linear and areal anthropogenic disturbances, other disturbances, mining claims, and fire, assuming the spatial layers have been uploaded in the previous step.

**Select buffer type:** The user has two options.

  1. **Use custom buffers** - Select this option if variable buffer widths are desired e.g., 1000-m buffer for highways and 500-m buffer for trails.
     
     If selected, the **mapview** will be replaced with the table summarizing linear and area disturbances under the **custom buffers** tab (Figure 1). Variable buffer widths are assigned in metres via the table by editing the widths in the column named "BUFFER_SIZE_M". The default buffer width assigned to all disturbances is 500 m.

     When editing is complete, select "Buffer features" on the left sidebar to continue.

  2. **Use overall buffers:** Select this option if a single buffer width is to be applied to linear and areal disturbances. Different buffer widths can be applied to the linear and areal disturbances.

     If selected, use the sliders to set buffers. The slider options provide increments of 10 m. If the slider options are not sufficient, use the custom buffers option above. 

   *Note*: The option will be disable if the uploaded GeoPackage only include fires

**Set minimum undisturbed patch size (km2)**: In the left sidebar, a minimum patch size for undisturbed areas can be set using the slider. This function will remove patches smaller than the minimum patch size from undisturbed areas map. 

**Include other disturbances**: The user can choose to include other human or natural disturbances (linear and areal) if these spatial layers were uploaded during the **Select study area** step.  To include other disturbances, check the box and set the buffer width using the slider(s). The option is disable in the absence of layer representing other disturbances. 

**Include mining claims**: If mining claims were included in the GeoPackage uploaded in the **Select study area** step, the option to include mining claims will be active. To include mining claims, check the box and set the buffer width using the slider. The option is disable in the absence of layer representing mining claims. 

**Include fires**: If fires were included in the GeoPackage uploaded in the **Select study area** step, the option to include fires will be active. To include fires, check the box and set the mininum fire size (ha), as well as fire years, to include in the generation of the undisturbed and disturbed areas maps. The option is disable in the absence of layer representing fires. 

**Generate undisturbed areas**: To create the undisturbed and disturbed area maps, click on **Generate undisturbed areas** buttom. When the app is done creating the maps, two new layers will appear in the map: (1) "disturbed" layer comprised of buffered disturbances and 
(2) "undisturbed" layer comprised of all regions within the study region that are not intersected by the disturbed areas. 
<br><br>

### Download data

Here, the user can download a GeoPackage containing spatial layers of undisturbed and disturbed areas as well as all input layers (e.g., fire).

Click the **Download data** button to download the GeoPackage. The GeoPackage will be saved to the Downloads folder.

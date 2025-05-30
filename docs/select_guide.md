---
format: md
---

## Select study area

The **Select study area** panel offers two options:

  - Use demo dataset (which is embedded in the app). This dataset represents the fundamental drainage area 10AB, located in the Yukon and British Columbia. 
  It includes all the necessary layers to run the application.
  - Upload a custom GeoPackage ('.gpkg'). When using your own dataset, you are responsible for ensuring that all required layers are included and that all 
  layers share a consistent coordinate reference system. Refer to the **Dataset Requirements** tab for a detailed description of required and optional layers, 
  as well as their respective data structure.

If a user-defined GeoPackage is uploaded, the app provides the option to classify linear and areal disturbances by industry and type. This classification generates 
a table used in the next step to assign custom buffer sizes for each industry/type combination.
<br><br>
<center><img src="pics/SelectSA.png" width="600"><br>Figure 1. Mapping study area and underlying dataset with the option to classify disturbances per industry/type.</center>
<br><br>

It is also possible for the user to upload additional linear and areal disturbances as shapefiles, which can be included in the mapping workflow or used solely for visualization.

Pressing the **Confirm** button loads the study area along with its underlying layers, allowing the user to toggle individual layers on and off in the map legend.


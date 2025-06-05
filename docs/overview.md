## Welcome to Disturbance Explorer 

**Disturbance Explorer** generates custom maps of undisturbed and disturbed landscapes, with tools to explore the influence of buffer size (or zone of influence), minimum patch size, fire, and potential future disturbances (e.g., mining claims) on these landscapes. 
A built-in **User Guide** tab provides step-by-step instructions and function descriptions, while the **Dataset Requirements** tab details data formats and spatial
layers needed to run the app.


### Input data
  
The **Disturbance Explorer** app relies on several key spatial layers contained within a single GeoPackage. A demo dataset is included with the app for a watershed in southeast Yukon, Canada. Users may upload their own GeoPackage, provided it follows the required structure. 
Please refer to the **Dataset Requirements** tab for details on the required spatial layers and associated attributes and formatting.

### Functionality
    
The app consists of three sections:



#### Select study area:

  - Use the demo dataset or upload a custom GeoPackage ("gpkg" extension) containing all required spatial layers. If a custom GeoPackage is provided, users can also upload additional linear and areal disturbance features as shapefiles. 
These can be incorporated into the mapping process or used for viewing purposes only.
    
  - View the spatial layers (i.e., linear and areal anthropogenic disturbances, fires, mining claims, etc.)

📌 Note: All layers in the GeoPackage must have the same projection. Additionally, the study area must capture the full extent of disturbance layers to ensure accurate analysis.
<br><br>

#### Buffer features:
    
  - Apply buffers to disturbance features. Choose between:
  
  
    - A uniform buffer size applied to all features, or

    - Custom buffer sizes specific to disturbance types.

  - Optional: Set a minimum patch size for undisturbed areas after buffering. Smaller patches will be removed. Default is 0 km2.
  
  - Optional: Include other linear and areal disturbances not in the GeoPackage, such as future disturbances (e.g., proposed road network and/or cutblocks) and specify a buffer.

  - Optional: Include fires and specify a minimum size threshold and the years to include.

  - Optional: Include mining claims and specify a buffer.
<br><br>
  
#### Download undisturbed and disturbed areas:
    
  - Download a GeoPackage of the **undisturbed areas** and **disturbed areas** created by the app, as well as the input spatial layers (e.g., linear and areal disturbances).
<br><br>
  
  
### Disturbance Explorer workflow diagram

The worflow diagram below provides an overview of the process.

<br><br>
<center><img src="pics/workflow.png" width="800"></center>
<br><br>

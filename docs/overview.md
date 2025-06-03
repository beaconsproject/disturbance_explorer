## Welcome to the Disturbance Explorer 

The **Disturbance Explorer** app lets users interactively generate maps of undisturbed landscape,  or alternatively, maps highlighting human disturbance. 
It also provides tools to evaluate how buffer size, minimum patch size, fire events, and potential future disturbances such as mining claims affect both undisturbed and disturbed area outputs. 
A built-in **User Guide** tab provides step-by-step instructions and function descriptions, while the **Dataset Requirements** tab details data formats and 
layers needed to run the app.


### Input data
  
The **Disturbance Explorer** app relies on several key data layers contained within a single GeoPackage. A demo dataset is included with the app for a watershed in the southeast Yukon, Canada. Users may upload their own GeoPackage, provided it follows the required structure. 
Please refer to the **Dataset Requirements** tab for details on the required layers, attributes, and formatting.

### Functionality
    
The app consists of three sections:



#### Select study area

  - Use the demo dataset or upload a custom GeoPackage ("gpkg" extension) containing all required spatial layers.

  - View the included features: linear and areal anthropogenic disturbances, forest fires, and mining claims.

If a custom GeoPackage is provided, users can also upload additional linear and areal disturbance features as shapefiles. 
These can be incorporated into the mapping process or used for viewing purposes only.

📌 Note: All layers in the GeoPackage must have the same projection. Additionally, the study area must capture the full extent of disturbance layers to ensure 
accurate analysis.
<br><br>

#### Buffer features:
    
  - Apply buffers to disturbance features. Choose between:
  
  
    - A uniform buffer applied to all features.

    - Custom buffer sizes specific to disturbance types.

  - Optional: Set a minimum patch size for undisturbed areas after buffering. Smaller patches will be removed. Default is 0 km2.
  
  - Optional: Include other linear and areal disturbances not in the GeoPackage, such as future disturbances (e.g., proposed road network and/or cutblocks) and specify a buffer.

  - Optional: Include fires and specify a minimum size threshold and the years to include.

  - Optional: Include mining claims and specify a buffer.
<br><br>
  
#### Download undisturbed and disturbed areas:
    
  - View the **undisturbed areas**, showing only patches that meet the minimum size of undisturbed areas.
  - View the **disturbed areas**.
  - Explore and compare the **proportion of undisturbed and disturbed areas**.
<br><br>
  
  
### Disturbance Explorer workflow diagram

The worflow diagram below provides an overview of the process.

<br><br>
<center><img src="pics/workflow.png" width="800"></center>
<br><br>

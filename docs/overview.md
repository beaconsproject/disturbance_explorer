## Welcome to the Disturbance Explorer 

The **Disturbance Explorer** app lets users interactively generate maps of undisturbed landscape  or, alternatively, maps highlighting human disturbances. 
It also provides tools to evaluate how buffer size, minimum patch size, fire events, and mining claims affect both undisturbed and disturbed area outputs. 
A built-in **User Guide** tab walks you step by step through each feature and its functionality, while the **Dataset Requirements** tab details the data formats and 
layers needed to run the app.


### Input data
  
The **Disturbance Explorer** app relies on several key data layers contained within a single GeoPackage. A sample dataset is included with the app, representing the 
fundamental drainage area 10ab in Canada located in the Yukon and British Columbia. Users may upload their own GeoPackage, provided it follows the required structure. 
Please refer to the **Dataset Requirements** section for details on the necessary layers, attributes, and formatting.

### Functionality
    
The app consists of three sections:



#### Select study area

  - Use the demo dataset or upload a custom GeoPackage ("gpkg" extension) containing all required spatial layers.

  - Visualize the included features: linear and areal anthropogenic disturbances, forest fires, and mining claims.

If a custom GeoPackage is provided, users can also upload additional disturbance features—either linear or areal—as shapefiles. 
These can be incorporated into the mapping process or used for visualization purposes only.

📌 Note: All layers in the GeoPackage must share the same projection. Additionally, the study area must completely overlap the extent of disturbance layers to ensure 
accurate analysis.
<br><br>

#### Buffer features:
    
  - Apply buffers around disturbance features. Choose between:
  
  
    - A uniform buffer applied to all features.

    - Custom buffer sizes specific to disturbance types.

  - Set a minimum patch size for intact areas after buffering. Smaller patches will be removed, as they are considered insufficient to support key ecological processes.

  - Include fires with the option to specify a minimum size threshold.

  - Include mining claims, with the option to apply a buffer.
<br><br>
  
#### Download undisturbed and disturbed areas:
    
  - View the **undisturbed areas**, showing only patches that meet the minimum size of intactness areas.
  - View the **disturbed areas**.
  - Explore and compare the **proportion of undisturbed and disturbed areas**.
<br><br>
  
  
### Disturbance Explorer workflow diagram

The worflow diagram below provides an overview of the process.

<br><br>
<center><img src="pics/workflow.png" width="800"><br>Workflow diagram of the Disturbance Explorer app.</center>
<br><br>

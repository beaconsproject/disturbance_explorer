## Welcome to the Disturbance Explorer 

The purpose of the Regional Disturbance Explorer app is to enable users to interactively create a landscape intactness map or, 
conversely, a human footprint map. The app also allows users to assess the influence of buffer size, minimum patch size, fires and mining claims on the resulting intactness and 
footprint maps. The dataset is described below. 

<center>
  <img src="pics/app.png" width="600">
    <br>
    Figure 1. Shiny-based disturbance explorer app.
  </center>
    
## Functionality
    
The app consists of four sections:
    
**Documentation: User guide and datasets**:
    
- Provides a description of the app, its functionality, and the description of the required datasets.
  
**Select study area**:
    
  - Use demo dataset or upload a geopackage ('.gpkg') that contains all the required layers.
  
The projection of all layers within the geopackage must be the same. Moreover, in order to generate intactness and footprint maps, 
the uploaded study area must completely overlay the disturbances extent provided within the geopackage.
  
**Buffer features**:
    
  - view linear and areal anthropogenic surface disturbances, forest fires, and mining claims included in the geopackage.
  - apply buffers around disturbance features. Users can either apply a uniform buffer or specify custom buffer sizes based on disturbance types. 
  - set a minimum patch size of intactness areas after the buffering on the disturbance features are applied. This function will remove every patch in the study area that are considered not sufficiently large to maintain key ecological processes
  - include fires. Option to specify the minimum size of fire to include.
  - include mining claims. Option to apply a buffer. 
  
**Save footprint and intactness maps**:
    
  - view a map of intactness that contain only patches that reach the minimum size of intactness areas
  - view a map of the buffered footprint 
  - explore the proportion of intactness and footprint
  
## Input data
  
The **Disturbance Explorer** app depends on several key data layers contained within one GeoPackage ("gpkg" extension). An example datasets come with the app which correspond to the fundamental drainage area 10ab in Canada located in the Yukon and British Columbia.
The user can upload his or her own dataset provided that it contains the same layers (and attributes) and is in the same format as the demo dataset. For a description of the data layers and attributes required by the app, go to the **Datasets** tab.  
  

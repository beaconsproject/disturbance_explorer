## Datasets
  
For each study area (planning region), the **Disturbance Explorer** app depends on several key data layers contained within one GeoPackage ("gpkg" extension). Several example datasets come with the app - they consist of fundamental drainage areas (FDAs or watersheds) from southeast Yukon and northeast BC. In addition, the user can upload his or her own dataset provided that it contains the same layers (and attributes) and is in the same format as the demo dataset. Below we describe the required datasets along with their attributes (if any).
  
### Required data layers

The demo datasets or user-defined datasets require the following layers within a file geopackage. All layers within the geopackage should be in the same coordinate reference system e.g., Yukon Albers equal area projection (EPSG:3579).
    
  - fda : a single polygon outlining the boundary of the study area e.g., an FDA; displayed as "Study area" in the map legend.
  - ifl_2000 : distribution of intact forest landscapes in the year 2000; displayed as "Intactness 2000" in the map legend.
  - ifl_2020 : distribution of intact forest landscapes in the year 2020; displayed as "Intactness 2020" in the map legend.
  - fires : distribution of wildfire polygons for the past 70 years; displayed as "Fires" in the map legend.
  - sd_line : linear anthropogenic surface disturbance features; displayed as "Linear disturbances" in the map legend.
  - sd_poly : areal (polygonal) anthropogenic surface disturbance features; displayed as "Areal disturbances" in the map legend.

An additional layer, which is not a user-defined layer, is available in the Mapview tab:

  - FDAs : location of FDAs that comprise the demo study regions available with the app.

#### Attributes of linear and areal disturbances

The fda, ifl_2000, ifl_2020, and fires do not have any specific requirements for attributes; however, they must all be polygonal layers. Conversely, the linear and areal disturbance layers require the following minimum set of attributes (additional attributes will be ignored):

The **Linear_Features** layer must include the following attributes:
    
  - TYPE_INDUSTRY : a text attribute describing industry type e.g., Mining, Transportation
  - TYPE_DISTURBANCE : a text attribute describing disturbance type (nested within industry type) e.g., Survey / Cutline, Access Road
  
The **Areal_Features** layer must include the following attributes:
    
  - TYPE_INDUSTRY : a text attribute describing industry type e.g., Mining, Transportation
  - TYPE_DISTURBANCE : a text attribute describing disturbance type (nested within industry type) e.g., Drill Pad, Clearing

Additional attributes may be added in the near future.

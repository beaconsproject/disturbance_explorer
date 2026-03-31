## Datasets
  
This page outlines the required and optional spatial layers used by **Disturbance Explorer**. The required layers are uploaded via a single GeoPackage. 
All layers in the GeoPackage must use a projected coordinate system (not geographic/lat-long) and must share the same coordinate reference system (CRS) to ensure 
accurate distance measurements and consistent mapping across all layers. For accurate analysis, the study area must contain the full extent of disturbance layers.
Optional layers are uploaded in the same Geopackage with the exception of **Other disturbances** (see below).

<br> 

### Map layers 

For the app to recognize the spatial layers in the GeoPackage, the layer names **must exactly match the expected names shown below.** 

#### Required 

- A polygon that define the extent of the analysis (referred as study area in the app) e.g., watershed, ecoregion or any other user-defined area.


- At least one of the following disturbance layers:

  In the GeoPackage:
  
  - **linear_disturbance**: Linear human surface disturbance features e.g., roads, seismic line. 
  - **areal_disturbance**: Areal (polygonal) human surface disturbance features e.g., mine, town site, cutblock. 
  - **fires** : Distribution of wildfire polygons. The layer must have the following two attributes: (1) "YEAR" - an integer  which is the ignition year of the
  fire e.g., 1995 and (2) "CAUSE" - cause of ignition of the fire which can either be 'Human', 'Lightning' or 'Unknown'.  
  

  Or as shapefile:
  
  - **Other disturbances** (linear and areal): Only an overall buffer size can be applied to those disturbances. There are no requirements for the shapefile name.

📌 **Other disturbances** layers do not need to use the same coordinate system as the study area. The app will automatically reproject these layers to match the study 
area’s coordinate system during processing, ensuring consistent alignment on the map.

<br>

#### Optional - GeoPackage 

Bold text is the required name of the spatial layer in the GeoPackage in order to be recognized by the app.

- **protected_areas**: Distribution of protected areas e.g., Canadian Protected and Conserved Areas Database. 
- **Quartz_Claims**: Quartz mining claims for hard rock mining. 
- **Placer_Claims**: Placer mining claims for the mining of waterways (e.g., stream beds, wetlands) for mineral deposits (e.g., gold).
- **Mining_Claims**: Generic option for mining claims.
- **Intact_FL_2000**: Distribution of intact forest landscapes in the year 2000 (Potapov et al. 2017).
- **Intact_FL_2020**: Distribution of intact forest landscapes in the year 2020 (Potapov et al. 2017).
<br>

#### Optional - Shapefile

If disturbances are provided in the GeoPackage, the user can still upload extra features using the shapefile upload and include them in the analysis. There are no requirements for the shapefile name.

<br>

### Demo Dataset

The app comes with a built-in demo dataset for users who want to explore the functionality without uploading their own data. This demo represents a watershed located
in southeast Yukon, Canada, and includes all the essential spatial layers required to run the analysis. The dataset is designed to showcase how the app handles 
different disturbance types and spatial operations. It contains the following spatial layers:

- studyarea 
- streams: https://map-data.service.yukon.ca/GeoYukon/Base/Watercourses_1M/
- linear_disturbance: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/Surface_Disturbance_Linear_Features/
- areal_disturbance: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/Surface_Disturbance_Areal_Features/
- Quartz_Claims: https://map-data.service.yukon.ca/geoyukon/Mining/Quartz_Claims_50k/
- Placer_Claims: https://map-data.service.yukon.ca/geoyukon/Mining/Placer_Claims_50k/
- fires: https://cwfis.cfs.nrcan.gc.ca/datamart
- protected_areas: https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c
- Intact_FL_2000*: https://intactforests.org
- Intact_FL_2020*:  https://intactforests.org
<br><br>

### References

*Potapov, P., Hansen, M. C., Laestadius L., Turubanova S., Yaroshenko A., Thies C., Smith W., Zhuravleva I., Komarova A., Minnemeyer S., Esipova E. The last frontiers of wilderness: Tracking loss of intact forest landscapes from 2000 to 2013. Science Advances, 2017; 3:e1600821

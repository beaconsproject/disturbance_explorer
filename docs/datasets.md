## Datasets
  
This page outlines the required and optional spatial layers that are used by **Disturbance Explorer**. The required layers are uploaded via a single GeoPackage. For the app to recognize the spatial layers in the GeoPackage, the layer names **must exactly match the expected names shown below.** All spatial layers must have the same projection.
  
### Map layers

#### Required - GeoPackage

- **studyarea** : A polygon of the study area e.g., watershed, ecoregion or any other user-defined area.
- **linear_disturbance and/or areal_disturbance** 
  - **linear_disturbance** : Linear anthropogenic surface disturbance features e.g., roads, seismic line. Example source: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/Surface_Disturbance_Linear_Features/
  - **areal_disturbance** : Areal (polygonal) anthropogenic surface disturbance features e.g., mine, town site, cutblock. Example source: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/Surface_Disturbance_Areal_Features/

#### Optional - GeoPackage 

Bold text is the required name of the spatial layer in the GeoPackage.

- **fires** : Distribution of wildfire polygons. The layer must have the following two attributes: (1) "YEAR" - an integer  which is the ignition year of the fire e.g., 1995 and (2) "CAUSE" - cause of ignition of the fire which can either be 'Human', 'Lightning' or 'Unknown'. Example source: https://cwfis.cfs.nrcan.gc.ca/datamart
- **protected_areas** : Distribution of protected areas e.g., Canadian Protected and Conserved Areas Database. Example source: https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c
- **Quartz_Claims** : Quartz mining claims. Example source: https://map-data.service.yukon.ca/geoyukon/Mining/Quartz_Claims_50k/
- **Placer_Claims** : Placer mining claims. Example source: https://map-data.service.yukon.ca/geoyukon/Mining/Placer_Claims_50k/
- **Mining_Claims** : Mining claims.
- **Intact_FL_2000** : Distribution of intact forest landscapes in the year 2000. Example source: https://intactforests.org
- **Intact_FL_2020** : Distribution of intact forest landscapes in the year 2020. Example source: https://intactforests.org

#### Optional - Shapefile

There are no requirements for the shapefile name.

- **Other disturbances** (linear and areal) can be uploaded as shapefiles and included in the analysis. However, only an overall buffer size can be applied to additional disturbances.


📌 Note: All layers in the GeoPackage and the shapefiles for **Other disturbances** must have the same projection. Additionally, the study area must contain the full extent of disturbance layers for
accurate analysis.

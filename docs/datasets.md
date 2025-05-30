## Datasets
  
This page outlines the required and optional layers that are used by the **Disturbance Explorer** app. For the app to recognize uploaded GeoPackage layers, the layer names **must exactly match the expected names.**
  
### Map layers

#### Required

- **studyarea** : A single polygon outlining the boundary of the study area e.g., a watershed or ecoregion or any other user-defined area.
- **linear_disturbance or areal_disturbance** 
  - **linear_disturbance** : Linear anthropogenic surface disturbance features. Example source: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/
  - **areal_disturbance** : Areal (polygonal) anthropogenic surface disturbance features. Example source: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/

#### Optional

- **fires** : Distribution of wildfire polygons for the past 70 years. Example source: https://cwfis.cfs.nrcan.gc.ca/datamart
- **Intact_FL_2000** : Distribution of intact forest landscapes in the year 2000. Example source: https://intactforests.org
- **Intact_FL_2020** : Distribution of intact forest landscapes in the year 2020. Example source: https://intactforests.org
- **protected_areas** : Distribution of protected areas from the Canadian Protected and Conserved Areas Database (CPCAD). Example source: https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c
- **Quartz_Claims** : Active quartz mining claims. Example source: https://map-data.service.yukon.ca/geoyukon/Mining/Quartz_Claims_50k/
- **Placer_Claims** : Active placer mining claims. Example source: https://map-data.service.yukon.ca/geoyukon/Mining/Placer_Claims_50k/
- **Mining_Claims** : Active mining claims. 


Other disturbances can be uploaded as Shapefiles and included in the analysis; however, only an overall buffer size can be applied to additional disurbances.


📌 Note: All layers in the GeoPackage must share the same projection. Additionally, the study area must completely overlap the extent of disturbance layers to ensure 
accurate analysis.

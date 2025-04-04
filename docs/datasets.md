## Datasets
  
This page describes the required and optional map layers that are used by the Disturbance Explorer apps.
  
### Map layers

#### Required

- **studyarea** : A single polygon outlining the boundary of the study area e.g., a watershed or ecoregion or any other user-defined area.
- **linear_disturbances** : Linear anthropogenic surface disturbance features. Available from: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/
- **areal_disturbances** : Areal (polygonal) anthropogenic surface disturbance features. Available from: https://map-data.service.yukon.ca/geoyukon/Environmental_Monitoring/

#### Optional - projected

- fires : Distribution of wildfire polygons for the past 70 years. Available from: https://cwfis.cfs.nrcan.gc.ca/datamart
- **intactness_2000** : Distribution of intact forest landscapes in the year 2000. Available from: https://intactforests.org
- **intactness_2020** : Distribution of intact forest landscapes in the year 2020. Available from: https://intactforests.org
- **protected_areas** : Distribution of protected areas from the Canadian Protected and Conserved Areas Database (CPCAD). Available from: https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c
- **Quartz_Claims** : Active quartz mining claims. Available from: https://map-data.service.yukon.ca/geoyukon/Mining/Quartz_Claims_50k/
- **Place_Claims** : Active placer mining claims. Available from: https://map-data.service.yukon.ca/geoyukon/Mining/Placer_Claims_50k/

### Attributes of linear and areal disturbances

The **linear_disturbances** and **areal_disturbances** layers include the following two attributes which have to be present to use the Disturbance Explorer and Hydrology Explorer apps (additional attributes will be ignored):

- TYPE_INDUSTRY : a text attribute describing industry type e.g., Mining, Transportation
- TYPE_DISTURBANCE : a text attribute describing disturbance type (nested within industry type) e.g., Survey / Cutline, Access Road, Drill Pad, Clearing
  
---
format: md
---


## Buffer features

In this step, the user specifies the disturbance layers and associated buffers for creating the undisturbed and disturbed areas maps. The disturbance layer options 
include linear and areal anthropogenic disturbances, other disturbances, mining claims, and fire, assuming the spatial layers have been uploaded in the previous step.

**Select buffer type:** The user has two options.

  1. **Use custom buffers** - Select this option to assign different buffer widths to specific disturbance classes (e.g., 1000 m for highways and 500 m for trails).
Custom buffers are based on the disturbance classification defined in the previous step. By default, disturbances are grouped by industry and disturbance type. 
If you modified this classification earlier, the table will reflect your updated categories, allowing you to assign buffer sizes to each class.
When selected, the app automatically switches to the Custom buffers tab, where a table summarizes linear and areal disturbances. You can edit buffer widths (in metres) 
directly in the BUFFER_SIZE_M column. The default buffer width for all classes is 500 m.

  2. **Use overall buffers:** Select this option if a single buffer width is to be applied to linear and areal disturbances. Different buffer widths can be applied to the linear and areal disturbances.

     Use the sliders to set buffers. The slider options provide increments of 10-m. If the slider options are not sufficient, use the custom buffers option above. 

   *Note*: The option will be disable if the uploaded GeoPackage only include fires
   
<br>

**Set minimum undisturbed patch size (km2)**: In the left sidebar, a minimum patch size for undisturbed areas can be set using the slider. This function will remove patches smaller than the minimum patch size from the undisturbed areas map. 

<br>

**Include other disturbances**: If additional human or natural disturbance layers (linear or areal) were uploaded during the **Select study area** step, you can include 
them in the analysis. Check the box and use the sliders to assign buffer widths. This option is disabled if no “other disturbance” layers are available.

<br>

**Include mining claims**: If mining claims are present in the uploaded GeoPackage, this option becomes available. Check the box and set a buffer width to 
include them in the analysis. This option is disabled if no mining claims layer is provided.

<br>

**Include fires**: If fire data are included in the uploaded GeoPackage, this option becomes available. Check the box and define the minimum fire size (ha) 
and the range of years to include. This option is disabled if no fire layer is provided. 

<br>

📌 Buffered linear and areal anthropogenic disturbances, along with any selected optional disturbances (e.g., other uploaded disturbances, mining claims, and fires), are combined to define the disturbed areas. 
All remaining regions within the study area that do not intersect these buffered disturbances are considered undisturbed.

<br>

Press the **Generate undisturbed areas** to create the undisturbed and disturbed area maps. The two new layers (disturbed areas and undisturbed areas) will appear on the map. 


### View statistics

The "Statistics" table on the right panel provides stats on the area and length of disturbances in the study area, as well as the percent area of fires, mining and protected areas if provided. 
If Intact Forest Landscapes (IFL) for the years 2000 and 2020 are available, users can compare their generated undisturbed areas to these reference datasets.

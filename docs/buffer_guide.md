---
format: md
---


## Buffer features

In this step, the user specifies the disturbance layers and associated buffers for creating the undisturbed and disturbed areas maps. The disturbance layer options include linear and areal anthropogenic disturbances, other disturbances, mining claims, and fire, assuming the spatial layers have been uploaded in the previous step.

**Select buffer type:** The user has two options.

  1. **Use custom buffers** - Select this option if variable buffer widths are desired e.g., 1000-m buffer for highways and 500-m buffer for trails.
     
     If selected, the **mapview** will be replaced with the table summarizing linear and area disturbances under the **custom buffers** tab (Figure 1). Variable buffer widths are assigned in metres via the table by editing the widths in the column named "BUFFER_SIZE_M". The default buffer width assigned to all disturbances is 500 m.

     When editing is complete, select "Buffer features" on the left sidebar to continue.

  2. **Use overall buffers:** Select this option if a single buffer width is to be applied to linear and areal disturbances. Different buffer widths can be applied to the linear and areal disturbances.

     If selected, use the sliders to set buffers. The slider options provide increments of 10 m. If the slider options are not sufficient, use the custom buffers option above. 

**Set minimum undisturbed patch size (km2):**

In the left sidebar, a minimum patch size for undisturbed areas can be set using the slider. This function will remove patches smaller than the minimum patch size from undisturbed areas map. 

**Include other disturbances**

The user can choose to include other human or natural disturbances (linear and areal) if these spatial layers were uploaded during the **Select study area** step.  To include other disturbances, check the box and set the buffer width using the slider(s). 

**Include mining claims**

If mining claims were included in the GeoPackage uploaded in the **Select study area** step, the option to include mining claims will be active. To include mining claims, check the box and set the buffer width using the slider. 

**Include fires**

If fires were included in the GeoPackage uploaded in the **Select study area** step, the option to include fires will be active. To include fires, check the box and set the mininum fire size (ha), as well as fire years, to include in the generation of the undisturbed and disturbed areas maps. 

#### Generate undisturbed areas

To create the undisturbed and disturbed area maps, click on **Generate undisturbed areas**. When the App is done creating the maps, two new layers will appear in the map: (1) "disturbed" layer comprised of buffered disturbances and 
(2) "undisturbed" layer which shows all regions within the study region that are not intersected by the disturbed areas. 

#### View statistics

The "Statistics" table on the right panel provides stats on the area and length of disturbances in the area of interest, as well as the percent coverage of fires, mining and protected areas if provided. 
If Intact Forest Landscapes (IFL) for the years 2000 and 2020 are available, users can compare their generated undisturbed areas to these reference datasets.

<br><br>
<center><img src="pics/stats.png" width="600"><br>Figure 5. Statistics on generated undisturbed and disturbed areas.</center>
<br><br>

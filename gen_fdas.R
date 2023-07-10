# Create FDA-level GPKGs
# PV 2023-07-10

library(sf)
library(dplyr)

w <- st_read('www/fda10.gpkg', 'fda', quiet=T)
v <- st_layers('www/fda10.gpkg')
for (j in w$FDA) {
    cat("Clipping", j,':\n'); flush.console()
    fda <- filter(w, FDA==j)
    st_write(fda, paste0('www/fda',tolower(j),'.gpkg'), 'fda', quiet=T, delete_layer=T)
    x <- st_read('www/fda10.gpkg', 'sd_line', quiet=T)
    y <- st_intersection(x, fda)
    y <- st_cast(y, 'MULTILINESTRING')
    st_write(y, paste0('www/fda',tolower(j),'.gpkg'), 'sd_line', quiet=T, delete_layer=T)
    x <- st_read('www/fda10.gpkg', 'sd_poly', quiet=T)
    y <- st_intersection(x, fda)
    y <- st_cast(y, 'MULTIPOLYGON')
    st_write(y, paste0('www/fda',tolower(j),'.gpkg'), 'sd_poly', quiet=T, delete_layer=T)
}

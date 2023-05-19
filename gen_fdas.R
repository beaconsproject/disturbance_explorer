# Create FDA-level GPKGs (optional)
# PV 2023-05-19

library(sf)
library(dplyr)

w <- st_read('www/fda9.gpkg', 'fda', quiet=T)
v <- st_layers('www/fda9.gpkg')
for (j in w$FDA) {
    cat("Clipping", j,':\n'); flush.console()
    fda <- filter(w, FDA==j)
    for (i in 1:7) {
        cat("...", v[[1]][i],'\n'); flush.console()
        x <- st_read('www/fda9.gpkg', v[[1]][i], quiet=T)
        y <- st_intersection(x, fda)
        st_write(y, paste0('www/fda_',tolower(j),'.gpkg'), v[[1]][i], quiet=T)
    }
}

library(dplyr)
library(terra)
library(sf)
v <- vect(st_read('data/fda_10ab.gpkg', 'Fire_History'))
r2019 <- rast('data/fda_10ab/lc_2019.tif')

r_freq <- freq(r)
x <- tibble(value=r_freq$value, count_fda=r_freq$count)

r2 <- crop(r2019, v)
r2 <- mask(r2, v)
r2_freq <- freq(r2)[,2:3] %>%
    rename(count_2019=count)

xx <- left_join(x, r2_freq) %>%
    mutate(area_ha=round(count_fda*30*30/10000,1),
        intact_2019=round((count_fda-count_2019)/count_fda*100,1),
        count_fda=NULL, count_2019=NULL) %>%
    rename(Class=value)
xx


r_rcl <- subst(r2, c(20,31,32,33,40,50,80,81,100,210,220,230), c(0,0,0,0,0,1,0,0,0,0,0,0))

r_freq <- freq(r_rcl)
x <- tibble(value=1, count_fda=r_freq$count[2])

r2 <- crop(r_rcl, v)
r2 <- mask(r2, v)
r2_freq <- tibble(freq(r2)[2,2:3]) %>%
    rename(count_2019=count)
r2_freq

xx <- left_join(x, r2_freq) %>%
    mutate(area_ha=round(count_fda*30*30/10000,1),
        intact_2019=round((count_fda-count_2019)/count_fda*100,1),
        count_fda=NULL, count_2019=NULL) %>%
    rename(Class=value)
xx

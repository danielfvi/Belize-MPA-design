#Mangroves, Seagrass and Coral Reef habitats

library(tidyverse)
library(raster)

##Target fish biomass
r <- raster("data/target_biomass/s0_clim0_targ.tif")
plot(r)


root = read_sf('data', layer = 'roots_all_services') %>% st_transform(4326)

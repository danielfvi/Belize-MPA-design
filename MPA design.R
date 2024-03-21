##MPA 
library(sf)
library(raster)

reef_ras <- read_csv("data/reef_ras.csv") %>% 
  rename(layer = ...3)
seagrass_ras <- read_csv("data/seagrass_ras.csv") %>% 
  rename(layer = ...3)

reef <- raster("data/reef_ras.tif")
seagrass <- raster("data/seagrass_ras.tif")


ggplot() +
  geom_raster(data = reef_ras , aes(x = x, y = y, fill = layer))


ggplot() +
  geom_raster(data = seagrass_ras , aes(x = x, y = y, fill = layer))

##MPA 1 polygon - 20% reef and 20% seagrass
x1 = -88.2
x2 = -87.6
y1 = 17.3
y2 = 16.75
pol = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol, crs=4326)
pol2 = st_as_sf(polc)

ggplot() +
  geom_raster(data = seagrass_ras , aes(x = x, y = y, fill = layer))+
  geom_sf(data = pol2, alpha = 0.2)

ggplot() +
  geom_raster(data = reef_ras , aes(x = x, y = y, fill = layer))+
  geom_sf(data = pol2, alpha = 0.2)

reef_mpa = extract(reef, pol2)
100*sum(reef_mpa[[1]], na.rm = T)/sum(reef_ras$layer, na.rm = T)

sea_mpa = extract(seagrass, pol2)
100*sum(sea_mpa[[1]], na.rm = T)/sum(seagrass_ras$layer, na.rm = T)

##MPA 2 polygon - 20% of patches with habitat 
x1 = -88.2
x2 = -87.6
y1 = 17.3
y2 = 16.75
pol = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol, crs=4326)
pol2 = st_as_sf(polc)

ggplot() +
  geom_raster(data = seagrass_ras , aes(x = x, y = y, fill = layer))+
  geom_sf(data = pol2, alpha = 0.2)

ggplot() +
  geom_raster(data = reef_ras , aes(x = x, y = y, fill = layer))+
  geom_sf(data = pol2, alpha = 0.2)

reef_mpa = extract(reef, pol2)
100*sum(reef_mpa[[1]], na.rm = T)/sum(reef_ras$layer, na.rm = T)

sea_mpa = extract(seagrass, pol2)
100*sum(sea_mpa[[1]], na.rm = T)/sum(seagrass_ras$layer, na.rm = T)


###Test many configurations
k = rep(seq(-88.5, -87.9, by = 0.1), 19)
j = rep(seq(16, 17.8, by=0.1), 7)
g2 = data.frame(y1 = j) %>% 
  arrange(y1)
g = data.frame(x1 = k) %>% 
  mutate(x2 = x1+0.5) %>% 
  cbind(g2) %>% 
  mutate(y2 = y1+0.5)


for(n in 1:nrow(g)){
  print(n)
  x1 = g$x1[n]
  x2 = g$x2[n]
  y1 = g$y1[n]
  y2 = g$y2[n]
  
  pol = st_polygon(
    list(
      cbind(
        c(x1,x2,x2,x1,x1), 
        c(y2,y2,y1,y1,y2))
    )
  )
  polc = st_sfc(pol, crs=4326)
  pol2 = st_as_sf(polc)
  
  reef_mpa = extract(reef, pol2)
  reef_perc = 100*sum(reef_mpa[[1]], na.rm = T)/sum(reef_ras$layer, na.rm = T)
  
  sea_mpa = extract(seagrass, pol2)
  sea_perc = 100*sum(sea_mpa[[1]], na.rm = T)/sum(seagrass_ras$layer, na.rm = T)
  
  if(n==1){
    dat = data.frame(g[n,], 
                     reef_perc = reef_perc,
                     sea_perc = sea_perc)
  }else{
    dat = rbind(dat,
                data.frame(g[n,], 
                           reef_perc = reef_perc,
                           sea_perc = sea_perc))
  }
  
}


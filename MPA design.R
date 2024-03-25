##MPA 
library(sf)
library(raster)
library(tidyverse)

reef_ras <- read_csv("data/reef_ras.csv") %>% 
  rename(layer = ...3)

seagrass_ras <- read_csv("data/seagrass_ras.csv") %>% 
  rename(layer = ...3)

seagrass_reef_ras <- read_csv("data/seagrass_reef_ras.csv") %>% 
  rename(layer = ...3)

id_ras <- read_csv("data/id_ras.csv") %>% 
  rename(ID = ...3)

patches_habitat = nrow(seagrass_reef_ras %>% drop_na(layer))

reef <- raster("data/reef_ras.tif")
seagrass <- raster("data/seagrass_ras.tif")
seagrass_reef <- raster("data/seagrass_reef_ras.tif")
ras_id = raster("data/id_ras.tif")

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

reef_mpa = raster::extract(reef, pol2)
100*sum(reef_mpa[[1]], na.rm = T)/sum(reef_ras$layer, na.rm = T)

sea_mpa = raster::extract(seagrass, pol2)
100*sum(sea_mpa[[1]], na.rm = T)/sum(seagrass_ras$layer, na.rm = T)

##Overlay poligon with ID
id = extract(ras_id, pol2)
id_df_1 = data.frame(ID = id[[1]],
                   MPA_1 = TRUE)

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
  
  reef_mpa = raster::extract(reef, pol2)
  reef_perc = 100*sum(reef_mpa[[1]], na.rm = T)/sum(reef_ras$layer, na.rm = T)
  
  sea_mpa = raster::extract(seagrass, pol2)
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

##MPA 2 polygon - 20% of patches with habitat 

##Design 1
x1 = -88.4
x2 = -87.9
y1 = 16.6
y2 = 16.1
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

reef_mpa = raster::extract(seagrass_reef, pol2)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat

##Overlay poligon with ID
id = raster::extract(ras_id, pol2)
id_df_2 = data.frame(ID = id[[1]],
                     MPA_2 = TRUE)

##Design 2
x1 = -88.35
x2 = -87.85
y1 = 17.58
y2 = 18.1
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

reef_mpa = raster::extract(seagrass_reef, pol2)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat

##Overlay poligon with ID
id = raster::extract(ras_id, pol2)
id_df_3 = data.frame(ID = id[[1]],
                     MPA_3 = TRUE)

##Design 3
x1 = -87.98
x2 = -87.4
y1 = 17.63
y2 = 16.6
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

reef_mpa = raster::extract(seagrass_reef, pol2)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat

##Overlay poligon with ID
id = raster::extract(ras_id, pol2)
id_df_4 = data.frame(ID = id[[1]],
                     MPA_4 = TRUE)

##Design 4
x1 = -88.3
x2 = -88
y1 = 17.4
y2 = 16.55
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

reef_mpa = raster::extract(seagrass_reef, pol2)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat

##Overlay poligon with ID
id = raster::extract(ras_id, pol2)
id_df_5 = data.frame(ID = id[[1]],
                     MPA_5 = TRUE)

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
  
  reef_mpa = raster::extract(seagrass_reef, pol2)
  perc_patches = 100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat
  
  if(n==1){
    dat = data.frame(g[n,], 
                     perc_patches = perc_patches)
  }else{
    dat = rbind(dat,
                data.frame(g[n,], 
                           perc_patches = perc_patches))
  }
  
}


##MPA polygon - two MPAs to protect 20% seagrass and 20% coral reef
x1 = -88.28
x2 = -88.05
y1 = 17.75
y2 = 17.5
pol_sea = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol_sea, crs=4326)
pol_sea = st_as_sf(polc)

x1 = -88.25
x2 = -87.95
y1 = 16.75
y2 = 16.5
pol_reef = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol_reef, crs=4326)
pol_reef = st_as_sf(polc)

ggplot() +
  geom_raster(data = seagrass_reef_ras , aes(x = x, y = y, fill = layer)) +
  geom_sf(data = pol_sea, alpha = 0.3) +
  geom_sf(data = pol_reef, alpha = 0.3)

ggplot() +
  geom_raster(data = reef_ras , aes(x = x, y = y, fill = layer)) +
  geom_sf(data = pol_sea, alpha = 0.2) +
  geom_sf(data = pol_reef, alpha = 0.2)

reef_mpa1 = raster::extract(reef, pol_reef)
reef_mpa2 = raster::extract(reef, pol_sea)
e1 = 100*sum(reef_mpa1[[1]], na.rm = T)/sum(reef_ras$layer, na.rm = T)
e2 = 100*sum(reef_mpa2[[1]], na.rm = T)/sum(reef_ras$layer, na.rm = T)
e1+e2

sea_mpa1 = raster::extract(seagrass, pol_sea)
sea_mpa2 = raster::extract(seagrass, pol_reef)
f1 = 100*sum(sea_mpa1[[1]], na.rm = T)/sum(seagrass_ras$layer, na.rm = T)
f2 = 100*sum(sea_mpa2[[1]], na.rm = T)/sum(seagrass_ras$layer, na.rm = T)
f1+f2

##Overlay polygon with ID

id_df_6 = rbind(data.frame(ID = extract(ras_id, pol_sea)[[1]],
                     MPA_6 = TRUE),
                data.frame(ID = extract(ras_id, pol_reef)[[1]],
                           MPA_6 = TRUE))

###Test many configurations
k = rep(seq(-88.5, -87.7, by = 0.1), 23)
j = rep(seq(16, 18.2, by=0.1), 9)
g2 = data.frame(y1 = j) %>% 
  arrange(y1)
g = data.frame(x1 = k) %>% 
  mutate(x2 = x1+0.25) %>% 
  cbind(g2) %>% 
  mutate(y2 = y1+0.25)

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
  
  reef_mpa = raster::extract(reef, pol2)
  reef_perc = 100*sum(reef_mpa[[1]], na.rm = T)/sum(reef_ras$layer, na.rm = T)
  
  sea_mpa = raster::extract(seagrass, pol2)
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

dat$ID = 1:nrow(dat)

for(n in 1:nrow(dat)){
  print(n)
  sea_dat = dat %>% 
    dplyr::select(ID, sea_perc)
  
  reef_dat = dat %>% 
    dplyr::select(ID, reef_perc)
  
  dd = cbind(id1 = rep(dat$ID[n], nrow(dat)-1),
             id2 = dat$ID[-n]) %>% 
    as.data.frame() %>% 
    left_join(sea_dat, by=c("id1" = "ID")) %>% 
    rename(sea_perc1 = sea_perc) %>% 
    left_join(sea_dat, by=c("id2" = "ID")) %>% 
    rename(sea_perc2 = sea_perc) %>% 
    left_join(reef_dat, by=c("id1" = "ID")) %>% 
    rename(reef_perc1 = reef_perc) %>% 
    left_join(reef_dat, by=c("id2" = "ID")) %>% 
    rename(reef_perc2 = reef_perc) %>% 
    mutate(total_reef = reef_perc1+reef_perc2,
           total_sea = sea_perc1+sea_perc2,
           total_coverage = total_reef + total_sea)
  
  if(n==1){
    dat_total = dd
  }else{
    dat_total = rbind(dat_total, dd)
  }
       
}

coords_id = dat %>% 
  dplyr::select(ID, x1, x2, y1, y2)

dat_total2 = dat_total %>% 
  left_join(coords_id, by=c("id1" = "ID")) %>% 
  left_join(coords_id, by=c("id2" = "ID"), suffix = c(".1", ".2"))


################MPA polygon - two MPAs with 10% of patches with habitat each

#Option 1
x1 = -88.2
x2 = -87.9
y1 = 18
y2 = 17.7
pol_sea = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol_sea, crs=4326)
pol_sea = st_as_sf(polc)

x1 = -88.4
x2 = -88.1
y1 = 16.6
y2 = 16.3
pol_reef = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol_reef, crs=4326)
pol_reef = st_as_sf(polc)

ggplot() +
  geom_raster(data = seagrass_reef_ras , aes(x = x, y = y, fill = layer)) +
  geom_sf(data = pol_sea, alpha = 0.3) +
  geom_sf(data = pol_reef, alpha = 0.3)

reef_mpa = raster::extract(seagrass_reef, pol_sea)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat

reef_mpa = raster::extract(seagrass_reef, pol_reef)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat


id_df_7 = rbind(data.frame(ID = extract(ras_id, pol_sea)[[1]],
                           MPA_7 = TRUE),
                data.frame(ID = extract(ras_id, pol_reef)[[1]],
                           MPA_7 = TRUE))

#Option 2
x1 = -88.2
x2 = -87.9
y1 = 17.4
y2 = 17.1
pol_sea = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol_sea, crs=4326)
pol_sea = st_as_sf(polc)

x1 = -88.3
x2 = -88
y1 = 16.7
y2 = 16.4
pol_reef = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol_reef, crs=4326)
pol_reef = st_as_sf(polc)

ggplot() +
  geom_raster(data = seagrass_reef_ras , aes(x = x, y = y, fill = layer)) +
  geom_sf(data = pol_sea, alpha = 0.3) +
  geom_sf(data = pol_reef, alpha = 0.3)

reef_mpa = raster::extract(seagrass_reef, pol_sea)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat

reef_mpa = raster::extract(seagrass_reef, pol_reef)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat

id_df_8 = rbind(data.frame(ID = extract(ras_id, pol_sea)[[1]],
                           MPA_8 = TRUE),
                data.frame(ID = extract(ras_id, pol_reef)[[1]],
                           MPA_8 = TRUE))

#Option 3
x1 = -88
x2 = -87.65
y1 = 17.65
y2 = 17.1
pol_sea = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol_sea, crs=4326)
pol_sea = st_as_sf(polc)

x1 = -88.3
x2 = -88
y1 = 16.7
y2 = 16.4
pol_reef = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol_reef, crs=4326)
pol_reef = st_as_sf(polc)

ggplot() +
  geom_raster(data = seagrass_reef_ras , aes(x = x, y = y, fill = layer)) +
  geom_sf(data = pol_sea, alpha = 0.3) +
  geom_sf(data = pol_reef, alpha = 0.3)

reef_mpa = raster::extract(seagrass_reef, pol_sea)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat

reef_mpa = raster::extract(seagrass_reef, pol_reef)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat

id_df_9 = rbind(data.frame(ID = extract(ras_id, pol_sea)[[1]],
                           MPA_9 = TRUE),
                data.frame(ID = extract(ras_id, pol_reef)[[1]],
                           MPA_9 = TRUE))


#Option 4
x1 = -88
x2 = -87.65
y1 = 17.65
y2 = 17.1
pol_sea = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol_sea, crs=4326)
pol_sea = st_as_sf(polc)

x1 = -88.2
x2 = -87.9
y1 = 18
y2 = 17.7
pol_reef = st_polygon(
  list(
    cbind(
      c(x1,x2,x2,x1,x1), 
      c(y2,y2,y1,y1,y2))
  )
)
polc = st_sfc(pol_reef, crs=4326)
pol_reef = st_as_sf(polc)

ggplot() +
  geom_raster(data = seagrass_reef_ras , aes(x = x, y = y, fill = layer)) +
  geom_sf(data = pol_sea, alpha = 0.3) +
  geom_sf(data = pol_reef, alpha = 0.3)

reef_mpa = raster::extract(seagrass_reef, pol_sea)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat

reef_mpa = raster::extract(seagrass_reef, pol_reef)
100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat


id_df_10 = rbind(data.frame(ID = extract(ras_id, pol_sea)[[1]],
                           MPA_10 = TRUE),
                data.frame(ID = extract(ras_id, pol_reef)[[1]],
                           MPA_10 = TRUE))

###Test many configurations
k = rep(seq(-88.5, -87.7, by = 0.1), 23)
j = rep(seq(16, 18.2, by=0.1), 9)
g2 = data.frame(y1 = j) %>% 
  arrange(y1)
g = data.frame(x1 = k) %>% 
  mutate(x2 = x1+0.3) %>% 
  cbind(g2) %>% 
  mutate(y2 = y1+0.3)

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
  
  reef_mpa = raster::extract(seagrass_reef, pol2)
  reef_perc = 100*nrow(data.frame(dd = reef_mpa[[1]]) %>% drop_na())/patches_habitat
  
  if(n==1){
    dat = data.frame(g[n,], 
                     reef_perc = reef_perc)
  }else{
    dat = rbind(dat,
                data.frame(g[n,], 
                           reef_perc = reef_perc))
  }
  
}

dat$ID = 1:nrow(dat)

for(n in 1:nrow(dat)){
  print(n)

  reef_dat = dat %>% 
    dplyr::select(ID, reef_perc)
  
  dd = cbind(id1 = rep(dat$ID[n], nrow(dat)-1),
             id2 = dat$ID[-n]) %>% 
    as.data.frame() %>% 
    left_join(reef_dat, by=c("id1" = "ID")) %>% 
    rename(reef_perc1 = reef_perc) %>% 
    left_join(reef_dat, by=c("id2" = "ID")) %>% 
    rename(reef_perc2 = reef_perc) %>% 
    mutate(total_reef = reef_perc1+reef_perc2)
  
  if(n==1){
    dat_total = dd
  }else{
    dat_total = rbind(dat_total, dd)
  }
  
}

coords_id = dat %>% 
  dplyr::select(ID, x1, x2, y1, y2)

dat_total2 = dat_total %>% 
  left_join(coords_id, by=c("id1" = "ID")) %>% 
  left_join(coords_id, by=c("id2" = "ID"), suffix = c(".1", ".2"))


#######################################Compile data

dat_id = id_ras %>% 
  left_join(id_df_1) %>% 
  left_join(id_df_2) %>%
  left_join(id_df_3) %>%
  left_join(id_df_4) %>%
  left_join(id_df_5) %>%
  left_join(id_df_6) %>%
  left_join(id_df_7) %>%
  left_join(id_df_8) %>%
  left_join(id_df_9) %>%
  left_join(id_df_10) %>% 
  dplyr::select(-ID)

write.csv(dat_id, "data/mpa_locations.csv", row.names = F)
ggplot() +
  geom_raster(data = dat_id , aes(x = x, y = y, fill = MPA_1)) 

######################################Function to generate MPA design

mpa_gen = function(mpa_numb){

  if(mpa_numb == 4){
    z1 = -88.5
    z2 = -87.5
    zz1 = 16
    zz2 = 18.2
    buff_dist = 10000
  }else{
    if(mpa_numb == 3){
      z1 = -88.4
      z2 = -87.6
      zz1 = 16.2
      zz2 = 18
      buff_dist = 16000
    }else{
      if(mpa_numb == 2){
        z1 = -88.4
        z2 = -87.6
        zz1 = 16.2
        zz2 = 18
        buff_dist = 20000
      }else{
        z1 = -88.2
        z2 = -87.7
        zz1 = 16.3
        zz2 = 17.9
        buff_dist = 30000
    }
    }
  }
  k1 = seq(z1, z2, by = 0.01)
  j1 = seq(zz1, zz2, by=0.01)
  k = rep(k1, length(j1))
  j = rep(j1, length(k1))
  g2 = data.frame(y1 = j) %>% 
    arrange(y1)
  g = data.frame(x1 = k) %>% 
    cbind(g2)
  #g$ID = 1:nrow(g)
  
  booths = st_as_sf(g, coords=c("x1","y1"), crs=4326)
  st_crs(booths) = st_crs(pol2)

  for(n in 1:nrow(g)){
    print(n)
    
    mpa_buf = sf::st_buffer(booths[n,], dist = 30000, endCapStyle = "SQUARE")
    
    xx = st_bbox(mpa_buf)
    
    x1 = xx[1]
    x2 = xx[3]
    y1 = xx[4]
    y2 = xx[2]
    
    pol = st_polygon(
      list(
        cbind(
          c(x1,x2,x2,x1,x1), 
          c(y2,y2,y1,y1,y2))
      )
    )
    polc = st_sfc(pol, crs=4326)
    pol2 = st_as_sf(polc)
    
    # ggplot() +
    #   geom_raster(data = seagrass_reef_ras , aes(x = x, y = y, fill = layer)) +
    #   geom_sf(data = pol2, alpha = 0.8, col = "red")
    
    patch_mpa = raster::extract(seagrass_reef, pol2)
    patch_perc = 100*nrow(data.frame(dd = patch_mpa[[1]]) %>% drop_na())/patches_habitat
    
    hab_mpa = raster::extract(seagrass_reef, pol2)
    hab_perc = 100*sum(hab_mpa[[1]], na.rm = T)/sum(seagrass_reef_ras$layer, na.rm = T)
    
    if(n==1){
      dat = data.frame(patch_perc = patch_perc,
                       hab_perc = hab_perc,
                       mpa_buf)
    }else{
      dat = rbind(dat,
                  data.frame(patch_perc = patch_perc,
                       hab_perc = hab_perc,
                       mpa_buf))
    }
  }
  return(dat)
}   
  
##Run MPA options for a network with 1 MPA
mpaNet_1 = mpa_gen(mpa_numb = 1)  

##Run MPA options for a netwrk with 2 MPA
mpaNet_2 = mpa_gen(mpa_numb = 2)

##Run MPA options for a netwrk with 3 MPA
mpaNet_3 = mpa_gen(mpa_numb = 3)

##Run MPA options for a netwrk with 4 MPA
mpaNet_4 = mpa_gen(mpa_numb = 4)
  
  
  example_point_1 = sf::st_point(c(0, 10)) 
  example_point_2 = sf::st_point(c(110, 40))
  
  example_points_sf <- sf::st_sfc(example_point_1, example_point_2) %>% 
    sf::st_sf()
  
  sf::st_buffer(example_points_sf,
                dist = 50,
                endCapStyle = "SQUARE") %>% plot() 
  plot(example_points_sf, add = T)
    
  mpa_buf = terra::buffer(g,
                          width = 10000,
                          quadsegs = 1)
  


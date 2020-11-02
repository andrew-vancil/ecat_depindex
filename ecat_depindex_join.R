library(tidyr)
library(dplyr)
library(ecat)
library(sf)
library(tigris)
library(tmap)
library(tmaptools)
tmap_mode("plot")

#ecat bounding box
ecat_bb <-
  st_bbox(ecat:::elevation.raster) %>%
  st_as_sfc(ecat_bb) %>%
  st_transform(5072)

#read in ecat grid
ecat_grid<-st_read('ecat_grid.gpkg')
#mapview::mapview(ecat_grid,zcol="ecat")
ecat_grid<-ecat_grid %>%
  st_transform(5072)

#read in deprivation index
dep_index <- 'https://github.com/geomarker-io/dep_index/raw/master/ACS_deprivation_index_by_census_tracts.rds' %>% 
  url() %>% 
  gzcon() %>% 
  readRDS() %>% 
  as_tibble()

#Collect census tract geometry
all_tracts<-rbind(tracts("Ohio",class="sf"),tracts("Indiana",class="sf"),tracts("Kentucky",class="sf"))
all_tracts<-all_tracts %>%
  st_transform(5072)
ecat_tracts<-st_intersection(ecat_grid,all_tracts) #the census tracts that overlap with our ecat grid
#mapview::mapview(ecat_tracts,zcol="ecat")
di_tracts<-left_join(all_tracts,dep_index,by=c("GEOID"="census_tract_fips")) %>%
  st_transform(5072)
  

#join together
ecat_dep<-st_join(ecat_tracts,di_tracts,) %>%
  select(ecat,GEOID.x,GEOID.y,fraction_assisted_income:geom)%>%
  st_transform(3735)

#Make plots with tmap
map_roads <-
  tigris::primary_roads() %>%
  st_transform(5072) %>%
  st_intersection(ecat_bb)

ecat_only_map<-
  #tm_basemap(leaflet::providers$CartoDB.Positron) +
  tm_shape(st_transform(map_roads,3735))+
  tm_lines(col="black",lwd=4,alpha=.6)+
  tm_shape(ecat_dep)+
  tm_fill("ecat", pallete = "YlGrBl", alpha = 0.8,
          title = expression(ECAT~(ug/m^"3")),
          legend.format = list(text.separator = "to"),
          popup.vars=c('ECAT'='ecat'))+
  tm_layout(legend.frame = T, legend.bg.color = "ivory", legend.position = c("left","top"))
ecat_only_map

di_only_map<-
  #tm_basemap(leaflet::providers$CartoDB.Positron) +
  tm_shape(st_transform(map_roads,3735))+
  tm_lines(col="black",lwd=4,alpha=.6)+
  tm_shape(ecat_dep)+
  tm_fill("dep_index",pallette = "YlGrBl",alpha=0.8,
          title=c("Dep. Index"),
          popup.vars=c('Dep. Index'='dep_index'))+
  tm_layout(legend.frame = T, legend.bg.color = "ivory", legend.position = c("left","top"))
di_only_map

losdos<-
  tmap_arrange(ecat_only_map,di_only_map)
  tm_basemap(leaflet::providers$CartoDB.Positron) +
  tm_shape(ecat_dep)+
  tm_polygons(c("ecat", "dep_index")) +
  tm_facets(sync = TRUE, ncol = 2)
losdos

tmap_save(ecat_only_map,"ecat_map_only.png")
tmap_save(di_only_map,"dep_index_map_only.png")

#Introduce Coefficients, calculate contributions
coeff<-read.csv("coefficients for ECAT model.csv")
ecat_dep<-ecat_dep %>%
  select(ecat,dep_index,GEOID.x,geom) %>%
  mutate(ecat_contrib = ecat[] *  coeff[7,2]) %>%
  mutate(depind_contrib = dep_index[] *  coeff[8,2])

# ecat_dep$ecat_contrib<-0
# ecat_dep$depind_contrib<-0
# for (i in 1:nrow(ecat_dep)){
#   ecat_dep$ecat_contrib[i] <- ecat_dep$ecat[i] * coeff[7,2]
#   ecat_dep$depind_contrib[i] <- ecat_dep$dep_index[i] * coeff[8,2]
#}
   
#Maps of the contributions 
ecat_contribution_map<-
  tm_shape(st_transform(map_roads,3735))+
  tm_lines(col="black",lwd=4,alpha=.6)+
  tm_shape(ecat_dep)+
  tm_fill("ecat_contrib", pallete = "YlOrRd", alpha = 0.8,
          title = c("ECAT\nContribution"),
          legend.format = list(text.separator = "to"),
          popup.vars=c('ECAT'='ecat_contr'))+
  tm_layout(legend.frame = T, legend.bg.color = "ivory", legend.position = c("left","top"))
ecat_contribution_map

di_contribution_map<-
  tm_shape(st_transform(map_roads,3735))+
  tm_lines(col="black",lwd=4,alpha=.6)+
  tm_shape(ecat_dep)+
  tm_fill("depind_contrib",pallette = "YlGrBl",alpha=0.8,
          title=c("Dep. Index\nContribution"),
          popup.vars=c('Dep. Index'='depind_contrib'))+
  tm_layout(legend.frame = T, legend.bg.color = "ivory", legend.position = c("left","top"))
di_contribution_map

tmap_save(ecat_contribution_map,"ecat_map_contribution.png")
tmap_save(di_contribution_map,"dep_index_map_contribution.png")

#Drivetime 
#read in drivetime geodata & join to dataset
drive_all<-readRDS("pepr_isochrones_no_overlap_5072.rds")
drive_cincy<-drive_all$cincinnati_childrens

ecat_dep<-ecat_dep %>%
  st_join(st_transform(drive_cincy,3735),largest=TRUE)
ecat_dep$drive_time<-ecat_dep$drive_time %>%
  as.character() %>%
  replace_na(">60") %>%
  factor()

#levels(ecat_dep$drive_time)<-list("6 Min"="6","12 min"="12","18 min"="18","24 min"="24",
#30 min"="30","36 min"="36","42 min"="42","48 min"="48",
 #                                   "54 min"="54","60 min"="60")

drivetime_map<-
  tm_shape(st_transform(map_roads,3735))+
  tm_lines(col="black",lwd=4,alpha=.6)+
  tm_shape(ecat_dep)+
  tm_fill("drive_time",pallette = "RdOrYl",alpha=0.8,
          title=c("Drivetime"),
          popup.vars=c('Drivetime'='drive_time'))+
  tm_layout(legend.frame = T, legend.bg.color = "ivory", legend.position = c("left","top"))
drivetime_map

#Add drivetime coefficients into the appropriate rows
ecat_dep<-ecat_dep %>% mutate(drive_contrib = 
 ifelse(drive_time == "6",  coeff[18,2],
  ifelse(drive_time == "12",  coeff[10,2],
  ifelse(drive_time == "18",  coeff[11,2],
  ifelse(drive_time == "24",  coeff[12,2],
  ifelse(drive_time == "30",  coeff[13,2],
  ifelse(drive_time == "36",  coeff[14,2],
  ifelse(drive_time == "42",  coeff[15,2],
  ifelse(drive_time == "48",  coeff[16,2],
  ifelse(drive_time == "54",  coeff[17,2], coeff[19,2] #using the 60 min coeff for 60 and >60
  ))))))))))
  
drive_contrib_map<-
  tm_shape(st_transform(map_roads,3735))+
  tm_lines(col="black",lwd=4,alpha=.6)+
  tm_shape(ecat_dep)+
  tm_fill("drive_contrib",alpha=0.8,
          title=c("Drivetime\nContribution"),
          popup.vars=c('Drivetime'='drive_time'))+
  tm_layout(legend.frame = T, legend.bg.color = "ivory", legend.position = c("left","top"))
drive_contrib_map

tmap_save(drivetime_map,"drivetime_map_only.png")
tmap_save(drive_contrib_map,"drivetime_map_contribution.png")

#Percent Green
remotes::install_github("geomarker-io/addNlcdData")
library(addNlcdData)
ecat2<-ecat_grid[200,]
green_grid<-get_nlcd_data_polygons(ecat_grid)
#it downloads an fst file and then errors out once it gets that downloaded. 
out <- fst::read_fst(
  path = "D:/Cincinnati Childrens/R/ecat_depindex/nlcd_fst/nlcd_chunk_809.fst")
utils::download.file(
  url = glue::glue(
    "https://geomarker.s3.us-east-2.amazonaws.com/",
    "nlcd/nlcd_fst/", "nlcd_chunk_809.fst"), destfile="D:/Cincinnati Childrens/R/ecat_depindex/nlcd_fst/nlcd_chunk_809.fst"
  )


#overall contribution map
ecat_dep2<-ecat_dep %>%
  as.numeric(ecat_dep$ecat_contrib,ecat_dep$depind_contrib,ecat_dep$drive_contrib) %>%
  mutate(tot_contrib = rowSums(.[3,4,8]))# %>%
  #as.character(ecat_contrib,depind_contrib,drive_contrib)



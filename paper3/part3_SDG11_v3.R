##############################################################
### Comparison of SDG11.1.1 in 6 cities                    ###
### By: Dana R Thomson (dana.r.thomson@gmail.com) and      ###
###     Hazem Mahmoud (Hazem.mahmoud@my.utsa.edu)          ###
### Aug 2023                                               ###
### Part 3: SDG 11 assessment                              ###
##############################################################

# This project is funded by CIESIN, Columbia University
# Objectives:
#   1. Compare gridded pop density estimates to field density estimates in 12 cities
#      where SDI enumerated 12+ slums (https://knowyourcity.info/explore-our-data/)
#   2. Calculate SDG11.1.1 in 6 African & Asian cities with complete slum area maps


####################################    SET-UP    #######################################


######################
## Initialize
######################

rm(list=ls())

setwd("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/data") 


#Projections
proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj.moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
proj.utm30 <- "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 30N # Accra
proj.utm31 <- "+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 31N # Lagos
proj.utm37 <- "+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 37S # Nairobi 
proj.utm43 <- "+proj=utm +zone=43 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 43N # Mumbai
proj.utm44 <- "+proj=utm +zone=44 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 44N # Hyderabad
proj.utm46 <- "+proj=utm +zone=46 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 46N # Dhaka


#packages (versions)
# R version 4.3.1
library(data.table)     #v1.14.8
library(exactextractr)  #v0.9.1
library(ggplot2)        #v3.4.2
library(haven)          #v2.5.3 
library(raster)         #3.6-23
library(rgdal)          #1.6-7
library(sf)             #v1.0-14
library(sp)             #v2.0-0 
library(survey)         #v4.2-1
library(tmaptools)      #v3.1-1

#library(NCmisc)
#list.functions.in.file("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/code/paper3/part3_SDG11_v3.R", alphabetic = TRUE)
#print(sessionInfo())


######################
##  Summarise population estimates
######################


##### NAIROBI #####

#GADM: Nairobi County + Mahabir 2018 cleaned
city_gadm <- read_sf("mahabir/Nairobi_Mahabiretal_2018_dis_100mbuff_gadm2.shp") #file generated in ArcGIS by DRT

#create unprojected bounding box to crop rasters
bbox <- bb(city_gadm, ext=1.2, output=c("extent"))   
bbox_moll <- bb(city_gadm, ext=1.2, projection = proj.moll, output=c("extent"))

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
city_gadm$gpw20 <- exact_extract(gpw20, city_gadm, 'sum')
rm(gpw20)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm37)
city_gadm$ghs20 <- exact_extract(ghs20, city_gadm, 'sum')
rm(ghs20)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/ken_general_2020.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm37)
city_gadm$hrsl20 <- exact_extract(hrsl20, city_gadm, 'sum')
rm(hrsl20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/KEN_population_202207292310.tif")
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm37)
city_gadm$wp_pb20 <- exact_extract(wp_pb20, city_gadm, 'sum')
rm(wp_pb20)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/ken_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm37)
city_gadm$wp_u20 <- exact_extract(wp_u20, city_gadm, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/ken_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm37)
city_gadm$wp_c20 <- exact_extract(wp_c20, city_gadm, 'sum')
rm(wp_c20)

#LandScan Global (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm37)
city_gadm$lsg20 <- exact_extract(lsg20, city_gadm, 'sum')
rm(lsg20)


#calculate SDG11.1.1 (GADM)
nai_city_gadm <- as.data.frame(city_gadm)
nai_city_gadm <- nai_city_gadm[,-3] #drop geometry column
nai_city_gadm <- melt(setDT(nai_city_gadm), id.vars = c("city_name","slum"), variable.name = "dataset")
poptot <- nai_city_gadm[ ,list(sum=sum(value)), by=dataset]
nai_city_gadm <- merge(nai_city_gadm,poptot, by="dataset")
nai_city_gadm <- nai_city_gadm[nai_city_gadm$slum==1,]
names(nai_city_gadm) <- c("dataset", "city_name", "slum", "slum_pop_n", "tot_pop_n")
nai_city_gadm$sdg11_1_1 <- nai_city_gadm$slum_pop_n / nai_city_gadm$tot_pop_n * 100


# 2014 Kenya DHS
dhs <- read_dta("dhs/KEN/KEPR72FL.DTA")
keep <- c("hhid", "hv001", "hv002", "hv005", "hv009", "hv023", "hv024", "hv025", "hv201", "hv205", "hv213", "hv216")
dhs <- dhs[keep]
dhs <- dhs[dhs$hv024==9,] # keep "Nairobi" households

#flag slum households
dhs$pid <- seq_along(dhs[[1]])
dhs$water <- ifelse(dhs$hv201 %in% c(32, 42, 43, 96), 1, 0)
dhs$toilet <- ifelse(dhs$hv205  %in% c(14, 23, 31, 42, 43, 96), 1, 0)
dhs$floor <- ifelse(dhs$hv213 %in% c(10:29, 96, 99), 1, 0)
dhs$crowd <- ifelse(dhs$hv009 / dhs$hv216 >=3, 1, 0)
dhs$slum <- ifelse(dhs$water + dhs$toilet + dhs$floor + dhs$crowd >=1, 1, 0)


#calculate weighted percent slumhh
dhs$hv005 <- dhs$hv005/1000000
dhs_design <- svydesign(id= ~pid, weights= ~hv005, data=dhs)
summary(dhs_design)
nai_city_gadm$dhs <- svymean(~slum, dhs_design, na=TRUE)



##### Accra, Ghana ######

#GADM: Accra District + Slumap 2017 cleaned
city_gadm <- read_sf("cities_slum_census/gha_accra/acc_slum_gadm_v2.shp") #file generated in ArcGIS by DRT
city_gadm <- st_transform(city_gadm, proj)

#create unprojected bounding box to crop rasters
bbox <- bb(city_gadm, ext=1.2, output=c("extent"))   
bbox_moll <- bb(city_gadm, ext=1.2, projection = proj.moll, output=c("extent"))

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm30)
city_gadm$gpw20 <- exact_extract(gpw20, city_gadm, 'sum')
rm(gpw20)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm30)
city_gadm$ghs20 <- exact_extract(ghs20, city_gadm, 'sum')
rm(ghs20)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/gha_general_2020.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm30)
city_gadm$hrsl20 <- exact_extract(hrsl20, city_gadm, 'sum')
rm(hrsl20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/GHA_population_202207292314.tif")
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm30)
city_gadm$wp_pb20 <- exact_extract(wp_pb20, city_gadm, 'sum')
rm(wp_pb20)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/gha_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm30)
city_gadm$wp_u20 <- exact_extract(wp_u20, city_gadm, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/gha_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm30)
city_gadm$wp_c20 <- exact_extract(wp_c20, city_gadm, 'sum')
rm(wp_c20)

#LandScan Global (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm30)
city_gadm$lsg20 <- exact_extract(lsg20, city_gadm, 'sum')
rm(lsg20)

#calculate SDG11.1.1 (GADM)
acc_city_gadm <- as.data.frame(city_gadm)
acc_city_gadm <- acc_city_gadm[,-3] #drop geometry column
acc_city_gadm <- melt(setDT(acc_city_gadm), id.vars = c("city_name","slum"), variable.name = "dataset")
poptot <- acc_city_gadm[ ,list(sum=sum(value)), by=dataset]
acc_city_gadm <- merge(acc_city_gadm,poptot, by="dataset")
acc_city_gadm <- acc_city_gadm[acc_city_gadm$slum==1,]
names(acc_city_gadm) <- c("dataset", "city_name", "slum", "slum_pop_n", "tot_pop_n")
acc_city_gadm$sdg11_1_1 <- acc_city_gadm$slum_pop_n / acc_city_gadm$tot_pop_n * 100


# 2016 Ghana DHS
dhs <- read_dta("dhs/GHA/GHPR72FL.DTA") 
keep <- c("hhid", "hv001", "hv002", "hv005", "hv009", "hv023", "hv024", "hv025", "hv201", "hv205", "hv213", "hv216")
dhs <- dhs[keep]
dhs <- dhs[dhs$hv024==3 & dhs$hv025==1,] # keep "Greater Accra" urban households

#flag slum households
dhs$pid <- seq_along(dhs[[1]])
dhs$water <- ifelse(dhs$hv201 %in% c(32, 42, 43, 96), 1, 0) # sachet water (72) is often classified as unimproved; however, this bring %slum >80% which is inconsistent with reports of slum dwellers in Accra
dhs$toilet <- ifelse(dhs$hv205  %in% c(14, 15, 23, 31, 42, 43, 96), 1, 0)
dhs$floor <- ifelse(dhs$hv213 %in% c(10:29, 96, 99), 1, 0)
dhs$crowd <- ifelse(dhs$hv009 / dhs$hv216 >=3, 1, 0)
dhs$slum <- ifelse(dhs$water + dhs$toilet + dhs$floor + dhs$crowd >=1, 1, 0)

#calculate weighted percent slumhh
dhs$hv005 <- dhs$hv005/1000000
dhs_design <- svydesign(id= ~pid, weights= ~hv005, data=dhs)
summary(dhs_design)
acc_city_gadm$dhs <- svymean(~slum, dhs_design, na=TRUE)





##### LAGOS, NIGERIA ######

#GADM: Nairobi County + Mahabir 2018 cleaned
city_gadm <- read_sf("Badmos_etal/lag_slum_gadm_v1.shp") #file generated in ArcGIS by DRT
city_gadm <- st_transform(city_gadm, proj)

#create unprojected bounding box to crop rasters
bbox <- bb(city_gadm, ext=1.2, output=c("extent"))   
bbox_moll <- bb(city_gadm, ext=1.2, projection = proj.moll, output=c("extent"))


#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm31)
city_gadm$gpw20 <- exact_extract(gpw20, city_gadm, 'sum')
rm(gpw20)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm31)
city_gadm$ghs20 <- exact_extract(ghs20, city_gadm, 'sum')
rm(ghs20)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/nga_general_2020.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm31)
city_gadm$hrsl20 <- exact_extract(hrsl20, city_gadm, 'sum')
rm(hrsl20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/NGA_population_202207292317.tif")
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm31)
city_gadm$wp_pb20 <- exact_extract(wp_pb20, city_gadm, 'sum')
rm(wp_pb20)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/nga_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm31)
city_gadm$wp_u20 <- exact_extract(wp_u20, city_gadm, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/nga_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm31)
city_gadm$wp_c20 <- exact_extract(wp_c20, city_gadm, 'sum')
rm(wp_c20)


#LandScan Global (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm31)
city_gadm$lsg20 <- exact_extract(lsg20, city_gadm, 'sum')
rm(lsg20)

#LandScan HD (2020)
lshd20 <- raster("landscan/LandScan HD/landscan-hd-nigeria.tif")
lshd20 <- crop(lshd20, bbox)
lshd20 <- projectRaster(lshd20, crs=proj.utm31)
city_gadm$lshd20 <- exact_extract(lshd20, city_gadm, 'sum')
rm(lshd20)

#GRID3 Nigeria v2.0 (2019)
grid19 <- raster("grid3/NGA - population - v2.0 - gridded/NGA_population_v2_0_gridded.tif")
grid19 <- crop(grid19, bbox)
grid19 <- projectRaster(grid19, crs=proj.utm31)
city_gadm$grid19 <- exact_extract(grid19, city_gadm, 'sum')
rm(grid19)

#calculate SDG11.1.1 (GADM)
lag_city_gadm <- as.data.frame(city_gadm)
lag_city_gadm <- lag_city_gadm[,-3] #drop geometry column
lag_city_gadm <- melt(setDT(lag_city_gadm), id.vars = c("city_name","slum"), variable.name = "dataset")
poptot <- lag_city_gadm[ ,list(sum=sum(value)), by=dataset]
lag_city_gadm <- merge(lag_city_gadm,poptot, by="dataset")
lag_city_gadm <- lag_city_gadm[lag_city_gadm$slum==1,]
names(lag_city_gadm) <- c("dataset", "city_name", "slum", "slum_pop_n", "tot_pop_n")
lag_city_gadm$sdg11_1_1 <- lag_city_gadm$slum_pop_n / lag_city_gadm$tot_pop_n * 100


# 2018 Nigeria DHS
dhs <- read_dta("dhs/NGA/NGPR7BFL.DTA") 
keep <- c("hhid", "hv001", "hv002", "hv005", "hv009", "hv023", "hv024", "hv025", "hv201", "hv205", "hv213", "hv216")
dhs <- dhs[keep]
dhs <- dhs[dhs$hv023==65,] # keep "Lagos urban" households

#flag slum households
dhs$pid <- seq_along(dhs[[1]])
dhs$water <- ifelse(dhs$hv201 %in% c(32, 42, 43, 96), 1, 0) # sachet water (92) is often classified as unimproved; however, this bring %slum 78% which is substantialy higher then commonly reported
dhs$toilet <- ifelse(dhs$hv205  %in% c(14, 15, 23, 31, 42, 43, 96), 1, 0)
dhs$floor <- ifelse(dhs$hv213 %in% c(10:29, 96, 99), 1, 0)
dhs$crowd <- ifelse(dhs$hv009 / dhs$hv216 >=3, 1, 0)
dhs$slum <- ifelse(dhs$water + dhs$toilet + dhs$floor + dhs$crowd >=1, 1, 0)

#calculate weighted percent slumhh
dhs$hv005 <- dhs$hv005/1000000
dhs_design <- svydesign(id= ~pid, weights= ~hv005, data=dhs)
summary(dhs_design)
lag_city_gadm$dhs <- svymean(~slum, dhs_design, na=TRUE)





##### MUMBAI, INDIA ######

#GADM: Mumbai city district (no suburbs) + SRA 2017 
city_gadm <- read_sf("cities_slum_census/ind_mumbai/mum_slum_gadm_v3.shp") #file generated in ArcGIS by DRT
city_gadm <- st_transform(city_gadm, proj)

#create unprojected bounding box to crop rasters
bbox <- bb(city_gadm, ext=1.2, output=c("extent"))   
bbox_moll <- bb(city_gadm, ext=1.2, projection = proj.moll, output=c("extent"))

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm43)
city_gadm$gpw20 <- exact_extract(gpw20, city_gadm, 'sum')
rm(gpw20)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm43)
city_gadm$ghs20 <- exact_extract(ghs20, city_gadm, 'sum')
rm(ghs20)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/population_ind_pak_general/population_10_lon_70_general-v1.5.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm43)
city_gadm$hrsl20 <- exact_extract(hrsl20, city_gadm, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/ind_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm43)
city_gadm$wp_u20 <- exact_extract(wp_u20, city_gadm, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/ind_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm43)
city_gadm$wp_c20 <- exact_extract(wp_c20, city_gadm, 'sum')
rm(wp_c20)

#LandScan Global (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm43)
city_gadm$lsg20 <- exact_extract(lsg20, city_gadm, 'sum')
rm(lsg20)

#calculate SDG11.1.1 (GADM)
mum_city_gadm <- as.data.frame(city_gadm)
mum_city_gadm <- mum_city_gadm[,-3] #drop geometry column
mum_city_gadm <- melt(setDT(mum_city_gadm), id.vars = c("city_name","slum"), variable.name = "dataset")
poptot <- mum_city_gadm[ ,list(sum=sum(value)), by=dataset]
mum_city_gadm <- merge(mum_city_gadm,poptot, by="dataset")
mum_city_gadm <- mum_city_gadm[mum_city_gadm$slum==1,]
names(mum_city_gadm) <- c("dataset", "city_name", "slum", "slum_pop_n", "tot_pop_n")
mum_city_gadm$sdg11_1_1 <- mum_city_gadm$slum_pop_n / mum_city_gadm$tot_pop_n * 100


# 2019-20 India DHS
dhs <- read_dta("dhs/IND/IAHR7CFL_mum_hyd.dta") # data filtered to Mumbai and Hyderabad in Stata
keep <- c("hhid", "hv001", "hv002", "hv005", "hv009", "hv023", "hv024", "hv025", "hv201", "hv205", "hv213", "hv216", "shdist")
dhs <- dhs[keep]
dhs <- dhs[dhs$shdist==519,] # keep "Mumbai city district" households (not suburbs)

#flag slum households 
dhs$water <- ifelse(dhs$hv201 %in% c(32, 42, 43, 96), 1, 0) 
dhs$toilet <- ifelse(dhs$hv205  %in% c(14, 15, 23, 31, 42, 43, 96), 1, 0)
dhs$floor <- ifelse(dhs$hv213 %in% c(10:29, 96, 99), 1, 0)
dhs$crowd <- ifelse(dhs$hv009 / dhs$hv216 >=3, 1, 0)
dhs$slum <- ifelse(dhs$water + dhs$toilet + dhs$floor + dhs$crowd >=1, 1, 0)

#duplicate records for each HH member
dhs <- as.data.frame(lapply(dhs, rep, as.numeric(dhs$hv009)))
dhs$pid <- seq_along(dhs[[1]])

#calculate weighted percent slumhh
dhs$hv005 <- dhs$hv005/1000000
dhs_design <- svydesign(id= ~pid, weights= ~hv005, data=dhs)
summary(dhs_design)
mum_city_gadm$dhs <- svymean(~slum, dhs_design, na=TRUE)






##### HYDERABAD, INDIA ######

#GADM: Hyderabad city + Kit 2014
city_gadm <- read_sf("cities_slum_census/ind_hyderabad/hyd_slum_gadm_v3.shp") #file generated in ArcGIS by DRT
city_gadm <- st_transform(city_gadm, proj)

#create unprojected bounding box to crop rasters
bbox <- bb(city_gadm, ext=1.2, output=c("extent"))   
bbox_moll <- bb(city_gadm, ext=1.2, projection = proj.moll, output=c("extent"))

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm44)
city_gadm$gpw20 <- exact_extract(gpw20, city_gadm, 'sum')
rm(gpw20)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm44)
city_gadm$ghs20 <- exact_extract(ghs20, city_gadm, 'sum')
rm(ghs20)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/population_ind_pak_general/population_10_lon_70_general-v1.5.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm44)
city_gadm$hrsl20 <- exact_extract(hrsl20, city_gadm, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/ind_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm44)
city_gadm$wp_u20 <- exact_extract(wp_u20, city_gadm, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/ind_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm44)
city_gadm$wp_c20 <- exact_extract(wp_c20, city_gadm, 'sum')
rm(wp_c20)

#LandScan Global (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm44)
city_gadm$lsg20 <- exact_extract(lsg20, city_gadm, 'sum')
rm(lsg20)

#calculate SDG11.1.1 (GADM)
hyd_city_gadm <- as.data.frame(city_gadm)
hyd_city_gadm <- hyd_city_gadm[,-3] #drop geometry column
hyd_city_gadm <- melt(setDT(hyd_city_gadm), id.vars = c("city_name","slum"), variable.name = "dataset")
poptot <- hyd_city_gadm[ ,list(sum=sum(value)), by=dataset]
hyd_city_gadm <- merge(hyd_city_gadm,poptot, by="dataset")
hyd_city_gadm <- hyd_city_gadm[hyd_city_gadm$slum==1,]
names(hyd_city_gadm) <- c("dataset", "city_name", "slum", "slum_pop_n", "tot_pop_n")
hyd_city_gadm$sdg11_1_1 <- hyd_city_gadm$slum_pop_n / hyd_city_gadm$tot_pop_n * 100


# 2019-20 India DHS
dhs <- read_dta("dhs/IND/IAHR7CFL_mum_hyd.dta")  # data filtered to Mumbai and Hyderabad in Stata
keep <- c("hhid", "hv001", "hv002", "hv005", "hv009", "hv023", "hv024", "hv025", "hv201", "hv205", "hv213", "hv216", "shdist")
dhs <- dhs[keep]
dhs <- dhs[dhs$shdist==885,] # keep "hyderabad" households


#flag slum households 
dhs$water <- ifelse(dhs$hv201 %in% c(32, 42, 43, 96), 1, 0) 
dhs$toilet <- ifelse(dhs$hv205  %in% c(14, 15, 23, 31, 42, 43, 96), 1, 0)
dhs$floor <- ifelse(dhs$hv213 %in% c(10:29, 96, 99), 1, 0)
dhs$crowd <- ifelse(dhs$hv009 / dhs$hv216 >=3, 1, 0)
dhs$slum <- ifelse(dhs$water + dhs$toilet + dhs$floor + dhs$crowd >=1, 1, 0)

#duplicate records for each HH member
dhs <- as.data.frame(lapply(dhs, rep, as.numeric(dhs$hv009)))
dhs$pid <- seq_along(dhs[[1]])

#calculate weighted percent slumhh
dhs$hv005 <- dhs$hv005/1000000
dhs_design <- svydesign(id= ~pid, weights= ~hv005, data=dhs)
summary(dhs_design)
hyd_city_gadm$dhs <- svymean(~slum, dhs_design, na=TRUE)







##### DHAKA, BANGLADESH ######

#GADM: Dhaka city + Gruebner 2014
city_gadm <- read_sf("cities_slum_census/bng_dhaka/dha_slum_gadm_v3.shp") #file generated in ArcGIS by DRT
city_gadm <- st_transform(city_gadm, proj)

#create unprojected bounding box to crop rasters
bbox <- bb(city_gadm, ext=1.2, output=c("extent"))   
bbox_moll <- bb(city_gadm, ext=1.2, projection = proj.moll, output=c("extent"))

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm46)
city_gadm$gpw20 <- exact_extract(gpw20, city_gadm, 'sum')
rm(gpw20)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm46)
city_gadm$ghs20 <- exact_extract(ghs20, city_gadm, 'sum')
rm(ghs20)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/bgd_general_2020.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm46)
city_gadm$hrsl20 <- exact_extract(hrsl20, city_gadm, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/bgd_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm46)
city_gadm$wp_u20 <- exact_extract(wp_u20, city_gadm, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/bgd_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm46)
city_gadm$wp_c20 <- exact_extract(wp_c20, city_gadm, 'sum')
rm(wp_c20)

#LandScan Global (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm46)
city_gadm$lsg20 <- exact_extract(lsg20, city_gadm, 'sum')
rm(lsg20)

#calculate SDG11.1.1 (GADM)
dha_city_gadm <- as.data.frame(city_gadm)
dha_city_gadm <- dha_city_gadm[,-3] #drop geometry column
dha_city_gadm <- melt(setDT(dha_city_gadm), id.vars = c("city_name","slum"), variable.name = "dataset")
poptot <- dha_city_gadm[ ,list(sum=sum(value)), by=dataset]
dha_city_gadm <- merge(dha_city_gadm,poptot, by="dataset")
dha_city_gadm <- dha_city_gadm[dha_city_gadm$slum==1,]
names(dha_city_gadm) <- c("dataset", "city_name", "slum", "slum_pop_n", "tot_pop_n")
dha_city_gadm$sdg11_1_1 <- dha_city_gadm$slum_pop_n / dha_city_gadm$tot_pop_n * 100

# 2017-18 Bangladesh DHS
dhs <- read_dta("dhs/BGD/BDPR7RFL.dta")  
keep <- c("hhid", "hv001", "hv002", "hv005", "hv009", "hv023", "hv024", "hv025", "hv201", "hv205", "hv213", "hv216")
dhs <- dhs[keep]
dhs <- dhs[dhs$hv023==7,]
# 7 dhaka - city corporation
# 8 dhaka - other urban


#flag slum households 
dhs$pid <- seq_along(dhs[[1]])
dhs$water <- ifelse(dhs$hv201 %in% c(32, 42, 43, 96), 1, 0) 
dhs$toilet <- ifelse(dhs$hv205  %in% c(14, 15, 23, 31, 42, 43, 96), 1, 0)
dhs$floor <- ifelse(dhs$hv213 %in% c(10:29, 96, 99), 1, 0)
dhs$crowd <- ifelse(dhs$hv009 / dhs$hv216 >=3, 1, 0)
dhs$slum <- ifelse(dhs$water + dhs$toilet + dhs$floor + dhs$crowd >=1, 1, 0)

#calculate weighted percent slumhh
dhs$hv005 <- dhs$hv005/1000000
dhs_design <- svydesign(id= ~pid, weights= ~hv005, data=dhs)
summary(dhs_design)
dha_city_gadm$dhs <- svymean(~slum, dhs_design, na=TRUE)

rm(bbox, bbox_moll, dhs_design, poptot, dhs)


######################
## Summary figure
######################

# compile single dataset

sdg <- rbind(acc_city_gadm, dha_city_gadm, hyd_city_gadm, lag_city_gadm, mum_city_gadm, nai_city_gadm)
sdg$ref <- sdg$dhs*100
sdg$order <- with(sdg, ifelse(city_name %in% "Accra", 1,
                          ifelse(city_name %in% "Nairobi", 2,
                          ifelse(city_name %in% "Dhaka", 4,
                          ifelse(city_name %in% "Hyderabad",5,
                          ifelse(city_name %in% "Mumbai",3,6))))))

write.csv(sdg, "C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/output/sdg.csv", row.names = TRUE)



# plot

png("fig_8_sdg_v2.png")

### sort fig descending by smallest diff per city

fig_sdg <- ggplot(sdg, aes(x=reorder(city_name, -order), y=sdg11_1_1)) + 
  geom_boxplot() +
  geom_point(data=sdg, aes(x=reorder(city_name, -order), y=ref, size=6, color="red", fill="red")) +
  labs(x="", y='% City population living in "slums" (SDG 11.1.1)') +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")
fig_sdg

ggsave("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_8_sdg_v2.tiff", fig_sdg, units="in", width=6, height=2.5, dpi=400, compression = 'lzw')
ggsave("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_8_sdg_v2.png", fig_sdg, units="in", width=6, height=2.5, dpi=400)


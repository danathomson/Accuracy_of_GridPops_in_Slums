##############################################################
### Comparison of estimates populations in 12 cities       ###
### v3 includes gridded estimates for all settlements      ###
### drop Athiriver and Freetown due to small n             ###
### v4 includes further cleaning when checking results     ###
### By: Dana R Thomson (dana.r.thomson@gmail.com) and      ###
###     Hazem Mahmoud (Hazem.mahmoud@my.utsa.edu)          ###
### 10 Feb 2023                                            ###
### Part 1: Generate dataset for KYC accuracy assessment   ###
##############################################################

# Project Objectives:
#   1. Compare gridded density estimates to field density estimates in 12 cities
#      where SDI enumerated 12+ slums (https://knowyourcity.info/explore-our-data/)
#   2. Calculate SDG11.1.1 in 6 African & Asian cities with complete slum area maps



######################
## Initialize
######################


rm(list=ls())

setwd("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/data")   

#projections
proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj.moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
proj.utm30 <- "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 30N # Accra
proj.utm31 <- "+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 31N # Lagos
proj.utm32 <- "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 32N # Port Harcourt
proj.utm33 <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 33N # Capetown
proj.utm36 <- "+proj=utm +zone=36 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 36N # Kampala
proj.utm37 <- "+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 37S # Kisumu #Nairobi #Dar Es Salaam
proj.utm51 <- "+proj=utm +zone=51 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #WGS84/UTM 51N # Davao # Malabon

#packages (versions)
# R version 4.2.2
library(raster) #v3.0-12 
library(rgeos)  #v0.5-2
library(rgdal)  #v1.4-8 
library(dplyr)  #v1.0.9 
library(sp)     #1.3-2
library(sf)     #v0.9-5 
library(exactextractr)  #v0.4.0
library(tmaptools)      #v3.1



######################
##  Read & join pop data to boundaries
######################

##### LAGOS, NIGERIA ######

# KnowYourCity Campaign slum boundaries
slums <- readOGR("kyc_campaign/nga/kyc_cln_data_Lagos_Nigeria.shp") 
slums <- slums[,names(slums) %in% c("Country", "City", "Settlement", "Last_updat", "kyc_pop")]
names(slums) <- c("Country", "City", "Settlement", "KYC_Year", "KYC_Pop")

#create bounding box to crop rasters
bbox <- bb(slums, ext=1.2, output=c("extent"))
bbox_moll <- bb(slums, ext=1.2, projection = proj.moll, output=c("extent"))

#create sf object for exact_extract calcs
slums <- st_as_sf(spTransform(slums, CRS = proj.utm31))

#clean date
x <- data.frame(do.call("rbind", strsplit(as.character(slums$KYC_Year),".", fixed = TRUE))) 
slums$KYC_Year<-x$X3

#GPWv4.11 (2015)
gpw15 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
gpw15 <- crop(gpw15, bbox)
gpw15 <- projectRaster(gpw15, crs=proj.utm31)
slums$gpw15 <- exact_extract(gpw15, slums, 'sum')
rm(gpw15)

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm31)
slums$gpw20 <- exact_extract(gpw20, slums, 'sum')
rm(gpw20)

#GHS-POP (2015) 
ghs15 <- raster("ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")
ghs15 <- crop(ghs15, bbox_moll)
ghs15 <- projectRaster(ghs15, crs=proj.utm31)
slums$ghs15 <- exact_extract(ghs15, slums, 'sum')
rm(ghs15)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm31)
slums$ghs20 <- exact_extract(ghs20, slums, 'sum')
rm(ghs20)

#HRSL-Facebook (2015)
hrsl15 <- raster("hrsl/2015/hrsl_nga_pop.tif") 
hrsl15 <- crop(hrsl15, bbox)
hrsl15 <- projectRaster(hrsl15, crs=proj.utm31)
slums$hrsl15 <- exact_extract(hrsl15, slums, 'sum')
rm(hrsl15)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/nga_general_2020.tif") 
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm31)
slums$hrsl20 <- exact_extract(hrsl20, slums, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2015)
wp_u15 <- raster("wp_u/nga_ppp_2015_UNadj.tif")
wp_u15 <- crop(wp_u15, bbox)
wp_u15 <- projectRaster(wp_u15, crs=proj.utm31)
slums$wp_u15 <- exact_extract(wp_u15, slums, 'sum')
rm(wp_u15)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/nga_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm31)
slums$wp_u20 <- exact_extract(wp_u20, slums, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/nga_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm31)
slums$wp_c20 <- exact_extract(wp_c20, slums, 'sum')
rm(wp_c20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/NGA_population_202207292317.tif")
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm31)
slums$wp_pb20 <- exact_extract(wp_pb20, slums, 'sum')
rm(wp_pb20)

#LandScan Global (2015)
lsg15 <- raster("landscan/LandScan Global 2015/lspop2015/w001001.adf")
lsg15 <- crop(lsg15, bbox)
lsg15 <- projectRaster(lsg15, crs=proj.utm31)
slums$lsg15 <- exact_extract(lsg15, slums, 'sum')
rm(lsg15)

#LandScan Global (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm31)
slums$lsg20 <- exact_extract(lsg20, slums, 'sum')
rm(lsg20)

#LandScan HD (2020)
lshd20 <- raster("landscan/LandScan HD/landscan-hd-nigeria.tif")
lshd20 <- crop(lshd20, bbox)
lshd20 <- projectRaster(lshd20, crs=proj.utm31)
slums$lshd20 <- exact_extract(lshd20, slums, 'sum')
rm(lshd20)

#GRID3 Nigeria v1.2 (2016)
grid16 <- raster("grid3/NGA - population - v1.2 - gridded/NGA_population_v1_2_gridded.tif")
grid16 <- crop(grid16, bbox)
grid16 <- projectRaster(grid16, crs=proj.utm31)
slums$grid16 <- exact_extract(grid16, slums, 'sum')
rm(grid16)

#GRID3 Nigeria v2.0 (2019)
grid19 <- raster("grid3/NGA - population - v2.0 - gridded/NGA_population_v2_0_gridded.tif")
grid19 <- crop(grid19, bbox)
grid19 <- projectRaster(grid19, crs=proj.utm31)
slums$grid19 <- exact_extract(grid19, slums, 'sum')
rm(grid19)


slums$area_m2 <- st_area(slums)
slums$area_m2 <- as.numeric(slums$area_m2)
slums$area_100m <- slums$area_m2 / 10000
slums$area_km2 <- slums$area_m2 / 1000000

slums_lag <- slums


##### PORT HARCOURT, NIGERIA #####

# KnowYourCity Campaign slum boundaries
slums <- readOGR("kyc_campaign/nga/kyc_cln_data_PortHarcourt_Nigeria.shp") 
slums <- slums[,names(slums) %in% c("Country", "City", "Settlement", "Last_updat", "kyc_pop")]
names(slums) <- c("Country", "City", "Settlement", "KYC_Year", "KYC_Pop")

#create bounding box to crop rasters
bbox <- bb(slums, ext=1.2, output=c("extent"))
bbox_moll <- bb(slums, ext=1.2, projection = proj.moll, output=c("extent"))

#create sf object for exact_extract calcs
slums <- st_as_sf(spTransform(slums, CRS = proj.utm32))

#clean date
x <- data.frame(do.call("rbind", strsplit(as.character(slums$KYC_Year),".", fixed = TRUE))) 
slums$KYC_Year<-x$X3

#GPWv4.11 (2015)
gpw15 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
gpw15 <- crop(gpw15, bbox)
gpw15 <- projectRaster(gpw15, crs=proj.utm31)
slums$gpw15 <- exact_extract(gpw15, slums, 'sum')
rm(gpw15)

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm31)
slums$gpw20 <- exact_extract(gpw20, slums, 'sum')
rm(gpw20)

#GHS-POP (2015) 
ghs15 <- raster("ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")
ghs15 <- crop(ghs15, bbox_moll)
ghs15 <- projectRaster(ghs15, crs=proj.utm31)
slums$ghs15 <- exact_extract(ghs15, slums, 'sum')
rm(ghs15)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm31)
slums$ghs20 <- exact_extract(ghs20, slums, 'sum')
rm(ghs20)

#HRSL-Facebook (2015)
hrsl15 <- raster("hrsl/2015/hrsl_nga_pop.tif") 
hrsl15 <- crop(hrsl15, bbox)
hrsl15 <- projectRaster(hrsl15, crs=proj.utm31)
slums$hrsl15 <- exact_extract(hrsl15, slums, 'sum')
rm(hrsl15)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/nga_general_2020.tif") 
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm31)
slums$hrsl20 <- exact_extract(hrsl20, slums, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2015)
wp_u15 <- raster("wp_u/nga_ppp_2015_UNadj.tif")
wp_u15 <- crop(wp_u15, bbox)
wp_u15 <- projectRaster(wp_u15, crs=proj.utm31)
slums$wp_u15 <- exact_extract(wp_u15, slums, 'sum')
rm(wp_u15)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/nga_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm31)
slums$wp_u20 <- exact_extract(wp_u20, slums, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/nga_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm31)
slums$wp_c20 <- exact_extract(wp_c20, slums, 'sum')
rm(wp_c20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/NGA_population_202207292317.tif")
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm31)
slums$wp_pb20 <- exact_extract(wp_pb20, slums, 'sum')
rm(wp_pb20)

#LandScan Global (2015)
lsg15 <- raster("landscan/LandScan Global 2015/lspop2015/w001001.adf")
lsg15 <- crop(lsg15, bbox)
lsg15 <- projectRaster(lsg15, crs=proj.utm31)
slums$lsg15 <- exact_extract(lsg15, slums, 'sum')
rm(lsg15)

#LandScan Global (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm31)
slums$lsg20 <- exact_extract(lsg20, slums, 'sum')
rm(lsg20)

#LandScan HD (2020)
lshd20 <- raster("landscan/LandScan HD/landscan-hd-nigeria.tif")
lshd20 <- crop(lshd20, bbox)
lshd20 <- projectRaster(lshd20, crs=proj.utm31)
slums$lshd20 <- exact_extract(lshd20, slums, 'sum')
rm(lshd20)

#GRID3 Nigeria v1.2 (2016)
grid16 <- raster("grid3/NGA - population - v1.2 - gridded/NGA_population_v1_2_gridded.tif")
grid16 <- crop(grid16, bbox)
grid16 <- projectRaster(grid16, crs=proj.utm31)
slums$grid16 <- exact_extract(grid16, slums, 'sum')
rm(grid16)

#GRID3 Nigeria v2.0 (2019)
grid19 <- raster("grid3/NGA - population - v2.0 - gridded/NGA_population_v2_0_gridded.tif")
grid19 <- crop(grid19, bbox)
grid19 <- projectRaster(grid19, crs=proj.utm31)
slums$grid19 <- exact_extract(grid19, slums, 'sum')
rm(grid19)

slums$area_m2 <- st_area(slums)
slums$area_m2 <- as.numeric(slums$area_m2)
slums$area_100m <- slums$area_m2 / 10000
slums$area_km2 <- slums$area_m2 / 1000000

slums_phc <- slums



##### NAIROBI, KENYA #####

# KnowYourCity Campaign slum boundaries
slums <- readOGR("kyc_campaign/ken/kyc_cln_data_Nairobi_Kenya.shp") 
slums <- slums[,names(slums) %in% c("Country", "City", "Settlement", "Date_updat", "kyc_pop")]
names(slums) <- c("Country", "City", "Settlement", "KYC_Year", "KYC_Pop")

#create bounding box to crop rasters
bbox <- bb(slums, ext=1.2, output=c("extent"))
bbox_moll <- bb(slums, ext=1.2, projection = proj.moll, output=c("extent"))

#create sf object for exact_extract calcs
slums <- st_as_sf(spTransform(slums, CRS = proj.utm37))

#clean date
x <- data.frame(do.call("rbind", strsplit(as.character(slums$KYC_Year),".", fixed = TRUE))) 
slums$KYC_Year<-x$X3

#GPWv4.11 (2015)
gpw15 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
gpw15 <- crop(gpw15, bbox)
gpw15 <- projectRaster(gpw15, crs=proj.utm31)
slums$gpw15 <- exact_extract(gpw15, slums, 'sum')
rm(gpw15)

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm31)
slums$gpw20 <- exact_extract(gpw20, slums, 'sum')
rm(gpw20)

#GHS-POP (2015)
ghs15 <- raster("ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")
ghs15 <- crop(ghs15, bbox_moll)
ghs15 <- projectRaster(ghs15, crs=proj.utm31)
slums$ghs15 <- exact_extract(ghs15, slums, 'sum')
rm(ghs15)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm31)
slums$ghs20 <- exact_extract(ghs20, slums, 'sum')
rm(ghs20)

#HRSL-Facebook (2015)
hrsl15 <- raster("hrsl/2015/hrsl_ken_pop.tif") 
hrsl15 <- crop(hrsl15, bbox)
hrsl15 <- projectRaster(hrsl15, crs=proj.utm31)
slums$hrsl15 <- exact_extract(hrsl15, slums, 'sum')
rm(hrsl15)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/ken_general_2020.tif") 
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm31)
slums$hrsl20 <- exact_extract(hrsl20, slums, 'sum')
rm(hrsl20)


#WP UN-adj unconstrained (2015)
wp_u15 <- raster("wp_u/ken_ppp_2015_UNadj.tif")
wp_u15 <- crop(wp_u15, bbox)
wp_u15 <- projectRaster(wp_u15, crs=proj.utm31)
slums$wp_u15 <- exact_extract(wp_u15, slums, 'sum')
rm(wp_u15)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/ken_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm31)
slums$wp_u20 <- exact_extract(wp_u20, slums, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/ken_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm31)
slums$wp_c20 <- exact_extract(wp_c20, slums, 'sum')
rm(wp_c20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/KEN_population_202207292310.tif")
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm31)
slums$wp_pb20 <- exact_extract(wp_pb20, slums, 'sum')
rm(wp_pb20)

#LandScan Global (2015)
lsg15 <- raster("landscan/LandScan Global 2015/lspop2015/w001001.adf")
lsg15 <- crop(lsg15, bbox)
lsg15 <- projectRaster(lsg15, crs=proj.utm31)
slums$lsg15 <- exact_extract(lsg15, slums, 'sum')
rm(lsg15)

#LandScan Global (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm31)
slums$lsg20 <- exact_extract(lsg20, slums, 'sum')
rm(lsg20)

#LandScan HD (2020)
slums$lshd20 <- NA

#GRID3
slums$grid16 <- NA
slums$grid19 <- NA

slums$area_m2 <- st_area(slums)
slums$area_m2 <- as.numeric(slums$area_m2)
slums$area_100m <- slums$area_m2 / 10000
slums$area_km2 <- slums$area_m2 / 1000000

slums_nai <- slums


##### KISUMU, KENYA #####

  
# KnowYourCity Campaign slum boundaries
slums <- readOGR("kyc_campaign/ken/kyc_cln_data_Kisumu_Kenya.shp")
slums <- slums[,names(slums) %in% c("Country", "City", "Settlement", "Last_Updat", "pop")]
names(slums) <- c("Country", "City", "Settlement", "KYC_Year", "KYC_Pop")

#create bounding box to crop rasters
bbox <- bb(slums, ext=1.2, output=c("extent"))
bbox_moll <- bb(slums, ext=1.2, projection = proj.moll, output=c("extent"))

#create sf object for exact_extract calcs
slums <- st_as_sf(spTransform(slums, CRS = proj.utm37))

#clean date
x <- data.frame(do.call("rbind", strsplit(as.character(slums$KYC_Year),"-", fixed = TRUE))) 
slums$KYC_Year<-x$X3

#GPWv4.11 (2015)
gpw15 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
gpw15 <- crop(gpw15, bbox)
gpw15 <- projectRaster(gpw15, crs=proj.utm37)
slums$gpw15 <- exact_extract(gpw15, slums, 'sum')
rm(gpw15)

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm37)
slums$gpw20 <- exact_extract(gpw20, slums, 'sum')
rm(gpw20)

#GHS-POP (2015)
ghs15 <- raster("ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")
ghs15 <- crop(ghs15, bbox_moll)
ghs15 <- projectRaster(ghs15, crs=proj.utm37)
slums$ghs15 <- exact_extract(ghs15, slums, 'sum')
rm(ghs15)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm37)
slums$ghs20 <- exact_extract(ghs20, slums, 'sum')
rm(ghs20)

#HRSL-Facebook (2015)
hrsl15 <- raster("hrsl/2015/hrsl_ken_pop.tif")
hrsl15 <- crop(hrsl15, bbox)
hrsl15 <- projectRaster(hrsl15, crs=proj.utm37)
slums$hrsl15 <- exact_extract(hrsl15, slums, 'sum')
rm(hrsl15)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/ken_general_2020.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm37)
slums$hrsl20 <- exact_extract(hrsl20, slums, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2015)
wp_u15 <- raster("wp_u/ken_ppp_2015_UNadj.tif")
wp_u15 <- crop(wp_u15, bbox)
wp_u15 <- projectRaster(wp_u15, crs=proj.utm37)
slums$wp_u15 <- exact_extract(wp_u15, slums, 'sum')
rm(wp_u15)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/ken_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm37)
slums$wp_u20 <- exact_extract(wp_u20, slums, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/ken_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm37)
slums$wp_c20 <- exact_extract(wp_c20, slums, 'sum')
rm(wp_c20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/KEN_population_202207292310.tif")
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm37)
slums$wp_pb20 <- exact_extract(wp_pb20, slums, 'sum')
rm(wp_pb20)

#LandScan Global (2015)
lsg15 <- raster("landscan/LandScan Global 2015/lspop2015/w001001.adf")
lsg15 <- crop(lsg15, bbox)
lsg15 <- projectRaster(lsg15, crs=proj.utm37)
slums$lsg15 <- exact_extract(lsg15, slums, 'sum')
rm(lsg15)

#LandScan Global (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm37)
slums$lsg20 <- exact_extract(lsg20, slums, 'sum')
rm(lsg20)

#LandScan HD (2020)
slums$lshd20 <- NA

#GRID3 
slums$grid16 <- NA
slums$grid19 <- NA

slums$area_m2 <- st_area(slums)
slums$area_m2 <- as.numeric(slums$area_m2)
slums$area_100m <- slums$area_m2 / 10000
slums$area_km2 <- slums$area_m2 / 1000000

slums_kis <- slums



##### DAR ES SALAAM, TANZANIA ######

# KnowYourCity Campaign slum boundaries
slums <- readOGR("kyc_campaign/tza/kyc_cln_data2_DarEsSalaam_Tanzania.shp")
slums <- slums[,names(slums) %in% c("Country", "City", "Settlement", "Last_Updat", "pop")]
names(slums) <- c("Country", "City", "Settlement", "KYC_Year", "KYC_Pop")

#create bounding box to crop rasters
bbox <- bb(slums, ext=1.2, output=c("extent"))
bbox_moll <- bb(slums, ext=1.2, projection = proj.moll, output=c("extent"))

#create sf object for exact_extract calcs
slums <- st_as_sf(spTransform(slums, CRS = proj.utm37))

#clean dates
x <- data.frame(do.call("rbind", strsplit(as.character(slums$KYC_Year),"-", fixed = TRUE))) 
slums$KYC_Year<-x$X3

#GPWv4.11 (2015)
gpw15 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
gpw15 <- crop(gpw15, bbox)
gpw15 <- projectRaster(gpw15, crs=proj.utm37)
slums$gpw15 <- exact_extract(gpw15, slums, 'sum')
rm(gpw15)

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm37)
slums$gpw20 <- exact_extract(gpw20, slums, 'sum')
rm(gpw20)

#GHS-POP (2015)
ghs15 <- raster("ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")
ghs15 <- crop(ghs15, bbox_moll)
ghs15 <- projectRaster(ghs15, crs=proj.utm37)
slums$ghs15 <- exact_extract(ghs15, slums, 'sum')
rm(ghs15)

#GHS-POP (2020) 
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm37)
slums$ghs20 <- exact_extract(ghs20, slums, 'sum')
rm(ghs20)

#HRSL-Facebook (2015)
hrsl15 <- raster("hrsl/2015/hrsl_tza_pop.tif")
hrsl15 <- crop(hrsl15, bbox)
hrsl15 <- projectRaster(hrsl15, crs=proj.utm37)
slums$hrsl15 <- exact_extract(hrsl15, slums, 'sum')
rm(hrsl15)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/tza_general_2020.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm37)
slums$hrsl20 <- exact_extract(hrsl20, slums, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2015)
wp_u15 <- raster("wp_u/tza_ppp_2015_UNadj.tif")
wp_u15 <- crop(wp_u15, bbox)
wp_u15 <- projectRaster(wp_u15, crs=proj.utm37)
slums$wp_u15 <- exact_extract(wp_u15, slums, 'sum')
rm(wp_u15)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/tza_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm37)
slums$wp_u20 <- exact_extract(wp_u20, slums, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/tza_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm37)
slums$wp_c20 <- exact_extract(wp_c20, slums, 'sum')
rm(wp_c20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/TZA_population_202207292319.tif")
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm37)
slums$wp_pb20 <- exact_extract(wp_pb20, slums, 'sum')
rm(wp_pb20)

#LandScan (2015)
lsg15 <- raster("landscan/LandScan Global 2015/lspop2015/w001001.adf")
lsg15 <- crop(lsg15, bbox)
lsg15 <- projectRaster(lsg15, crs=proj.utm37)
slums$lsg15 <- exact_extract(lsg15, slums, 'sum')
rm(lsg15)

#LandScan (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm37)
slums$lsg20 <- exact_extract(lsg20, slums, 'sum')
rm(lsg20)

#LandScan HD (2020)
slums$lshd20 <- NA

#GRID3
slums$grid16 <- NA
slums$grid19 <- NA

slums$area_m2 <- st_area(slums)
slums$area_m2 <- as.numeric(slums$area_m2)
slums$area_100m <- slums$area_m2 / 10000
slums$area_km2 <- slums$area_m2 / 1000000

slums_dar <- slums



##### KAMPALA, UGANDA ######

# KnowYourCity Campaign slum boundaries
slums <- readOGR("kyc_campaign/uga/kyc_cln_data_Kampala_Uganda.shp")
slums <- slums[,names(slums) %in% c("Country", "City", "Settlement", "Last_Updat", "pop")]
names(slums) <- c("Country", "City", "Settlement", "KYC_Year", "KYC_Pop")

#create bounding box to crop rasters
bbox <- bb(slums, ext=1.2, output=c("extent"))
bbox_moll <- bb(slums, ext=1.2, projection = proj.moll, output=c("extent"))

#create sf object for exact_extract calcs
slums <- st_as_sf(spTransform(slums, CRS = proj.utm36))

#clean date
x <- data.frame(do.call("rbind", strsplit(as.character(slums$KYC_Year),"-", fixed = TRUE))) 
slums$KYC_Year<-x$X3

#GPWv4.11 (2015)
gpw15 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
gpw15 <- crop(gpw15, bbox)
gpw15 <- projectRaster(gpw15, crs=proj.utm36)
slums$gpw15 <- exact_extract(gpw15, slums, 'sum')
rm(gpw15)

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm36)
slums$gpw20 <- exact_extract(gpw20, slums, 'sum')
rm(gpw20)

#GHS-POP (2015)
ghs15 <- raster("ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")
ghs15 <- crop(ghs15, bbox_moll)
ghs15 <- projectRaster(ghs15, crs=proj.utm36)
slums$ghs15 <- exact_extract(ghs15, slums, 'sum')
rm(ghs15)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm37)
slums$ghs20 <- exact_extract(ghs20, slums, 'sum')
rm(ghs20)

#HRSL-Facebook (2015)
hrsl15 <- raster("hrsl/2015/hrsl_uga_pop.tif")
hrsl15 <- crop(hrsl15, bbox)
hrsl15 <- projectRaster(hrsl15, crs=proj.utm36)
slums$hrsl15 <- exact_extract(hrsl15, slums, 'sum')
rm(hrsl15)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/uga_general_2020.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm36)
slums$hrsl20 <- exact_extract(hrsl20, slums, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2015)
wp_u15 <- raster("wp_u/uga_ppp_2015_UNadj.tif")
wp_u15 <- crop(wp_u15, bbox)
wp_u15 <- projectRaster(wp_u15, crs=proj.utm36)
slums$wp_u15 <- exact_extract(wp_u15, slums, 'sum')
rm(wp_u15)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/uga_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm36)
slums$wp_u20 <- exact_extract(wp_u20, slums, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/uga_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm36)
slums$wp_c20 <- exact_extract(wp_c20, slums, 'sum')
rm(wp_c20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/UGA_population_202207292321.tif") 
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm36)
slums$wp_pb20 <- exact_extract(wp_pb20, slums, 'sum')
rm(wp_pb20)

#LandScan (2015)
lsg15 <- raster("landscan/LandScan Global 2015/lspop2015/w001001.adf")
lsg15 <- crop(lsg15, bbox)
lsg15 <- projectRaster(lsg15, crs=proj.utm36)
slums$lsg15 <- exact_extract(lsg15, slums, 'sum')
rm(lsg15)

#LandScan (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm36)
slums$lsg20 <- exact_extract(lsg20, slums, 'sum')
rm(lsg20)

#LandScan HD (2020)
slums$lshd20 <- NA

#GRID3
slums$grid16 <- NA
slums$grid19 <- NA

slums$area_m2 <- st_area(slums)
slums$area_m2 <- as.numeric(slums$area_m2)
slums$area_100m <- slums$area_m2 / 10000
slums$area_km2 <- slums$area_m2 / 1000000

slums_kam <- slums




##### CAPE TOWN, SOUTH AFRICA #####

# KnowYourCity Campaign slum boundaries
slums <- readOGR("kyc_campaign/zaf/kyc_cln_data_CapeTown_SouthAfrica.shp")
slums <- slums[,names(slums) %in% c("Country", "City", "Settlement", "Last_Updat", "pop")]
names(slums) <- c("Country", "City", "Settlement", "KYC_Year", "KYC_Pop")

#create bounding box to crop rasters
bbox <- bb(slums, ext=1.2, output=c("extent"))
bbox_moll <- bb(slums, ext=1.2, projection = proj.moll, output=c("extent"))

#create sf object for exact_extract calcs
slums <- st_as_sf(spTransform(slums, CRS = proj.utm33))

#clean date
x <- data.frame(do.call("rbind", strsplit(as.character(slums$KYC_Year),"-", fixed = TRUE))) 
slums$KYC_Year<-x$X3

#GPWv4.11 (2015)
gpw15 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
gpw15 <- crop(gpw15, bbox)
gpw15 <- projectRaster(gpw15, crs=proj.utm33)
slums$gpw15 <- exact_extract(gpw15, slums, 'sum')
rm(gpw15)

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm33)
slums$gpw20 <- exact_extract(gpw20, slums, 'sum')
rm(gpw20)

#GHS-POP (2015)
ghs15 <- raster("ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")
ghs15 <- crop(ghs15, bbox_moll)
ghs15 <- projectRaster(ghs15, crs=proj.utm33)
slums$ghs15 <- exact_extract(ghs15, slums, 'sum')
rm(ghs15)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm37)
slums$ghs20 <- exact_extract(ghs20, slums, 'sum')
rm(ghs20)

#HRSL-Facebook (2015)
hrsl15 <- raster("hrsl/2015/hrsl_zaf_pop.tif")
hrsl15 <- crop(hrsl15, bbox)
hrsl15 <- projectRaster(hrsl15, crs=proj.utm33)
slums$hrsl15 <- exact_extract(hrsl15, slums, 'sum')
rm(hrsl15)

#HRSL-Facebook (2020)
#hrsl20 <- raster("hrsl/2018-2020/zaf_general_2020.tif") ### something is wrong with this dataset - unusable
#hrsl20 <- crop(hrsl20, bbox)
#hrsl20 <- projectRaster(hrsl20, crs=proj.utm33)
#slums$hrsl20 <- exact_extract(hrsl20, slums, 'sum')
#rm(hrsl20)
slums$hrsl20 <- NA

#WP UN-adj unconstrained (2015)
wp_u15 <- raster("wp_u/zaf_ppp_2015_UNadj.tif")
wp_u15 <- crop(wp_u15, bbox)
wp_u15 <- projectRaster(wp_u15, crs=proj.utm33)
slums$wp_u15 <- exact_extract(wp_u15, slums, 'sum')
rm(wp_u15)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/zaf_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm33)
slums$wp_u20 <- exact_extract(wp_u20, slums, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/zaf_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm33)
slums$wp_c20 <- exact_extract(wp_c20, slums, 'sum')
rm(wp_c20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/ZAF_population_202207292324.tif")
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm33)
slums$wp_pb20 <- exact_extract(wp_pb20, slums, 'sum')
rm(wp_pb20)

#LandScan (2015)
lsg15 <- raster("landscan/LandScan Global 2015/lspop2015/w001001.adf")
lsg15 <- crop(lsg15, bbox)
lsg15 <- projectRaster(lsg15, crs=proj.utm33)
slums$lsg15 <- exact_extract(lsg15, slums, 'sum')
rm(lsg15)

#LandScan (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm33)
slums$lsg20 <- exact_extract(lsg20, slums, 'sum')
rm(lsg20)

#LandScan HD (2020)
slums$lshd20 <- NA

#GRID3 
slums$grid16 <- NA
slums$grid19 <- NA

slums$area_m2 <- st_area(slums)
slums$area_m2 <- as.numeric(slums$area_m2)
slums$area_100m <- slums$area_m2 / 10000
slums$area_km2 <- slums$area_m2 / 1000000

slums_cap <- slums


##### ACCRA, GHANA ######

# KnowYourCity Campaign slum boundaries
slums <- readOGR("kyc_campaign/gha/kyc_cln_data_Accra_Ghana.shp")
slums <- slums[,names(slums) %in% c("Country", "City", "Settlement", "Last_Updat", "pop")]
names(slums) <- c("Country", "City", "Settlement", "KYC_Year", "KYC_Pop")

#create bounding box to crop rasters
bbox <- bb(slums, ext=1.2, output=c("extent"))
bbox_moll <- bb(slums, ext=1.2, projection = proj.moll, output=c("extent"))

#create sf object for exact_extract calcs
slums <- st_as_sf(spTransform(slums, CRS = proj.utm30))

#clean date
x <- data.frame(do.call("rbind", strsplit(as.character(slums$KYC_Year),"-", fixed = TRUE))) 
slums$KYC_Year<-x$X3

#GPWv4.11 (2015)
gpw15 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
gpw15 <- crop(gpw15, bbox)
gpw15 <- projectRaster(gpw15, crs=proj.utm30)
slums$gpw15 <- exact_extract(gpw15, slums, 'sum')
rm(gpw15)

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm30)
slums$gpw20 <- exact_extract(gpw20, slums, 'sum')
rm(gpw20)

#GHS-POP (2015)
ghs15 <- raster("ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")
ghs15 <- crop(ghs15, bbox_moll)
ghs15 <- projectRaster(ghs15, crs=proj.utm30)
slums$ghs15 <- exact_extract(ghs15, slums, 'sum')
rm(ghs15)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm37)
slums$ghs20 <- exact_extract(ghs20, slums, 'sum')
rm(ghs20)

#HRSL-Facebook (2015)
hrsl15 <- raster("hrsl/2015/hrsl_gha_pop.tif")
hrsl15 <- crop(hrsl15, bbox)
hrsl15 <- projectRaster(hrsl15, crs=proj.utm30)
slums$hrsl15 <- exact_extract(hrsl15, slums, 'sum')
rm(hrsl15)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/gha_general_2020.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm30)
slums$hrsl20 <- exact_extract(hrsl20, slums, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2015)
wp_u15 <- raster("wp_u/gha_ppp_2015_UNadj.tif")
wp_u15 <- crop(wp_u15, bbox)
wp_u15 <- projectRaster(wp_u15, crs=proj.utm30)
slums$wp_u15 <- exact_extract(wp_u15, slums, 'sum')
rm(wp_u15)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/gha_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm30)
slums$wp_u20 <- exact_extract(wp_u20, slums, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/gha_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm30)
slums$wp_c20 <- exact_extract(wp_c20, slums, 'sum')
rm(wp_c20)

#WP peanutButter (2020)
wp_pb20 <- raster("wp_pb/GHA_population_202207292314.tif") 
wp_pb20 <- crop(wp_pb20, bbox)
wp_pb20 <- projectRaster(wp_pb20, crs=proj.utm30)
slums$wp_pb20 <- exact_extract(wp_pb20, slums, 'sum')
rm(wp_pb20)

#LandScan (2015)
lsg15 <- raster("landscan/LandScan Global 2015/lspop2015/w001001.adf")
lsg15 <- crop(lsg15, bbox)
lsg15 <- projectRaster(lsg15, crs=proj.utm30)
slums$lsg15 <- exact_extract(lsg15, slums, 'sum')
rm(lsg15)

#LandScan (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm30)
slums$lsg20 <- exact_extract(lsg20, slums, 'sum')
rm(lsg20)

#LandScan HD (2020)
slums$lshd20 <- NA

#GRID3
slums$grid16 <- NA
slums$grid19 <- NA

slums$area_m2 <- st_area(slums)
slums$area_m2 <- as.numeric(slums$area_m2)
slums$area_100m <- slums$area_m2 / 10000
slums$area_km2 <- slums$area_m2 / 1000000

slums_acc <- slums



##### DAVAO, PHILIPPINES ######

# KnowYourCity Campaign slum boundaries
slums <- readOGR("kyc_campaign/phl/kyc_cln_data_Davao_Philippines.shp")
slums <- slums[,names(slums) %in% c("Country", "City", "Settlement", "Last_Updat", "pop")]
names(slums) <- c("Country", "City", "Settlement", "KYC_Year", "KYC_Pop")

#create bounding box to crop rasters
bbox <- bb(slums, ext=1.2, output=c("extent"))
bbox_moll <- bb(slums, ext=1.2, projection = proj.moll, output=c("extent"))

#create sf object for exact_extract calcs
slums <- st_as_sf(spTransform(slums, CRS = proj.utm51))

#clean date
x <- data.frame(do.call("rbind", strsplit(as.character(slums$KYC_Year),"-", fixed = TRUE))) 
slums$KYC_Year<-x$X3

#GPWv4.11 (2015)
gpw15 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
gpw15 <- crop(gpw15, bbox)
gpw15 <- projectRaster(gpw15, crs=proj.utm51)
slums$gpw15 <- exact_extract(gpw15, slums, 'sum')
rm(gpw15)

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm51)
slums$gpw20 <- exact_extract(gpw20, slums, 'sum')
rm(gpw20)

#GHS-POP (2015)
ghs15 <- raster("ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")
ghs15 <- crop(ghs15, bbox_moll)
ghs15 <- projectRaster(ghs15, crs=proj.utm51)
slums$ghs15 <- exact_extract(ghs15, slums, 'sum')
rm(ghs15)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm51)
slums$ghs20 <- exact_extract(ghs20, slums, 'sum')
rm(ghs20)

#HRSL-Facebook (2015)
hrsl15 <- raster("hrsl/2015/hrsl_phl_pop.tif")
hrsl15 <- crop(hrsl15, bbox)
hrsl15 <- projectRaster(hrsl15, crs=proj.utm51)
slums$hrsl15 <- exact_extract(hrsl15, slums, 'sum')
rm(hrsl15)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/population_phl_2018-10-01.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm51)
slums$hrsl20 <- exact_extract(hrsl20, slums, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2015)
wp_u15 <- raster("wp_u/phl_ppp_2015_UNadj.tif")
wp_u15 <- crop(wp_u15, bbox)
wp_u15 <- projectRaster(wp_u15, crs=proj.utm51)
slums$wp_u15 <- exact_extract(wp_u15, slums, 'sum')
rm(wp_u15)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/phl_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm51)
slums$wp_u20 <- exact_extract(wp_u20, slums, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/phl_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm51)
slums$wp_c20 <- exact_extract(wp_c20, slums, 'sum')
rm(wp_c20)

#WP peanutButter (2020)
slums$wp_pb20 <- NA

#LandScan (2015)
lsg15 <- raster("landscan/LandScan Global 2015/lspop2015/w001001.adf")
lsg15 <- crop(lsg15, bbox)
lsg15 <- projectRaster(lsg15, crs=proj.utm51)
slums$lsg15 <- exact_extract(lsg15, slums, 'sum')
rm(lsg15)

#LandScan (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm51)
slums$lsg20 <- exact_extract(lsg20, slums, 'sum')
rm(lsg20)

#LandScan HD (2020)
slums$lshd20 <- NA

#GRID3 
slums$grid16 <- NA
slums$grid19 <- NA

slums$area_m2 <- st_area(slums)
slums$area_m2 <- as.numeric(slums$area_m2)
slums$area_100m <- slums$area_m2 / 10000
slums$area_km2 <- slums$area_m2 / 1000000

slums_dav <- slums


##### MALABON, PHILIPPINES ######

# KnowYourCity Campaign slum boundaries
slums <- readOGR("kyc_campaign/phl/kyc_cln_data_Malabon_Philippines.shp")
slums <- slums[,names(slums) %in% c("Country", "City", "Settlement", "Last_Updat", "pop")]
names(slums) <- c("Country", "City", "Settlement", "KYC_Year", "KYC_Pop")

#create bounding box to crop rasters
bbox <- bb(slums, ext=1.2, output=c("extent"))
bbox_moll <- bb(slums, ext=1.2, projection = proj.moll, output=c("extent"))

#create sf object for exact_extract calcs
slums <- st_as_sf(spTransform(slums, CRS = proj.utm51))

#clean date
x <- data.frame(do.call("rbind", strsplit(as.character(slums$KYC_Year),"-", fixed = TRUE))) 
slums$KYC_Year<-x$X3

#GPWv4.11 (2015)
gpw15 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
gpw15 <- crop(gpw15, bbox)
gpw15 <- projectRaster(gpw15, crs=proj.utm51)
slums$gpw15 <- exact_extract(gpw15, slums, 'sum')
rm(gpw15)

#GPWv4.11 (2020)
gpw20 <- raster("gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
gpw20 <- crop(gpw20, bbox)
gpw20 <- projectRaster(gpw20, crs=proj.utm51)
slums$gpw20 <- exact_extract(gpw20, slums, 'sum')
rm(gpw20)

#GHS-POP (2015)
# Global
ghs15 <- raster("ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif")
ghs15 <- crop(ghs15, bbox_moll)
ghs15 <- projectRaster(ghs15, crs=proj.utm51)
slums$ghs15 <- exact_extract(ghs15, slums, 'sum')
rm(ghs15)

#GHS-POP (2020)
ghs20 <- raster("ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif")
ghs20 <- crop(ghs20, bbox_moll)
ghs20 <- projectRaster(ghs20, crs=proj.utm37)
slums$ghs20 <- exact_extract(ghs20, slums, 'sum')
rm(ghs20)

#HRSL-Facebook (2015)
hrsl15 <- raster("hrsl/2015/hrsl_phl_pop.tif")
hrsl15 <- crop(hrsl15, bbox)
hrsl15 <- projectRaster(hrsl15, crs=proj.utm51)
slums$hrsl15 <- exact_extract(hrsl15, slums, 'sum')
rm(hrsl15)

#HRSL-Facebook (2020)
hrsl20 <- raster("hrsl/2018-2020/population_phl_2018-10-01.tif")
hrsl20 <- crop(hrsl20, bbox)
hrsl20 <- projectRaster(hrsl20, crs=proj.utm51)
slums$hrsl20 <- exact_extract(hrsl20, slums, 'sum')
rm(hrsl20)

#WP UN-adj unconstrained (2015)
wp_u15 <- raster("wp_u/phl_ppp_2015_UNadj.tif")
wp_u15 <- crop(wp_u15, bbox)
wp_u15 <- projectRaster(wp_u15, crs=proj.utm51)
slums$wp_u15 <- exact_extract(wp_u15, slums, 'sum')
rm(wp_u15)

#WP UN-adj unconstrained (2020)
wp_u20 <- raster("wp_u/phl_ppp_2020_UNadj.tif")
wp_u20 <- crop(wp_u20, bbox)
wp_u20 <- projectRaster(wp_u20, crs=proj.utm51)
slums$wp_u20 <- exact_extract(wp_u20, slums, 'sum')
rm(wp_u20)

#WP UN-adj constrained (2020)
wp_c20 <- raster("wp_c/phl_ppp_2020_UNadj_constrained.tif")
wp_c20 <- crop(wp_c20, bbox)
wp_c20 <- projectRaster(wp_c20, crs=proj.utm51)
slums$wp_c20 <- exact_extract(wp_c20, slums, 'sum')
rm(wp_c20)

#WP peanutButter (2020)
slums$wp_pb20 <- NA

#LandScan (2015)
lsg15 <- raster("landscan/LandScan Global 2015/lspop2015/w001001.adf")
lsg15 <- crop(lsg15, bbox)
lsg15 <- projectRaster(lsg15, crs=proj.utm51)
slums$lsg15 <- exact_extract(lsg15, slums, 'sum')
rm(lsg15)

#LandScan (2020)
lsg20 <- raster("landscan/LandScan Global 2020/landscan-global-2020.tif")
lsg20 <- crop(lsg20, bbox)
lsg20 <- projectRaster(lsg20, crs=proj.utm51)
slums$lsg20 <- exact_extract(lsg20, slums, 'sum')
rm(lsg20)

#LandScan HD (2020)
slums$lshd20 <- NA

#GRID3
slums$grid16 <- NA
slums$grid19 <- NA

slums$area_m2 <- st_area(slums)
slums$area_m2 <- as.numeric(slums$area_m2)
slums$area_100m <- slums$area_m2 / 10000
slums$area_km2 <- slums$area_m2 / 1000000

slums_mal <- slums


######################
## Combine data
######################

# Convert SF to dataframe
slums_lag <- as.data.frame(slums_lag)   
slums_phc <- as.data.frame(slums_phc)   
slums_nai <- as.data.frame(slums_nai)
slums_kis <- as.data.frame(slums_kis)  
slums_dar <- as.data.frame(slums_dar)  
slums_cap <- as.data.frame(slums_cap)  
slums_acc <- as.data.frame(slums_acc) 
slums_kam <- as.data.frame(slums_kam)  
slums_dav <- as.data.frame(slums_dav)  
slums_mal <- as.data.frame(slums_mal) 

names(slums_lag)
names(slums_phc)
names(slums_nai)
names(slums_kis)
names(slums_dar)
names(slums_cap)
names(slums_acc)
names(slums_kam)
names(slums_dav)
names(slums_mal)

# combine data from all cities and clean up - 
slums <- rbind(slums_lag, slums_phc, slums_nai, slums_kis, slums_dar, slums_cap, slums_acc, slums_kam, slums_dav, slums_mal)
slums <- slums[,c(-6)] #drop geometry column
slums$Id <- 1:nrow(slums)
slums$KYC_Year <- as.numeric(as.character(slums$KYC_Year))

#align gridded estimates by year of KYC data collection
slums$gpw_combo <- ifelse(slums$KYC_Year>=2017, slums$gpw20, slums$gpw15)
slums$ghs_combo <- ifelse(slums$KYC_Year>=2017, slums$ghs20, slums$ghs15)
slums$hrsl_combo <- ifelse(slums$KYC_Year>=2017, slums$hrsl20, slums$hrsl15)
  slums$hrsl_combo[slums$Country=="South Africa"] <- NA  ###### We exclude South Africa HRSL because 2020 dataset is wonkie
slums$wp_u_combo <- ifelse(slums$KYC_Year>=2017, slums$wp_u20, slums$wp_u15)
slums$wp_c_combo <- slums$wp_c20 
slums$wp_pb_combo <- slums$wp_pb20 
slums$lsg_combo <- ifelse(slums$KYC_Year>=2017, slums$lsg20, slums$lsg15)
slums$lshd_combo <- slums$lshd20
slums$grid_combo <- ifelse(slums$KYC_Year>=2017, slums$grid19, slums$grid16)

# Densities per 100x100m
slums$dens_kyc <- slums$KYC_Pop / slums$area_100m
slums$dens_gpw <- slums$gpw_combo / slums$area_100m
slums$dens_ghs <- slums$ghs_combo / slums$area_100m
slums$dens_hrsl <- slums$hrsl_combo / slums$area_100m
slums$dens_wp_u <- slums$wp_u_combo / slums$area_100m
slums$dens_wp_c <- slums$wp_c_combo / slums$area_100m
slums$dens_wp_pb <- slums$wp_pb_combo / slums$area_100m
slums$dens_lsg <- slums$lsg_combo / slums$area_100m
slums$dens_lshd <- slums$lshd_combo / slums$area_100m
slums$dens_grid <- slums$grid_combo / slums$area_100m


# clean data
rm(bbox, bbox_moll, x, slums_dar, slums_cap, slums_kis, slums_kam, slums_dav, slums_mal, slums_acc, slums_lag, slums_phc, slums_nai)

# save dataset for accuracy analysis
write.csv(slums, file="kyc_campaign/COMBINED/slums_v10.csv")  # includes estimates for all settlements
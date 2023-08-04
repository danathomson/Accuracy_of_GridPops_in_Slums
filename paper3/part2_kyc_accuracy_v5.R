##############################################################
### Comparison of estimates populations in 12 cities       ###
### v3 includes gridded estimates for all settlements      ###
### By: Dana R Thomson (dana.r.thomson@gmail.com) and      ###
###     Hazem Mahmoud (Hazem.mahmoud@my.utsa.edu)          ###
### Aug 2023                                               ###
### Step 2: KYC accuracy assessment                        ###
##############################################################

# This project is funded by CIESIN, Columbia University
# Objectives:
#   1. Compare gridded density estimates to field density estimates in 12 cities
#      where SDI enumerated 12+ slums (https://knowyourcity.info/explore-our-data/)
#   2. Calculate SDG11.1.1 in 6 African & Asian cities with complete slum area maps


####################################    SET-UP    #######################################


######################
## Initialize
######################

rm(list=ls())

setwd("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/data")   

#packages (versions)
# R version 4.3.1 
library("data.table")   #v1.14.8 
library("dplyr")        #v1.1.2 
library("ggplot2")      #v3.4.2  
library("readxl")       #v1.4.3  
library("scales")       #v1.2.1
library("gridExtra")    #v2.3
library("corrplot")     #v0.92
library("relaimpo")     #v2.2-6 


#library(NCmisc)
#list.functions.in.file("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/code/paper3/part2_kyc_accuracy_v4.R", alphabetic = TRUE)
#print(sessionInfo())


#Read analysis dataset & merge city characteristics

cities <- read_excel("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/cities_data_v3.xlsx", sheet = "cities - analysis 1")
cities <- cities[,c("City", "GHS_FUA_density", "GHS_UC_density", "class_pop", "class_density")]
cities$dens100m_FUA <- cities$GHS_FUA_density / 100
cities$dens100m_UC <- cities$GHS_UC_density / 100
slums <- read.csv("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/data/kyc_campaign/COMBINED/slums_v10.csv")
slums <- merge(slums, cities, by="City", all=T)



######################
##  Calculations 
######################

#######
# Settlement-level error statistics (per hectare - 100x100m)
#######

#differences btw KYC densities & gridded population densities 
slums$diff_gpw <- slums$dens_gpw - slums$dens_kyc
slums$diff_wp_u <- slums$dens_wp_u - slums$dens_kyc
slums$diff_lsg <- slums$dens_lsg - slums$dens_kyc
slums$diff_lshd <- slums$dens_lshd - slums$dens_kyc
slums$diff_ghs <- slums$dens_ghs - slums$dens_kyc
slums$diff_grid <- slums$dens_grid - slums$dens_kyc
slums$diff_hrsl <- slums$dens_hrsl - slums$dens_kyc
slums$diff_wp_pb <- slums$dens_wp_pb - slums$dens_kyc
slums$diff_wp_c <- slums$dens_wp_c - slums$dens_kyc

#absolute differences btw KYC density & gridded population density
slums$adiff_gpw <- abs(slums$diff_gpw)
slums$adiff_wp_u <- abs(slums$diff_wp_u)
slums$adiff_lsg <- abs(slums$diff_lsg)
slums$adiff_lshd <- abs(slums$diff_lshd)
slums$adiff_ghs <- abs(slums$diff_ghs)
slums$adiff_grid <- abs(slums$diff_grid)
slums$adiff_hrsl <- abs(slums$diff_hrsl)
slums$adiff_wp_pb <- abs(slums$diff_wp_pb)
slums$adiff_wp_c <- abs(slums$diff_wp_c)

#differences btw KYC density & gridded population density - squared
slums$diff2_gpw <- slums$diff_gpw^2
slums$diff2_wp_u <- slums$diff_wp_u^2
slums$diff2_lsg <- slums$diff_lsg^2
slums$diff2_lshd <- slums$diff_lshd^2
slums$diff2_ghs <- slums$diff_ghs^2
slums$diff2_grid <- slums$diff_grid^2
slums$diff2_hrsl <- slums$diff_hrsl^2
slums$diff2_wp_pb <- slums$diff_wp_pb^2
slums$diff2_wp_c <- slums$diff_wp_c^2

#fraction of KYC population estimated by gridded population 
slums$frac_gpw <- (slums$gpw_combo / slums$KYC_Pop) 
slums$frac_wp_u <- (slums$wp_u_combo / slums$KYC_Pop) 
slums$frac_lsg <- (slums$lsg_combo / slums$KYC_Pop)
slums$frac_lshd <- (slums$lshd_combo / slums$KYC_Pop)
slums$frac_ghs <- (slums$ghs_combo / slums$KYC_Pop) 
slums$frac_grid <- (slums$grid_combo / slums$KYC_Pop)
slums$frac_hrsl <- (slums$hrsl_combo / slums$KYC_Pop) 
slums$frac_wp_pb <- (slums$wp_pb_combo / slums$KYC_Pop) 
slums$frac_wp_c <- (slums$wp_c_combo / slums$KYC_Pop)


# Slum density MAE by dataset
gpw <- round(sum(slums$adiff_gpw, na.rm = TRUE) / NROW(which(!is.na(slums$adiff_gpw))), digits = 0) 
wp_u <- round(sum(slums$adiff_wp_u, na.rm = TRUE) / NROW(which(!is.na(slums$adiff_wp_u))), digits = 0) 
lsg <- round(sum(slums$adiff_lsg, na.rm = TRUE) / NROW(which(!is.na(slums$adiff_lsg))), digits = 0) 
lshd <- round(sum(slums$adiff_lshd, na.rm = TRUE) / NROW(which(!is.na(slums$adiff_lshd))), digits = 0) 
ghs <- round(sum(slums$adiff_ghs, na.rm = TRUE)  / NROW(which(!is.na(slums$adiff_ghs))), digits = 0) 
grid <- round(sum(slums$adiff_grid, na.rm = TRUE) / NROW(which(!is.na(slums$adiff_grid))), digits = 0) 
wp_pb <- round(sum(slums$adiff_wp_pb, na.rm = TRUE) / NROW(which(!is.na(slums$adiff_wp_pb))), digits = 0) 
wp_c <- round(sum(slums$adiff_wp_c, na.rm = TRUE) / NROW(which(!is.na(slums$adiff_wp_c))), digits = 0) 
hrsl <- round(sum(slums$adiff_hrsl, na.rm = TRUE) / NROW(which(!is.na(slums$adiff_hrsl))), digits = 0) 

mae <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(mae)[1] <- "mae"


# Slum density RMSE by dataset
gpw <- round(sqrt( sum(slums$diff2_gpw, na.rm = TRUE) / NROW(which(!is.na(slums$diff2_gpw))) ), digits = 0) 
wp_u <- round(sqrt( sum(slums$diff2_wp_u, na.rm = TRUE) / NROW(which(!is.na(slums$diff2_wp_u))) ), digits = 0) 
lsg <- round(sqrt( sum(slums$diff2_lsg, na.rm = TRUE) / NROW(which(!is.na(slums$diff2_lsg))) ), digits = 0) 
lshd <- round(sqrt( sum(slums$diff2_lshd, na.rm = TRUE) / NROW(which(!is.na(slums$diff2_lshd))) ), digits = 0) 
ghs <- round(sqrt( sum(slums$diff2_ghs, na.rm = TRUE)  / NROW(which(!is.na(slums$diff2_ghs))) ), digits = 0) 
grid <- round(sqrt( sum(slums$diff2_grid, na.rm = TRUE) / NROW(which(!is.na(slums$diff2_grid))) ), digits = 0) 
wp_pb <- round(sqrt( sum(slums$diff2_wp_pb, na.rm = TRUE) / NROW(which(!is.na(slums$diff2_wp_pb))) ), digits = 0) 
wp_c <- round(sqrt( sum(slums$diff2_wp_c, na.rm = TRUE) / NROW(which(!is.na(slums$diff2_wp_c))) ), digits = 0) 
hrsl <- round(sqrt( sum(slums$diff2_hrsl, na.rm = TRUE) / NROW(which(!is.na(slums$diff2_hrsl))) ), digits = 0) 

rmse <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(rmse)[1] <- "rmse"

# Median fraction of KYC pop estimated by gridded pop
gpw <- round(median(slums$frac_gpw, na.rm = TRUE), digits = 3) 
wp_u <- round(median(slums$frac_wp_u, na.rm = TRUE), digits = 3) 
lsg <- round(median(slums$frac_lsg, na.rm = TRUE), digits = 3) 
lshd <- round(median(slums$frac_lshd, na.rm = TRUE), digits = 3) 
ghs <- round(median(slums$frac_ghs, na.rm = TRUE), digits = 3)  
grid <- round(median(slums$frac_grid, na.rm = TRUE), digits = 3) 
wp_pb <- round(median(slums$frac_wp_pb, na.rm = TRUE), digits = 3) 
wp_c <- round(median(slums$frac_wp_c, na.rm = TRUE), digits = 3) 
hrsl <- round(median(slums$frac_hrsl, na.rm = TRUE), digits = 3)  

frac <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(frac)[1] <- "MF"


# Count (n) settlements with population estimate
gpw <- as.numeric(NROW(which(!is.na(slums$gpw_combo))))
wp_u <- as.numeric(NROW(which(!is.na(slums$wp_u_combo))))
lsg <- as.numeric(NROW(which(!is.na(slums$lsg_combo))))
lshd <- as.numeric(NROW(which(!is.na(slums$lshd_combo))))
ghs <- as.numeric(NROW(which(!is.na(slums$ghs_combo)))) 
grid <- as.numeric(NROW(which(!is.na(slums$grid_combo)))) 
wp_pb <- as.numeric(NROW(which(!is.na(slums$wp_pb_combo)))) 
wp_c <- as.numeric(NROW(which(!is.na(slums$wp_c_combo)))) 
hrsl <- as.numeric(NROW(which(!is.na(slums$hrsl_combo))))

n <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(n)[1] <- "n"

datasets <- as.data.frame(rbind("GPWv4.11", "WorldPop-U", "LandScan-G", "LandScan-HD", "GHS-POP2", "GRID3", "WorldPop-PB", "WorldPop-C", "HRSL"))

errorstats_wide <- cbind(datasets, n, mae,  rmse, frac)
errorstats_wide$label <- paste0(errorstats_wide$V1, " (n=", errorstats_wide$n, ")")
errorstats_wide <- errorstats_wide[order(-errorstats_wide$MF),]

rm(datasets, mae, rmse, frac, n)
rm(ghs, gpw, grid, hrsl, lsg, lshd, wp_c, wp_u, wp_pb)


#####
#summary to accompany fig2 box plot
#####

## 100m calcs
slums_dt <- data.table(slums)
slums_summary <- slums_dt[,list(KYC_N=.N, KYC_Mean_100m=round(mean(dens_kyc),0), KYC_SD_100m=round(sd(dens_kyc),0)), by=City]
slums <- merge(slums,slums_summary, by="City")
cities <- merge(cities, slums_summary, by="City")
rm(slums_dt, slums_summary)


############## 
# Settlement-level Overall error statistics 
##############

write.csv(errorstats_wide, file="C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/t1_error_stats_v4.csv")   # for data quality check of reported vs digitised area

errorstats_long <- errorstats_wide[, c("label", "mae", "rmse", "MF")]
errorstats_long <- melt(setDT(errorstats_long), id.vars = c("label", "MF"), variable.name = "stat")


#######
# Pooled City-level error statistics (per hectare - 100x100m)
#######

slums_agg <- slums %>% 
  group_by(City) %>% 
  summarise(across(where(is.numeric), sum))
slums_agg <- slums_agg[,c("City", "KYC_Pop", "gpw_combo", "ghs_combo", "hrsl_combo", "wp_u_combo", "wp_c_combo", "wp_pb_combo", "lsg_combo", "lshd_combo", "grid_combo", "area_m2", "area_100m", "area_km2")]


# Densities per 100x100m
slums_agg$dens_kyc <- slums_agg$KYC_Pop / slums_agg$area_100m
slums_agg$dens_gpw <- slums_agg$gpw_combo / slums_agg$area_100m
slums_agg$dens_ghs <- slums_agg$ghs_combo / slums_agg$area_100m
slums_agg$dens_hrsl <- slums_agg$hrsl_combo / slums_agg$area_100m
slums_agg$dens_wp_u <- slums_agg$wp_u_combo / slums_agg$area_100m
slums_agg$dens_wp_c <- slums_agg$wp_c_combo / slums_agg$area_100m
slums_agg$dens_wp_pb <- slums_agg$wp_pb_combo / slums_agg$area_100m
slums_agg$dens_lsg <- slums_agg$lsg_combo / slums_agg$area_100m
slums_agg$dens_lshd <- slums_agg$lshd_combo / slums_agg$area_100m
slums_agg$dens_grid <- slums_agg$grid_combo / slums_agg$area_100m


#differences btw KYC densities & gridded population densities 
slums_agg$diff_gpw <- slums_agg$dens_gpw - slums_agg$dens_kyc
slums_agg$diff_wp_u <- slums_agg$dens_wp_u - slums_agg$dens_kyc
slums_agg$diff_lsg <- slums_agg$dens_lsg - slums_agg$dens_kyc
slums_agg$diff_lshd <- slums_agg$dens_lshd - slums_agg$dens_kyc
slums_agg$diff_ghs <- slums_agg$dens_ghs - slums_agg$dens_kyc
slums_agg$diff_grid <- slums_agg$dens_grid - slums_agg$dens_kyc
slums_agg$diff_hrsl <- slums_agg$dens_hrsl - slums_agg$dens_kyc
slums_agg$diff_wp_pb <- slums_agg$dens_wp_pb - slums_agg$dens_kyc
slums_agg$diff_wp_c <- slums_agg$dens_wp_c - slums_agg$dens_kyc

#absolute differences btw KYC density & gridded population density
slums_agg$adiff_gpw <- abs(slums_agg$diff_gpw)
slums_agg$adiff_wp_u <- abs(slums_agg$diff_wp_u)
slums_agg$adiff_lsg <- abs(slums_agg$diff_lsg)
slums_agg$adiff_lshd <- abs(slums_agg$diff_lshd)
slums_agg$adiff_ghs <- abs(slums_agg$diff_ghs)
slums_agg$adiff_grid <- abs(slums_agg$diff_grid)
slums_agg$adiff_hrsl <- abs(slums_agg$diff_hrsl)
slums_agg$adiff_wp_pb <- abs(slums_agg$diff_wp_pb)
slums_agg$adiff_wp_c <- abs(slums_agg$diff_wp_c)

#differences btw KYC density & gridded population density - squared
slums_agg$diff2_gpw <- slums_agg$diff_gpw^2
slums_agg$diff2_wp_u <- slums_agg$diff_wp_u^2
slums_agg$diff2_lsg <- slums_agg$diff_lsg^2
slums_agg$diff2_lshd <- slums_agg$diff_lshd^2
slums_agg$diff2_ghs <- slums_agg$diff_ghs^2
slums_agg$diff2_grid <- slums_agg$diff_grid^2
slums_agg$diff2_hrsl <- slums_agg$diff_hrsl^2
slums_agg$diff2_wp_pb <- slums_agg$diff_wp_pb^2
slums_agg$diff2_wp_c <- slums_agg$diff_wp_c^2

#fraction of KYC population estimated by gridded population 
slums_agg$frac_gpw <- (slums_agg$gpw_combo / slums_agg$KYC_Pop) 
slums_agg$frac_wp_u <- (slums_agg$wp_u_combo / slums_agg$KYC_Pop) 
slums_agg$frac_lsg <- (slums_agg$lsg_combo / slums_agg$KYC_Pop)
slums_agg$frac_lshd <- (slums_agg$lshd_combo / slums_agg$KYC_Pop)
slums_agg$frac_ghs <- (slums_agg$ghs_combo / slums_agg$KYC_Pop) 
slums_agg$frac_grid <- (slums_agg$grid_combo / slums_agg$KYC_Pop)
slums_agg$frac_hrsl <- (slums_agg$hrsl_combo / slums_agg$KYC_Pop) 
slums_agg$frac_wp_pb <- (slums_agg$wp_pb_combo / slums_agg$KYC_Pop) 
slums_agg$frac_wp_c <- (slums_agg$wp_c_combo / slums_agg$KYC_Pop)


# Slum density MAE by dataset
gpw <- round(sum(slums_agg$adiff_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_agg$adiff_gpw))), digits = 0) 
wp_u <- round(sum(slums_agg$adiff_wp_u, na.rm = TRUE) / NROW(which(!is.na(slums_agg$adiff_wp_u))), digits = 0) 
lsg <- round(sum(slums_agg$adiff_lsg, na.rm = TRUE) / NROW(which(!is.na(slums_agg$adiff_lsg))), digits = 0) 
lshd <- round(sum(slums_agg$adiff_lshd, na.rm = TRUE) / NROW(which(!is.na(slums_agg$adiff_lshd))), digits = 0) 
ghs <- round(sum(slums_agg$adiff_ghs, na.rm = TRUE)  / NROW(which(!is.na(slums_agg$adiff_ghs))), digits = 0) 
grid <- round(sum(slums_agg$adiff_grid, na.rm = TRUE) / NROW(which(!is.na(slums_agg$adiff_grid))), digits = 0) 
wp_pb <- round(sum(slums_agg$adiff_wp_pb, na.rm = TRUE) / NROW(which(!is.na(slums_agg$adiff_wp_pb))), digits = 0) 
wp_c <- round(sum(slums_agg$adiff_wp_c, na.rm = TRUE) / NROW(which(!is.na(slums_agg$adiff_wp_c))), digits = 0) 
hrsl <- round(sum(slums_agg$adiff_hrsl, na.rm = TRUE) / NROW(which(!is.na(slums_agg$adiff_hrsl))), digits = 0) 

mae <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(mae)[1] <- "mae"


# Slum density RMSE by dataset
gpw <- round(sqrt( sum(slums_agg$diff2_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_agg$diff2_gpw))) ), digits = 0) 
wp_u <- round(sqrt( sum(slums_agg$diff2_wp_u, na.rm = TRUE) / NROW(which(!is.na(slums_agg$diff2_wp_u))) ), digits = 0) 
lsg <- round(sqrt( sum(slums_agg$diff2_lsg, na.rm = TRUE) / NROW(which(!is.na(slums_agg$diff2_lsg))) ), digits = 0) 
lshd <- round(sqrt( sum(slums_agg$diff2_lshd, na.rm = TRUE) / NROW(which(!is.na(slums_agg$diff2_lshd))) ), digits = 0) 
ghs <- round(sqrt( sum(slums_agg$diff2_ghs, na.rm = TRUE)  / NROW(which(!is.na(slums_agg$diff2_ghs))) ), digits = 0) 
grid <- round(sqrt( sum(slums_agg$diff2_grid, na.rm = TRUE) / NROW(which(!is.na(slums_agg$diff2_grid))) ), digits = 0) 
wp_pb <- round(sqrt( sum(slums_agg$diff2_wp_pb, na.rm = TRUE) / NROW(which(!is.na(slums_agg$diff2_wp_pb))) ), digits = 0) 
wp_c <- round(sqrt( sum(slums_agg$diff2_wp_c, na.rm = TRUE) / NROW(which(!is.na(slums_agg$diff2_wp_c))) ), digits = 0) 
hrsl <- round(sqrt( sum(slums_agg$diff2_hrsl, na.rm = TRUE) / NROW(which(!is.na(slums_agg$diff2_hrsl))) ), digits = 0) 

rmse <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(rmse)[1] <- "rmse"

# Median fraction of KYC pop estimated by gridded pop
gpw <- round(median(slums_agg$frac_gpw, na.rm = TRUE), digits = 3) 
wp_u <- round(median(slums_agg$frac_wp_u, na.rm = TRUE), digits = 3) 
lsg <- round(median(slums_agg$frac_lsg, na.rm = TRUE), digits = 3) 
lshd <- round(median(slums_agg$frac_lshd, na.rm = TRUE), digits = 3) 
ghs <- round(median(slums_agg$frac_ghs, na.rm = TRUE), digits = 3)  
grid <- round(median(slums_agg$frac_grid, na.rm = TRUE), digits = 3) 
wp_pb <- round(median(slums_agg$frac_wp_pb, na.rm = TRUE), digits = 3) 
wp_c <- round(median(slums_agg$frac_wp_c, na.rm = TRUE), digits = 3) 
hrsl <- round(median(slums_agg$frac_hrsl, na.rm = TRUE), digits = 3)  

frac <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(frac)[1] <- "MF"


# Count (n) settlements with population estimate
gpw <- as.numeric(NROW(which(!is.na(slums_agg$gpw_combo))))
wp_u <- as.numeric(NROW(which(!is.na(slums_agg$wp_u_combo))))
lsg <- as.numeric(NROW(which(!is.na(slums_agg$lsg_combo))))
lshd <- as.numeric(NROW(which(!is.na(slums_agg$lshd_combo))))
ghs <- as.numeric(NROW(which(!is.na(slums_agg$ghs_combo)))) 
grid <- as.numeric(NROW(which(!is.na(slums_agg$grid_combo)))) 
wp_pb <- as.numeric(NROW(which(!is.na(slums_agg$wp_pb_combo)))) 
wp_c <- as.numeric(NROW(which(!is.na(slums_agg$wp_c_combo)))) 
hrsl <- as.numeric(NROW(which(!is.na(slums_agg$hrsl_combo))))

n <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(n)[1] <- "n"

datasets <- as.data.frame(rbind("GPWv4.11", "WorldPop-U", "LandScan-G", "LandScan-HD", "GHS-POP2", "GRID3", "WorldPop-PB", "WorldPop-C", "HRSL"))

errorstats_wide_pooled <- cbind(datasets, n, mae,  rmse, frac)
errorstats_wide_pooled$label <- paste0(errorstats_wide_pooled$V1, " (n=", errorstats_wide_pooled$n, ")")
errorstats_wide_pooled <- errorstats_wide_pooled[order(-errorstats_wide_pooled$MF),]

rm(datasets, mae, rmse, frac, n)
rm(ghs, gpw, grid, hrsl, lsg, lshd, wp_c, wp_u, wp_pb)


############## 
# City-level Overall error statistics 
##############

errorstats_long_pooled <- errorstats_wide_pooled[, c("label", "mae", "rmse", "MF")]
errorstats_long_pooled <- melt(setDT(errorstats_long_pooled), id.vars = c("label", "MF"), variable.name = "stat")



######################
##  Tables & Figures
######################


############## 
# Figure 2: Boxplot of settlement densities & areas, by city
############## 

png("fig_2_boxplot_v6.png")

fig_density <- ggplot(slums, aes(x=reorder(City, KYC_Mean_100m), y=dens_kyc)) + 
  geom_boxplot() +
  scale_y_continuous(labels = comma_format()) +
  #geom_point(data=cities, aes(x=reorder(City, KYC_Mean_100m), y=dens100m_UC), size=1.8, color="red", shape=1, stroke=1.1) +  #excluding from visual and will summarize in text
  labs(x="", y="KYC population per 100x100m (density)") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")
fig_density

fig_area <- ggplot(slums, aes(x=reorder(City, KYC_Mean_100m), y=area_100m)) + 
  geom_boxplot() +
  scale_y_continuous(labels = comma_format()) +
  labs(x="", y="KYC settlement area in 100x100m (hectares)") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y=element_blank())
fig_area


fig_distribution <- grid.arrange(fig_density, fig_area, ncol = 2, widths=c(1.4,1))

dev.off()
ggsave("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_2_boxplot_v6.png", fig_distribution, units="in", width=8, height=3.5, dpi=400)
ggsave("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_2_boxplot_v6.tiff", fig_distribution, units="in", width=8, height=3.5, dpi=400, compression = 'lzw')

rm(fig_density, fig_area, fig_distribution)

## Note that this output is further formatted and combined with a table in Publisher


############## 
# Figure 4. Graph overall error statistics 
##############

png("fig_4_overall_error_v6.png")

### Settlement-level error

fig_lolli <- ggplot(errorstats_long, aes(x=value, y=reorder(label,MF))) +
  geom_line(aes(group = label)) +
  geom_point(aes(color = stat, size=2)) +
  geom_text(aes(label=value), nudge_x=69, nudge_y= .35) +
  guides(size = "none") +
  theme_minimal() +
  theme(axis.title.y=element_blank(), legend.title=element_blank(), legend.position ="none") +
  scale_color_hue(labels = c("MAE", "RMSE")) +
  xlab("MAE and      RMSE Value") 

fig_bar <- ggplot(errorstats_wide, aes(x=reorder(label,MF), y=MF)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=MF), nudge_y=.15) +
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  theme(axis.text.y=element_blank(), axis.title.y=element_blank(), plot.margin = unit(c(.16,.5,.16,.5), "cm")) +
  coord_flip() +
  ylab("Median Fraction (MF) Est'd")

fig_overall_error <- grid.arrange(fig_lolli, fig_bar, ncol = 2, widths=c(2.5,1))

dev.off()
ggsave("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_4_overall_error_v6.tiff", fig_overall_error, units="in", width=8, height=3.5, dpi=300, compression = 'lzw')
ggsave("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_4_overall_error_v6.png", fig_overall_error, units="in", width=8, height=3.5, dpi=300)

rm(fig_bar, fig_lolli)



############## 
#Table 1.  Error statistic comparisons 
##############

### Settlement-level

#from errorstats_wide: GHS minus GPW
#805 - 840     #Diff MAE   = -35
#1583 - 1608   #Diff RMSE  = -25
#0.351 - 0.272 #Diff MF    = 0.079

#from errorstats_wide & manual below: HRSL minus GPW
#slums_nozaf <- subset(slums[which(slums$Country!="South Africa"),])
#gpw_mae <- round(sum(slums_nozaf$adiff_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_nozaf$adiff_gpw))), digits = 0) 
#gpw_rmse <- round(sqrt( sum(slums_nozaf$diff2_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_nozaf$diff2_gpw))) ), digits = 0) 
#gpw_mf <- round(median(slums_nozaf$frac_gpw, na.rm = TRUE), digits = 3) 
#771 - 807     #Diff MAE   = -36
#1541 - 1582   #Diff RMSE  = -41
#0.436 - 0.289 #Diff MF    = 0.147


#from errorstats_wide: WPG-U minus GPW
#832 - 840     #Diff MAE   = -8
#1602 - 1608   #Diff RMSE  = -6
#0.31 - 0.272 #Diff MF    = 0.038

#from errorstats_wide: WPG-C minus GPW
#795 - 840     #Diff MAE   = -45
#1552 - 1608   #Diff RMSE  = -56
#0.411 - 0.272 #Diff MF    = 0.139



#### City-level


#from errorstats_wide_pooled: GHS minus GPW
405 - 470     #Diff MAE   = -65
539 - 625   #Diff RMSE  = -86
0.442 - 0.308 #Diff MF    = 0.134

#from errorstats_wide & manual below: HRSL minus GPW
slums_agg_nozaf <- subset(slums_agg[which(slums_agg$City!="Cape Town"),])
gpw_mae <- round(sum(slums_agg_nozaf$adiff_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_agg_nozaf$adiff_gpw))), digits = 0) 
gpw_rmse <- round(sqrt( sum(slums_agg_nozaf$diff2_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_agg_nozaf$diff2_gpw))) ), digits = 0) 
gpw_mf <- round(median(slums_agg_nozaf$frac_gpw, na.rm = TRUE), digits = 3) 
383 - 476     #Diff MAE   = -93
522 - 644   #Diff RMSE  = -122
0.485 - 0.318 #Diff MF    = 0.167


#from errorstats_wide: WPG-U minus GPW
462 - 470     #Diff MAE   = -8
607 - 625   #Diff RMSE  = -18
0.324 - 0.308 #Diff MF    = 0.016

#from errorstats_wide: WPG-C minus GPW
416 - 470     #Diff MAE   = -54
570 - 625   #Diff RMSE  = -55
0.464 - 0.308 #Diff MF    = 0.156





rm(gpw_mae, gpw_rmse, gpw_mf, slums_agg_nozaf)







############################
## Calculate city-level error statistics
############################  



CITY <- c("Accra", "Cape Town", "Dar es Salaam", "Davao", "Kampala", "Kisumu", "Lagos", "Malabon", "Nairobi", "Port Harcourt")

for(i in CITY) {

##############  CITY error statistics  ##########
# select all records for a given city
slums_city <- slums
slums_city <- slums_city[slums_city$City == i,]

# KYC density mean
gpw <- round(mean(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
wp_u <- round(mean(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
lsg <- round(mean(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
lshd <- round(mean(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
ghs <- round(mean(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
grid <- round(mean(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
wp_pb <- round(mean(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
wp_c <- round(mean(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
hrsl <- round(mean(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 

mean <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(mean)[1] <- "mean"

# KYC density Std Dev
gpw <- round(sd(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
wp_u <- round(sd(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
lsg <- round(sd(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
lshd <- round(sd(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
ghs <- round(sd(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
grid <- round(sd(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
wp_pb <- round(sd(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
wp_c <- round(sd(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 
hrsl <- round(sd(slums_city$dens_kyc, na.rm = TRUE), digits = 0) 

sd <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(sd)[1] <- "sd"

# Slum pop MAE by dataset
gpw <- round(sum(slums_city$adiff_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_city$adiff_gpw))), digits = 0) 
wp_u <- round(sum(slums_city$adiff_wp_u, na.rm = TRUE) / NROW(which(!is.na(slums_city$adiff_wp_u))), digits = 0) 
lsg <- round(sum(slums_city$adiff_lsg, na.rm = TRUE) / NROW(which(!is.na(slums_city$adiff_lsg))), digits = 0) 
lshd <- round(sum(slums_city$adiff_lshd, na.rm = TRUE) / NROW(which(!is.na(slums_city$adiff_lshd))), digits = 0) 
ghs <- round(sum(slums_city$adiff_ghs, na.rm = TRUE)  / NROW(which(!is.na(slums_city$adiff_ghs))), digits = 0) 
grid <- round(sum(slums_city$adiff_grid, na.rm = TRUE) / NROW(which(!is.na(slums_city$adiff_grid))), digits = 0) 
wp_pb <- round(sum(slums_city$adiff_wp_pb, na.rm = TRUE) / NROW(which(!is.na(slums_city$adiff_wp_pb))), digits = 0) 
wp_c <- round(sum(slums_city$adiff_wp_c, na.rm = TRUE) / NROW(which(!is.na(slums_city$adiff_wp_c))), digits = 0) 
hrsl <- round(sum(slums_city$adiff_hrsl, na.rm = TRUE) / NROW(which(!is.na(slums_city$adiff_hrsl))), digits = 0) 

mae <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(mae)[1] <- "mae"

# Slum pop RMSE by dataset
gpw <- round(sqrt( sum(slums_city$diff2_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_city$diff2_gpw))) ), digits = 0) 
wp_u <- round(sqrt( sum(slums_city$diff2_wp_u, na.rm = TRUE) / NROW(which(!is.na(slums_city$diff2_wp_u))) ), digits = 0) 
lsg <- round(sqrt( sum(slums_city$diff2_lsg, na.rm = TRUE) / NROW(which(!is.na(slums_city$diff2_lsg))) ), digits = 0) 
lshd <- round(sqrt( sum(slums_city$diff2_lshd, na.rm = TRUE) / NROW(which(!is.na(slums_city$diff2_lshd))) ), digits = 0) 
ghs <- round(sqrt( sum(slums_city$diff2_ghs, na.rm = TRUE)  / NROW(which(!is.na(slums_city$diff2_ghs))) ), digits = 0) 
grid <- round(sqrt( sum(slums_city$diff2_grid, na.rm = TRUE) / NROW(which(!is.na(slums_city$diff2_grid))) ), digits = 0) 
wp_pb <- round(sqrt( sum(slums_city$diff2_wp_pb, na.rm = TRUE) / NROW(which(!is.na(slums_city$diff2_wp_pb))) ), digits = 0) 
wp_c <- round(sqrt( sum(slums_city$diff2_wp_c, na.rm = TRUE) / NROW(which(!is.na(slums_city$diff2_wp_c))) ), digits = 0) 
hrsl <- round(sqrt( sum(slums_city$diff2_hrsl, na.rm = TRUE) / NROW(which(!is.na(slums_city$diff2_hrsl))) ), digits = 0) 

rmse <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(rmse)[1] <- "rmse"

# Median fraction of KYC pop estimated by gridded pop
gpw <- round(median(slums_city$frac_gpw, na.rm = TRUE), digits = 3) 
wp_u <- round(median(slums_city$frac_wp_u, na.rm = TRUE), digits = 3) 
lsg <- round(median(slums_city$frac_lsg, na.rm = TRUE), digits = 3) 
lshd <- round(median(slums_city$frac_lshd, na.rm = TRUE), digits = 3) 
ghs <- round(median(slums_city$frac_ghs, na.rm = TRUE), digits = 3)  
grid <- round(median(slums_city$frac_grid, na.rm = TRUE), digits = 3) 
wp_pb <- round(median(slums_city$frac_wp_pb, na.rm = TRUE), digits = 3) 
wp_c <- round(median(slums_city$frac_wp_c, na.rm = TRUE), digits = 3) 
hrsl <- round(median(slums_city$frac_hrsl, na.rm = TRUE), digits = 3)  

frac <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(frac)[1] <- "MF"

# Count (n) settlements with population estimate
gpw <- as.numeric(NROW(which(!is.na(slums_city$gpw_combo))))
wp_u <- as.numeric(NROW(which(!is.na(slums_city$wp_u_combo))))
lsg <- as.numeric(NROW(which(!is.na(slums_city$lsg_combo))))
lshd <- as.numeric(NROW(which(!is.na(slums_city$lshd_combo))))
ghs <- as.numeric(NROW(which(!is.na(slums_city$ghs_combo)))) 
grid <- as.numeric(NROW(which(!is.na(slums_city$grid_combo)))) 
wp_pb <- as.numeric(NROW(which(!is.na(slums_city$wp_pb_combo)))) 
wp_c <- as.numeric(NROW(which(!is.na(slums_city$wp_c_combo)))) 
hrsl <- as.numeric(NROW(which(!is.na(slums_city$hrsl_combo))))

n <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(n)[1] <- "n"

# Average area (km2) of KYC settlements
gpw <- round(mean(slums_city$area_km2, na.rm = TRUE), digits = 4) 
wp_u <- round(mean(slums_city$area_km2, na.rm = TRUE), digits = 4) 
lsg <- round(mean(slums_city$area_km2, na.rm = TRUE), digits = 4) 
lshd <- round(mean(slums_city$area_km2, na.rm = TRUE), digits = 4) 
ghs <- round(mean(slums_city$area_km2, na.rm = TRUE), digits = 4) 
grid <- round(mean(slums_city$area_km2, na.rm = TRUE), digits = 4) 
wp_pb <- round(mean(slums_city$area_km2, na.rm = TRUE), digits = 4) 
wp_c <- round(mean(slums_city$area_km2, na.rm = TRUE), digits = 4) 
hrsl <- round(mean(slums_city$area_km2, na.rm = TRUE), digits = 4) 

area_km2 <- as.data.frame(rbind(gpw, wp_u, lsg, lshd, ghs, grid, wp_pb, wp_c, hrsl))
names(area_km2)[1] <- "area_km2"

datasets <- as.data.frame(rbind("GPWv4.11", "WorldPop-U", "LandScan-G", "LandScan HD", "GHS-POP2", "GRID3", "WorldPop-PB", "WorldPop-C", "HRSL"))

errorstats_wide_CITY <- cbind(datasets, mean, sd, n, mae, rmse, frac, area_km2)
errorstats_wide_CITY <- errorstats_wide_CITY[errorstats_wide_CITY$n>0,]  #######  Drop NA values
errorstats_wide_CITY$label <- paste0(errorstats_wide_CITY$V1, " (n=", errorstats_wide_CITY$n, ")")
errorstats_wide_CITY <- errorstats_wide_CITY[order(-errorstats_wide_CITY$MF),]
assign(paste('errorstats_wide_',i,sep=''), errorstats_wide_CITY)

errorstats_long_CITY <- errorstats_wide_CITY[, c("label", "mean", "sd", "n", "mae", "rmse", "MF", "area_km2")]
errorstats_long_CITY <- melt(setDT(errorstats_long_CITY), id.vars = c("label","MF"), variable.name = "stat")
errorstats_long_CITY <- errorstats_long_CITY[errorstats_long_CITY$value>0,]
assign(paste('errorstats_long_',i,sep=''), errorstats_long_CITY)


}

rm(datasets, frac, mae, mean, n, rmse, sd, area_km2)


errorstats_wide_Accra$City <- "Accra"
`errorstats_wide_Cape Town`$City <- "Cape Town"
`errorstats_wide_Dar es Salaam`$City <- "Dar es Salaam"
errorstats_wide_Davao$City <- "Davao"
errorstats_wide_Kampala$City <- "Kampala"
errorstats_wide_Kisumu$City <- "Kisumu"
errorstats_wide_Lagos$City <- "Lagos"
errorstats_wide_Malabon$City <- "Malabon"
errorstats_wide_Nairobi$City <- "Nairobi"
`errorstats_wide_Port Harcourt`$City <- "Port Harcourt"

errorstats_wide_CITY <- rbind(errorstats_wide_Accra, `errorstats_wide_Cape Town`, `errorstats_wide_Dar es Salaam`, errorstats_wide_Davao, errorstats_wide_Kampala, errorstats_wide_Kisumu, errorstats_wide_Lagos, errorstats_wide_Malabon, errorstats_wide_Nairobi, `errorstats_wide_Port Harcourt`)

errorstats_wide_CITY <- errorstats_wide_CITY %>%
      mutate(dat = case_when(V1 == 'GPWv4.11' ~ 'gpw',
                            V1 == 'GHS-POP2' ~ 'ghs',
                             V1 == 'HRSL' ~ 'hrsl',
                             V1 == 'LandScan-G' ~ 'lsg',
                             V1 == 'LandScan HD' ~ 'lshd',
                             V1 == 'WorldPop-C' ~ 'wpg_c',
                             V1 == 'WorldPop-U' ~ 'wpg_u',
                             V1 == 'WorldPop-PB' ~ 'wp_pb',
                             V1 == 'GRID3' ~ 'grid'))
 


############################
## Calculate pooled city-level error statistics
############################  


mf_wide_pooled <- slums_agg[, c("City", "frac_gpw", "frac_ghs", "frac_hrsl", "frac_wp_u", "frac_wp_c", "frac_wp_pb", "frac_lsg", "frac_lshd", "frac_grid")]
mf_wide_pooled <- melt(setDT(mf_wide_pooled), id.vars = c("City"), variable.name = "dataset")

mf_wide_pooled <- mf_wide_pooled[mf_wide_pooled$value>0,]

mf_wide_pooled$dataset <- as.character(mf_wide_pooled$dataset)
mf_wide_pooled$dataset[mf_wide_pooled$dataset=="frac_gpw"] <- "GPWv4.11"
mf_wide_pooled$dataset[mf_wide_pooled$dataset=="frac_ghs"] <- "GHS-POP2"
mf_wide_pooled$dataset[mf_wide_pooled$dataset=="frac_hrsl"] <- "HRSL"
mf_wide_pooled$dataset[mf_wide_pooled$dataset=="frac_wp_u"] <- "WorldPop-U"
mf_wide_pooled$dataset[mf_wide_pooled$dataset=="frac_wp_c"] <- "WorldPop-C"
mf_wide_pooled$dataset[mf_wide_pooled$dataset=="frac_wp_pb"] <- "WorldPop-PB"
mf_wide_pooled$dataset[mf_wide_pooled$dataset=="frac_lsg"] <- "LandScan-G"
mf_wide_pooled$dataset[mf_wide_pooled$dataset=="frac_lshd"] <- "LandScan HD"
mf_wide_pooled$dataset[mf_wide_pooled$dataset=="frac_grid"] <- "GRID3"



############################
## Figure 5. MF of settlements accurately estimated, by CITY
############################


png("fig_5_citymfplot_v4.png")

fig_mfplot_pool <- ggplot(mf_wide_pooled, aes(x=reorder(dataset, value), y=value)) + 
  geom_point(aes(shape=City, color=City, size=City)) +
  scale_shape_manual(values = c(0, 1, 2, 4, 7, 10, 15, 16, 17, 18)) +
  scale_color_manual(values = c("#f78104", "#c1272d", "#0000a7", "#000000", "#008176", "#b4b4b4", "#c1272d", "#0000a7", "#faab36", "#008176")) +
  scale_size_manual(values = c(2.3,3,2.3,2.5,2.1,3,2.5,3,2.5,3)) +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0,1.04, by=.2),limits = c(0,1.04)) +
  ylab("Median Fraction (MF) of KYC population estimated") +
  theme(axis.title.y=element_blank()) +
  #ggtitle("All Pooled Settlements, by City") +
  coord_flip()
fig_mfplot_pool


dev.off()
ggsave("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_5_citymfplot_v4.tiff", fig_mfplot_pool, units="in", width=8, height=3.5, dpi=300, compression = 'lzw')
ggsave("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_5_citymfplot_v4.png", fig_mfplot_pool, units="in", width=8, height=3.5, dpi=300)



##########################
## Investigate potential sources of gridded population dataset error, by city
##########################

######
#Outline of analysis & create dataset - start with errorstats_wide_CITY
######

  #OBSERVATIONS

    #71 unique city <--> gridded pop combinations

  #OUTCOMES
    #err_mf:   median fraction of population accurately estimated for all pooled settlements in a city, by gridded pop
    #err_mae:   mean absolute error of gridded population estimates in city X
    #err_rmse:  root mean squared error of gridded population estimates in city X

  #PREDICTORS

    #CHARACTERISTICS OF CITY AND VALIDATION DATA
    #kyc_dens_m:  mean KYC slum population density per km2 in city X
    #kyc_dens_sd: std dev of KYC slum population density per km2 in city X
    #kyc_areakm2: average area KYC slums reported in city X
    #kyc_n:       n number of KYC slums reported in city X
    #cit_pop: GHS Functional Urban Area total population estimated in 2020
    #cit_dens:GHS Functional Urban Area population density estimated in 2020
    #cit_area:GHS Functional Urban Area in km2 [not a predictor; only used to calculate cen_u_area]

    #CHARACTERISTICS OF GRIDDED POPULATION DATA INPUTS/APPROACH/OUTPUTS
    #mod_top:   Modelling approach used is: 1=top-down, 0=bottom-up
    #mod_bldg:  Modelling approach incorporates building footprints: 1=yes, 0=no
    #mod_apprch:Modelling approach is: 0=un-modeled, 1=lightly-modeled, 2=highly-modeled
    #mod_constr:Modelling approach constrains to settlement: 1=yes, 0=no
    #mod_res:   Modelled output resolution: 1000=1x1km, 100=100x100m, 30=30x30m
    #cen_age:   Age (in years) of last census in 2020 (top-down models only)   
    #cen_u_cnt: Count of census unit centroids within FUA boundary of city X (top-down models only)   
    #cen_u_area:Average area (km2) of census units in city X (top-down models only) [calculation: cit_area / cen_u_cnt]  
 
    #cen_age_mod_top: Age of last census in 2020: 9=bottom-up, 0=top-down <10yrs, 1=top-down 10+ year    
    #cen_u_cnt_mod_top: Count of census unit centroids in FUA: 9=bottom-up, 0=top-down 2-17 units, 1=top-down 85-161 units, 2=top-down 3672-5049 units  
    #cen_u_area_mod_top: Average area (km2) of census units: 9=bottom-up, 0=topdown <15km2, 1=topdown 67-126km2, 2=topdown  214-582km2


city_pred <- read_excel("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/cities_data_v3.xlsx", sheet = "cities - accuracy")
data_pred <- read_excel("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/cities_data_v3.xlsx", sheet = "data - accuracy")

names(errorstats_wide_CITY) <- c("GridPop", "kyc_dens_m", "kyc_dens_sd", "kyc_n", "err_mae", "err_rmse", "err_mf_sett", "kyc_areakm2", "label", "City", "dat")

names(mf_wide_pooled) <- c("City", "GridPop", "err_mf")

error_cause_data <- merge(errorstats_wide_CITY, mf_wide_pooled, by=c("City", "GridPop"))
error_cause_data <- merge(error_cause_data, city_pred, by="City")
error_cause_data <- merge(error_cause_data, data_pred, by="GridPop")

rm(city_pred, data_pred)

write.csv(error_cause_data, file="C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/error_cause_data.csv")   # to double check model results in Stata


#################
# Fig 6. Correlation btw MF/MAE/RMSE and each predictor
#################


##### Transform for approx normal distributions

norm <- error_cause_data

# MAE
hist(norm$err_mae)
norm$log_mae <- log(norm$err_mae) #transform
hist(norm$log_mae)


# RMSE
hist(norm$err_rmse)
norm$log_rmse <- log(norm$err_rmse) #transform
hist(norm$log_rmse)

# MF
hist(norm$err_mf)
norm$sqrt_mf <- sqrt(norm$err_mf) #transform
hist(norm$sqrt_mf)

# mod_res
hist(norm$mod_res)
norm$log_mod_res <- log(norm$mod_res) #transformed because wildly skewed
hist(norm$log_mod_res)

# cen_u_cnt
hist(norm$cen_u_cnt)
norm$log_cen_u_cnt <- log(norm$cen_u_cnt) #transformed because wildly skewed
hist(norm$log_cen_u_cnt)


# Function to add p-values to correlation values, by www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of correlation values
corr_1 <- cor(norm[,c("log_mae", "log_rmse", "sqrt_mf", "kyc_dens_m", "kyc_dens_sd", "kyc_n", "kyc_areakm2", "cit_pop", "cit_dens", "mod_top", "mod_bldg", "mod_apprch", "mod_constr", "log_mod_res", "cen_age", "log_cen_u_cnt", "cen_u_area")])
head(corr_1[, 1:5])

# matrix of the p-value of the correlation
p.mat <- cor.mtest(norm[,c("log_mae", "log_rmse", "sqrt_mf", "kyc_dens_m", "kyc_dens_sd", "kyc_n", "kyc_areakm2", "cit_pop", "cit_dens", "mod_top", "mod_bldg", "mod_apprch", "mod_constr", "log_mod_res", "cen_age", "log_cen_u_cnt", "cen_u_area")])
head(p.mat[, 1:5])

#tiff("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_6_cause_corr3.tiff", units="in", width=8.5, height=8.8, res=300, pointsize = 16, compression = 'lzw')
png("C:/Users/danar/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_6_cause_corr3.png", units="in", width=8.5, height=8.8, res=300, pointsize = 16)
corrplot(corr_1, type="upper", p.mat = p.mat, sig.level = 0.1, insig = "blank", tl.col = "black", tl.srt = 75, col=colorRampPalette(c("#00BFC4", "#FFAF00", "#D25E68"))((200)), diag = FALSE)
dev.off()


#################
# Fig 6. Models of MF/MAE/RMSE and correlated predictors
#################

# check for collinearity (>0.8) among kyc_dens_m<->kyc_dens_sd, cit_pop<->cen_u_area
cor(norm[,c("log_mae", "log_rmse", "sqrt_mf", "kyc_dens_m", "kyc_dens_sd", "cit_pop",   "cen_u_area")])
# keep kyc_dens_m because stronger coorelation with all 3 metrics
# keep cen_u_area because strongly correlated with all 3 metrics


# MAE
mae_m1 <- lm(log_mae ~ kyc_dens_m + kyc_n + kyc_areakm2  + cit_dens + mod_top + mod_bldg + mod_apprch + mod_constr + log_mod_res + cen_age + log_cen_u_cnt , data=norm)
mae_full <- summary(mae_m1)
mae_full$coefficients <- round(mae_full$coefficients,4)
mae_full$residuals <- round(mae_full$residuals,4)
mae_full
plot(mae_full$residuals)

calc.relimp(mae_m1, type = c("lmg"), rela=TRUE)


mae_m2 <- lm(log_mae ~ mod_top + mod_bldg + mod_apprch + mod_constr + log_mod_res , data=norm)

mae_red <- summary(mae_m2)
mae_red$coefficients <- round(mae_red$coefficients,4)
mae_red$residuals <- round(mae_red$residuals,4)
plot(mae_red$residuals)

calc.relimp(mae_m2, type = c("lmg"), rela=TRUE)


# RMSE
rmse_m1 <- lm(log_rmse ~ kyc_dens_m + kyc_n + kyc_areakm2  + cit_dens + mod_top + mod_bldg + mod_apprch + mod_constr + log_mod_res + cen_age + log_cen_u_cnt , data=norm)
rmse_full <- summary(rmse_m1)
rmse_full$coefficients <- round(rmse_full$coefficients,4)
rmse_full$residuals <- round(rmse_full$residuals,4)
rmse_full
plot(rmse_full$residuals)

calc.relimp(rmse_m1, type = c("lmg"), rela=TRUE)


rmse_m2 <- lm(log_rmse ~ mod_top + mod_bldg + mod_apprch + mod_constr + log_mod_res , data=norm)
rmse_red <- summary(rmse_m2)
rmse_red$coefficients <- round(rmse_red$coefficients,4)
rmse_red$residuals <- round(rmse_red$residuals,4)
plot(rmse_red$residuals)

calc.relimp(rmse_m2, type = c("lmg"), rela=TRUE)




# MF
mf_m1 <- lm(sqrt_mf ~ kyc_dens_m + kyc_n + kyc_areakm2  + cit_dens + mod_top + mod_bldg + mod_apprch + mod_constr + log_mod_res + cen_age + log_cen_u_cnt , data=norm)
mf_full <- summary(mf_m1)
mf_full$coefficients <- round(mf_full$coefficients,4)
mf_full$residuals <- round(mf_full$residuals,4)
plot(mf_full$residuals)

calc.relimp(mf_m1, type = c("lmg"), rela=TRUE)


mf_m2 <- lm(sqrt_mf ~ mod_top + mod_bldg + mod_apprch + mod_constr + log_mod_res , data=norm)
mf_red <- summary(mf_m2)
mf_red$coefficients <- round(mf_red$coefficients,4)
mf_red$residuals <- round(mf_red$residuals,4)
plot(mf_red$residuals)

calc.relimp(mf_m2, type = c("lmg"), rela=TRUE)

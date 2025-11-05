##############################################################
### Comparison of estimates populations in 12 cities       ###
### Step 2: KYC accuracy assessment                        ###
### By: Dana R Thomson (dana.r.thomson@gmail.com) and      ###
###     Hazem Mahmoud (Hazem.mahmoud@my.utsa.edu)          ###
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

setwd("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/data")   

#packages (versions)
# R version 4.3.1 
library("dplyr")        #v1.1.2 
library("tidyr")
library("data.table")   #v1.14.8 
library("ggplot2")      #v3.4.2  
library("readxl")       #v1.4.3  
library("scales")       #v1.2.1
library("gridExtra")    #v2.3
library("corrplot")     #v0.92
library("relaimpo")     #v2.2-6 

select <- dplyr::select
mutate <- dplyr::mutate
rename <- dplyr::rename
filter <- dplyr::filter
summarise <- dplyr::summarise



#Read analysis dataset & merge city characteristics

cities <- readxl::read_excel(
  "C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/cities_data_v4.xlsx",
  sheet = "cities - analysis 1"
) %>%
  dplyr::select(City, GHS_FUA_density, GHS_UC_density, class_pop, class_density) %>%
  dplyr::mutate(
    dens100m_FUA = GHS_FUA_density / 100,
    dens100m_UC  = GHS_UC_density  / 100
  )

slums <- read.csv("kyc_campaign/COMBINED/slums_v13.csv") %>%
  left_join(cities, by = "City")

# dataset name mapping 
ds_map <- tibble::tribble(
  ~ds,     ~label,
  "gpw",   "GPWv4.11",
  "lsg",   "LandScan-G",
  "lshd",  "LandScan-HD",
  "lsm",  "LandScan-M",
  "ghs",   "GHS-POP2",
  "grid",  "GRID3-NGAv2.1",
  "wp_pb", "WorldPop-PB",
  "wp_c",  "WorldPop-C",
  "wp_g2", "WorldPop-G2",
  "hrsl",  "HRSLv1.5"
)


######################
##  Calculations 
######################

#######
# Settlement-level error statistics (per hectare - 100x100m)
#######


## ---- sanity check: required columns exist
req_cols <- c(
  "Id","City","Country","KYC_Pop","dens_kyc",
  "gpw_combo","lsg_combo","lshd_combo","lsm_combo",
  "ghs_combo","grid_combo","hrsl_combo",
  "wp_pb_combo","wp_c_combo","wp_g2_combo",
  grep("^dens_", names(slums), value = TRUE)
)
miss <- setdiff(req_cols, names(slums))
if (length(miss)) stop("Missing in slums_v13: ", paste(miss, collapse=", "))




# dens_* to long (exclude dens_kyc), attach KYC refs safely
dens_long <- slums %>%
  tidyr::pivot_longer(
    # pivot all dens_* except dens_kyc
    cols = c(dplyr::starts_with("dens_"), -dens_kyc),
    names_to  = "dens_name",
    values_to = "dens_val"
  ) %>%
  dplyr::mutate(
    ds            = sub("^dens_", "", dens_name),
    dens_kyc_ref  = dens_kyc,
    KYC_Pop_ref   = KYC_Pop
  ) %>%
  dplyr::select(Id, City, Country, ds, dens_val, dens_kyc_ref, KYC_Pop_ref)


# *_combo to long
combo_long <- slums %>%
  pivot_longer(
    cols = ends_with("_combo"),
    names_to  = "combo_name",
    values_to = "pop_est"
  ) %>%
  mutate(ds = sub("_combo$", "", combo_name)) %>%
  select(Id, ds, pop_est)

# join + compute
sett_long <- dens_long %>%
  left_join(combo_long, by = c("Id", "ds")) %>%
  mutate(
    diff  = dens_val - dens_kyc_ref,
    adiff = abs(diff),
    diff2 = diff^2,
    frac  = if_else(is.finite(KYC_Pop_ref) & KYC_Pop_ref > 0, pop_est / KYC_Pop_ref, NA_real_)
  ) %>%
  left_join(ds_map, by = "ds")


# settlement-level overall error table (Fig. 4/table input)
errorstats_wide <- sett_long %>%
  group_by(label) %>%
  summarise(
    n    = sum(!is.na(dens_val)),
    mae  = round(mean(adiff, na.rm = TRUE), 0),
    rmse = round(sqrt(mean(diff2, na.rm = TRUE)), 0),
    MF   = round(median(frac, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(MF)) %>%
  mutate(label = paste0(label, " (n=", n, ")"))

errorstats_long <- errorstats_wide %>%
  select(label, mae, rmse, MF) %>%
  pivot_longer(cols = c(mae, rmse), names_to = "stat", values_to = "value")


#####
#summary to accompany fig2 box plot
#####

## 100m calcs
slums_summary <- slums %>%
  group_by(City) %>%
  summarise(
    KYC_N         = n(),
    KYC_Mean_100m = round(mean(dens_kyc, na.rm = TRUE), 0),
    KYC_SD_100m   = round(sd(dens_kyc,   na.rm = TRUE), 0),
    .groups = "drop"
  )
slums  <- left_join(slums,  slums_summary, by = "City")
cities <- left_join(cities, slums_summary, by = "City")



############## 
# Settlement-level Overall error statistics 
##############

write.csv(errorstats_wide, file="C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/t1_error_stats_v6.csv")   # for data quality check of reported vs digitised area

errorstats_long <- errorstats_wide[, c("label", "mae", "rmse", "MF")]
errorstats_long <- melt(setDT(errorstats_long), id.vars = c("label", "MF"), variable.name = "stat")




#######
# Pooled City-level error statistics (per hectare - 100x100m)
#######

slums_agg <- slums %>%
  group_by(City) %>%
  summarise(
    across(c(KYC_Pop, ends_with("_combo"), area_m2, area_100m, area_km2), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    dens_kyc   = KYC_Pop   / area_100m,
    dens_gpw   = gpw_combo / area_100m,
    dens_ghs   = ghs_combo / area_100m,
    dens_hrsl  = hrsl_combo / area_100m,
    dens_wp_c  = wp_c_combo / area_100m,
    dens_wp_g2 = wp_g2_combo / area_100m,
    dens_wp_pb = wp_pb_combo / area_100m,
    dens_lsg   = lsg_combo / area_100m,
    dens_lshd  = lshd_combo / area_100m,
    dens_lsm  = lsm_combo / area_100m,
    dens_grid  = grid_combo / area_100m,
    # pooled fractions (numerator = city pop_est, denom = city KYC)
    frac_gpw   = if_else(KYC_Pop > 0, gpw_combo   / KYC_Pop, NA_real_),
    frac_ghs   = if_else(KYC_Pop > 0, ghs_combo   / KYC_Pop, NA_real_),
    frac_hrsl  = if_else(KYC_Pop > 0, hrsl_combo  / KYC_Pop, NA_real_),
    frac_wp_c  = if_else(KYC_Pop > 0, wp_c_combo  / KYC_Pop, NA_real_),
    frac_wp_g2 = if_else(KYC_Pop > 0, wp_g2_combo / KYC_Pop, NA_real_),
    frac_wp_pb = if_else(KYC_Pop > 0, wp_pb_combo / KYC_Pop, NA_real_),
    frac_lsg   = if_else(KYC_Pop > 0, lsg_combo   / KYC_Pop, NA_real_),
    frac_lshd  = if_else(KYC_Pop > 0, lshd_combo  / KYC_Pop, NA_real_),
    frac_lsm  = if_else(KYC_Pop > 0, lsm_combo  / KYC_Pop, NA_real_),
    frac_grid  = if_else(KYC_Pop > 0, grid_combo  / KYC_Pop, NA_real_)
  )

# IMPORTANT: Exclude HRSL for South Africa (Cape Town) like v7
slums_agg$frac_hrsl[slums_agg$City == "Cape Town"] <- NA_real_

# --- Fig. 5 input (pooled fractions per City×dataset; includes WP-G2) -------
mf_wide_pooled <- slums_agg %>%
  select(
    City,
    frac_gpw, frac_ghs, frac_hrsl,  frac_wp_c, frac_wp_g2,
    frac_wp_pb, frac_lsg, frac_lshd, frac_lsm, frac_grid
  ) %>%
  melt(id.vars = "City", variable.name = "dataset", value.name = "value") %>%
  filter(!is.na(value) & value > 0) %>%      # preserve NA pattern; drop non-positive
  mutate(dataset = dplyr::case_match(
    dataset,
    "frac_gpw"   ~ "GPWv4.11",
    "frac_ghs"   ~ "GHS-POP2",
    "frac_hrsl"  ~ "HRSLv1.5",
    "frac_wp_c"  ~ "WorldPop-C",
    "frac_wp_g2" ~ "WorldPop-G2",  # add WorldPop-Global2
    "frac_wp_pb" ~ "WorldPop-PB",
    "frac_lsg"   ~ "LandScan-G",
    "frac_lshd"  ~ "LandScan-HD",
    "frac_lsm"  ~ "LandScan-M",     # add LandScan-Mosaic
    "frac_grid"  ~ "GRID3-NGAv2.1",
    .default = NA_character_
  )) %>%
  filter(!is.na(dataset))

# ocontrol city/dataset order for consistent shapes/colors
city_levels <- c("Accra","Cape Town","Dar es Salaam","Davao","Kampala",
                 "Kisumu","Lagos","Malabon","Nairobi","Port Harcourt")
mf_wide_pooled <- mf_wide_pooled %>%
  mutate(
    City    = factor(City, levels = city_levels),
    dataset = factor(dataset, levels = ds_map$label)
  )



######################
##  Tables & Figures
######################


############## 
# Figure 2: Boxplot of settlement densities & areas, by city
############## 

## Fig. 2: boxplots of settlement densities & areas
png("fig_2_boxplot_v8.png")

fig_density <- ggplot(slums, aes(x = reorder(City, KYC_Mean_100m), y = dens_kyc)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma_format()) +
  labs(x = "", y = "KYC population per 100×100m (density)") +
  coord_flip() +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

fig_area <- ggplot(slums, aes(x = reorder(City, KYC_Mean_100m), y = area_100m)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma_format()) +
  labs(x = "", y = "KYC settlement area (100×100m cells)") +
  coord_flip() +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", axis.text.y = element_blank())

fig_distribution <- grid.arrange(fig_density, fig_area, ncol = 2, widths = c(1.4, 1))

dev.off()
ggsave("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_2_boxplot_v8.tiff",
       fig_distribution, units = "in", width = 8, height = 3.5, dpi = 400, compression = "lzw")
ggsave("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_2_boxplot_v8.png",
       fig_distribution, units = "in", width = 8, height = 3.5, dpi = 400)

## Note that this output is further formatted and combined with a table in Publisher


############## 
# Figure 4. Graph overall error statistics 
##############

png("fig_4_overall_error_v8.png")

fig_lolli <- ggplot(errorstats_long, aes(x = value, y = reorder(label, MF))) +
  geom_line(aes(group = label)) +
  geom_point(aes(color = stat), size = 2) +
  geom_text(aes(label = value), nudge_x = 69, nudge_y = .35) +
  guides(size = "none") +
  theme_minimal(base_size = 11) +
  theme(axis.title.y = element_blank(), legend.title = element_blank(), legend.position = "none") +
  scale_color_hue(labels = c("MAE", "RMSE")) +
  xlab("MAE and      RMSE Value")

fig_bar <- ggplot(errorstats_wide, aes(x = reorder(label, MF), y = MF)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = MF), nudge_y = .15) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(.16, .5, .16, .5), "cm")) +
  coord_flip() +
  ylab("Median Fraction (MF) Est'd")

fig_overall_error <- grid.arrange(fig_lolli, fig_bar, ncol = 2, widths = c(2.5, 1))
dev.off()
ggsave("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_4_overall_error_v8.tiff",
       fig_overall_error, units = "in", width = 8, height = 3.5, dpi = 300, compression = "lzw")
ggsave("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_4_overall_error_v8.png",
       fig_overall_error, units = "in", width = 8, height = 3.5, dpi = 300)

rm(fig_bar, fig_lolli)



############## 
#Table 1.  Error statistic comparisons 
##############

### Settlement-level


#from errorstats_wide & manual below: HRSL minus GPW
#slums_nozaf <- subset(slums[which(slums$Country!="South Africa"),])
#gpw_mae <- round(sum(slums_nozaf$adiff_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_nozaf$adiff_gpw))), digits = 0) 
#gpw_rmse <- round(sqrt( sum(slums_nozaf$diff2_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_nozaf$diff2_gpw))) ), digits = 0) 
#gpw_mf <- round(median(slums_nozaf$frac_gpw, na.rm = TRUE), digits = 3) 
#770 - 807     #Diff MAE   = -37
#1548 - 1582   #Diff RMSE  = -34
#0.487 - 0.289 #Diff MF    = 0.198

#from errorstats_wide: GHS minus GPW
#805 - 840     #Diff MAE   = -35
#1583 - 1608   #Diff RMSE  = -25
#0.351 - 0.272 #Diff MF    = 0.079

#from errorstats_wide: WPG-C minus GPW
#795 - 840     #Diff MAE   = -45
#1552 - 1608   #Diff RMSE  = -56
#0.411 - 0.272 #Diff MF    = 0.139





#### City-level


#from errorstats_wide_pooled: GHS minus GPW
404 - 467     #Diff MAE   = -63
538 - 619   #Diff RMSE  = -81
0.442 - 0.308 #Diff MF    = 0.134

#from errorstats_wide_pooled & manual below: HRSL minus GPW
slums_agg_nozaf <- subset(slums_agg[which(slums_agg$City!="Cape Town"),])
gpw_mae <- round(sum(slums_agg_nozaf$adiff_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_agg_nozaf$adiff_gpw))), digits = 0) 
gpw_rmse <- round(sqrt( sum(slums_agg_nozaf$diff2_gpw, na.rm = TRUE) / NROW(which(!is.na(slums_agg_nozaf$diff2_gpw))) ), digits = 0) 
gpw_mf <- round(median(slums_agg_nozaf$frac_gpw, na.rm = TRUE), digits = 3) 
378 - 473     #Diff MAE   = -95
522 - 637   #Diff RMSE  = -115
0.421 - 0.318 #Diff MF    = 0.103


#from errorstats_wide_pooled: WPG-C minus GPW
415 - 467     #Diff MAE   = -52
568 - 619   #Diff RMSE  = -51
0.464 - 0.308 #Diff MF    = 0.156



rm(gpw_mae, gpw_rmse, gpw_mf, slums_agg_nozaf)









############################
## Figure 5. MF of settlements accurately estimated, by CITY
############################


png("fig_5_citymfplot_v8.png", bg="white")

fig_mfplot_pool <- ggplot(mf_wide_pooled, aes(x = reorder(dataset, value), y = value)) +
  geom_point(aes(shape = City, color = City, size = City)) +
  scale_shape_manual(values = c(0, 1, 2, 4, 7, 10, 15, 16, 17, 18)) +
  scale_color_manual(values = c("#f78104", "#c1272d", "#0000a7", "#000000",
                                "#008176", "#b4b4b4", "#c1272d", "#0000a7",
                                "#faab36", "#008176")) +
  scale_size_manual(values = c(2.3, 3, 2.3, 2.5, 2.1, 3, 2.5, 3, 2.5, 3)) +
  theme_minimal(base_size = 11) +
  scale_y_continuous(breaks = seq(0, 1.04, by = .2), limits = c(0, 1.04)) +
  ylab("Median Fraction (MF) of KYC population estimated") +
  theme(axis.title.y = element_blank()) +
  coord_flip()

fig_mfplot_pool
dev.off()
ggsave("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_5_citymfplot_v8.tiff",
       fig_mfplot_pool, units = "in", width = 8, height = 3.5, dpi = 300, compression = "lzw", bg="white")
ggsave("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_5_citymfplot_v8.png",
       fig_mfplot_pool, units = "in", width = 8, height = 3.5, dpi = 300, bg="white")








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

# 1) Ensure sett_long has the pretty dataset label
if (!"label" %in% names(sett_long)) {
  sett_long <- sett_long %>% dplyr::left_join(ds_map, by = "ds")
}

# 2) Per-city KYC descriptors (from slums)
kyc_stats <- slums %>%
  dplyr::group_by(City) %>%
  dplyr::summarise(
    kyc_n        = dplyr::n(),
    kyc_dens_m   = mean(dens_kyc, na.rm = TRUE),
    kyc_dens_sd  = sd(dens_kyc,   na.rm = TRUE),
    kyc_areakm2  = mean(area_km2, na.rm = TRUE),
    .groups = "drop"
  )

# 3) Per-city × dataset error stats (from sett_long)
errs_city_ds <- sett_long %>%
  dplyr::group_by(City, ds, label) %>%
  dplyr::summarise(
    err_mae     = round(mean(adiff, na.rm = TRUE), 0),
    err_rmse    = round(sqrt(mean(diff2, na.rm = TRUE)), 0),
    err_mf_sett = round(median(frac, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  # Add GridPop (pretty name) and short code 'dat'
  dplyr::mutate(
    GridPop = label,
    dat     = ds
  )

# 4) Combine and order columns as requested (dropping the duplicate City)
errorstats_wide_CITY <- errs_city_ds %>%
  dplyr::left_join(kyc_stats, by = "City") %>%
  dplyr::select(
    City, GridPop,
    kyc_n, kyc_dens_m, kyc_dens_sd,
    err_mae, err_rmse, err_mf_sett,
    kyc_areakm2, label, dat
  )


city_pred <- read_excel("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/cities_data_v4.xlsx", sheet = "cities - accuracy")
data_pred <- read_excel("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/cities_data_v4.xlsx", sheet = "data - accuracy")

names(mf_wide_pooled) <- c("City", "GridPop", "err_mf")

error_cause_data <- merge(errorstats_wide_CITY, mf_wide_pooled, by=c("City", "GridPop"))
error_cause_data <- merge(error_cause_data, city_pred, by="City")
error_cause_data <- merge(error_cause_data, data_pred, by="GridPop")


write.csv(error_cause_data, file="C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/error_cause_data.csv")   # to double check model results in Stata


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

#tiff("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_6_cause_corr3.tiff", units="in", width=8.5, height=8.8, res=300, pointsize = 16, compression = 'lzw')
png("C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/paper_3/fig_6_cause_corr4.png", units="in", width=8.5, height=8.8, res=300, pointsize = 16)
corrplot(corr_1, type="upper", p.mat = p.mat, sig.level = 0.1, insig = "blank", tl.col = "black", tl.srt = 75, col=colorRampPalette(c("#00BFC4", "#FFAF00", "#D25E68"))((200)), diag = FALSE)
dev.off()


#################
# Fig 6. Models of MF/MAE/RMSE and correlated predictors
#################

# check for collinearity (>0.8) among kyc_dens_m<->kyc_dens_sd, cit_pop<->cen_u_area
cor(norm[,c("log_mae", "log_rmse", "sqrt_mf", "kyc_dens_m", "kyc_dens_sd", "cit_pop",   "cen_u_area")])
# keep kyc_dens_m because stronger coorelation with all 3 metrics
# keep cen_u_area because more strongly correlated with all 3 metrics


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

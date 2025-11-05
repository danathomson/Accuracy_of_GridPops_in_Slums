##############################################################
### Comparison of estimates populations in 12 cities       ###
### Part 1: Generate dataset for KYC accuracy assessment   ###
### By: Dana R. Thomson & Hazem Mahmoud                    ###
##############################################################

# --- Setup ---
rm(list = ls())

root <- "C:/Users/dthomson.CIESIN_NT/Dropbox/Work_main/p_ciesin_pop_slum_compare/data"

library(sf)
library(raster)
library(dplyr)
library(purrr)
library(exactextractr)
library(tibble)
library(stringr)
library(tmaptools)  # for bb()

# --- Projections ---
proj.moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
proj.utm <- list(
  LAG = "+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  PHC = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  NAI = "+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  KIS = "+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  DAR = "+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  KAM = "+proj=utm +zone=36 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  CAP = "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  ACC = "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  DAV = "+proj=utm +zone=51 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  MAL = "+proj=utm +zone=51 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
)

# --- City parameters ---
cities <- tribble(
  ~city_code, ~city,        ~iso3, ~kyc_relpath,                                                    ~year_col,    ~pop_col,  ~year_sep,
  "LAG",      "Lagos",      "NGA", "kyc_campaign/nga/kyc_cln_data_Lagos_Nigeria.shp",               "Last_updat", "kyc_pop", ".",
  "PHC",      "PortHarcourt","NGA", "kyc_campaign/nga/kyc_cln_data_PortHarcourt_Nigeria.shp",       "Last_updat", "kyc_pop", ".",
  "NAI",      "Nairobi",    "KEN", "kyc_campaign/ken/kyc_cln_data_Nairobi_Kenya.shp",               "Date_updat", "kyc_pop", ".",
  "KIS",      "Kisumu",     "KEN", "kyc_campaign/ken/kyc_cln_data_Kisumu_Kenya.shp",                "Last_Updat", "pop",     "-",
  "DAR",      "DarEsSalaam","TZA", "kyc_campaign/tza/kyc_cln_data2_DarEsSalaam_Tanzania.shp",       "Last_Updat", "pop",     "-",
  "KAM",      "Kampala",    "UGA", "kyc_campaign/uga/kyc_cln_data_Kampala_Uganda.shp",              "Last_Updat", "pop",     "-",
  "CAP",      "CapeTown",   "ZAF", "kyc_campaign/zaf/kyc_cln_data_CapeTown_SouthAfrica.shp",        "Last_Updat", "pop",     "-",
  "ACC",      "Accra",      "GHA", "kyc_campaign/gha/kyc_cln_data_Accra_Ghana.shp",                 "Last_Updat", "pop",     "-",
  "DAV",      "Davao",      "PHL", "kyc_campaign/phl/kyc_cln_data_Davao_Philippines.shp",           "Last_Updat", "pop",     "-",
  "MAL",      "Malabon",    "PHL", "kyc_campaign/phl/kyc_cln_data_Malabon_Philippines.shp",         "Last_Updat", "pop",     "-"
) %>%
  mutate(utm = proj.utm[city_code] %>% unlist())


# --- Raster specs (paths depend on iso3) ---
build_paths <- function(iso3) {
  iso <- tolower(iso3)
  list(
    gpw15 = file.path(root, "gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif"),
    gpw20 = file.path(root, "gpw/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif"),
    ghs15 = file.path(root, "ghspop/GHS_POP_E2015_GLOBE_R2023A_54009_100_V1_0.tif"),
    ghs20 = file.path(root, "ghspop/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif"),
    hrsl20 = dplyr::case_when(
      iso3 == "NGA" ~ file.path(root, "hrsl/2018-2020/nga_general_2020.tif"),
      iso3 == "KEN" ~ file.path(root, "hrsl/2018-2020/ken_general_2020.tif"),
      iso3 == "TZA" ~ file.path(root, "hrsl/2018-2020/tza_general_2020.tif"),
      iso3 == "UGA" ~ file.path(root, "hrsl/2018-2020/uga_general_2020.tif"),
      iso3 == "GHA" ~ file.path(root, "hrsl/2018-2020/gha_general_2020.tif"),
      iso3 == "PHL" ~ file.path(root, "hrsl/2018-2020/population_phl_2018-10-01.tif"),
      TRUE ~ NA_character_
    ),
    #wp_u15 = file.path(root, sprintf("wp_u/%s_ppp_2015_UNadj.tif", iso)),
    #wp_u20 = file.path(root, sprintf("wp_u/%s_ppp_2020_UNadj.tif", iso)),
    wp_c20 = file.path(root, sprintf("wp_c/%s_ppp_2020_UNadj_constrained.tif", iso)),
    wp_g15 = file.path(root, sprintf("wp-g2/%s_pop_2015_CN_100m_R2025A_v1.tif", iso)),
    wp_g20 = file.path(root, sprintf("wp-g2/%s_pop_2020_CN_100m_R2025A_v1.tif", iso)),
    wp_pb20 = dplyr::case_when(
      iso3 == "NGA" ~ file.path(root, "wp_pb/NGA_population_202207292317.tif"),
      iso3 == "KEN" ~ file.path(root, "wp_pb/KEN_population_202207292310.tif"),
      iso3 == "TZA" ~ file.path(root, "wp_pb/TZA_population_202207292319.tif"),
      iso3 == "UGA" ~ file.path(root, "wp_pb/UGA_population_202207292321.tif"),
      iso3 == "GHA" ~ file.path(root, "wp_pb/GHA_population_202207292314.tif"),
      iso3 == "ZAF" ~ file.path(root, "wp_pb/ZAF_population_202207292324.tif"),
      TRUE ~ NA_character_
    ),
    lsg20  = file.path(root, "landscan/LandScan Global 2020/landscan-global-2020.tif"),
    lshd20 = if (iso3 == "NGA") file.path(root, "landscan/LandScan HD/landscan-hd-nigeria.tif") else NA_character_,
    grid19 = if (iso3 == "NGA") file.path(root, "grid3/NGA_population_v2_1_gridded/NGA_population_v2_1_gridded.tif") else NA_character_,
    
    # NEW: LandScan Mosaic (only for KEN, TZA, UGA, PHL)
    lsm = dplyr::case_when(
      iso3 == "KEN" ~ file.path(root, "landscan/LandScan Mosaic/landscan-mosaic-kenya-v1.tif"),
      iso3 == "TZA" ~ file.path(root, "landscan/LandScan Mosaic/landscan-mosaic-tanzania-v1.tif"),
      iso3 == "UGA" ~ file.path(root, "landscan/LandScan Mosaic/landscan-mosaic-uganda-v1.tif"),
      iso3 == "PHL" ~ file.path(root, "landscan/LandScan Mosaic/landscan-mosaic-philippines-v1.tif"),
      TRUE ~ NA_character_
    )
  )
}


# --- Function: safe raster extraction (sum) ---
extract_sum <- function(r_path, slums_sf, bbox, target_crs) {
  if (is.na(r_path) || !file.exists(r_path)) return(rep(NA_real_, nrow(slums_sf)))
  r <- raster(r_path)
  r <- crop(r, bbox)
  r <- projectRaster(r, crs = target_crs)
  exact_extract(r, slums_sf, "sum")
}

# --- Function: load + standardize KYC for one city ---
load_kyc <- function(city_row) {
  # Read raw KYC shapefile
  shp <- sf::st_read(file.path(root, city_row$kyc_relpath), quiet = TRUE)
  
  # Keep only needed columns and standardize names
  shp <- shp[, names(shp) %in% c("Country", "City", "Settlement",
                                 city_row$year_col, city_row$pop_col)]
  names(shp)[names(shp) == city_row$year_col] <- "KYC_Year"
  names(shp)[names(shp) == city_row$pop_col]  <- "KYC_Pop"
  
  # Preserve original year string for auditing
  shp$KYC_Year_raw <- as.character(shp$KYC_Year)
  
  # Bounding boxes in source CRS (WGS84 for ll; Mollweide for GHS convenience)
  bbox_ll   <- tmaptools::bb(shp, ext = 1.2, output = "extent")
  bbox_moll <- tmaptools::bb(shp, ext = 1.2, projection = proj.moll, output = "extent")
  
  # Project polygons ONCE to the city's UTM for downstream area/extraction
  slums <- sf::st_transform(shp, crs = city_row$utm)
  
  # --- Clean year robustly ---
  # 1) Split by the declared separator and take the last token (handles "10.09.2018" or "2018-09-10")
  raw_year <- as.character(slums$KYC_Year_raw)
  parts <- stringr::str_split(raw_year, stringr::fixed(city_row$year_sep))
  last_token <- vapply(parts, function(x) if (length(x)) tail(x, 1) else NA_character_, character(1))
  year_num <- suppressWarnings(as.numeric(last_token))
  
  # 2) Fallback: extract any 4-digit year if still NA
  miss <- is.na(year_num)
  if (any(miss)) {
    year_num[miss] <- suppressWarnings(
      as.numeric(stringr::str_extract(raw_year[miss], "\\b(19|20)\\d{2}\\b"))
    )
  }
  slums$KYC_Year <- year_num
  
  # Return objects needed downstream
  list(slums = slums, bbox_ll = bbox_ll, bbox_moll = bbox_moll)
}

# --- Function: summarize all rasters for one city ---
summarize_city <- function(city_row) {
  paths <- build_paths(city_row$iso3)
  
  kyc <- load_kyc(city_row)
  sl  <- kyc$slums
  
  # Extract per dataset 
  sl$gpw15   <- extract_sum(paths$gpw15, sl, kyc$bbox_ll,   city_row$utm)
  sl$gpw20   <- extract_sum(paths$gpw20, sl, kyc$bbox_ll,   city_row$utm)
  sl$ghs15   <- extract_sum(paths$ghs15, sl, kyc$bbox_moll, city_row$utm)
  sl$ghs20   <- extract_sum(paths$ghs20, sl, kyc$bbox_moll, city_row$utm)
  sl$hrsl15  <- NA_real_
  sl$hrsl20  <- extract_sum(paths$hrsl20, sl, kyc$bbox_ll,  city_row$utm)
  #sl$wp_u15  <- extract_sum(paths$wp_u15, sl, kyc$bbox_ll,  city_row$utm)
  #sl$wp_u20  <- extract_sum(paths$wp_u20, sl, kyc$bbox_ll,  city_row$utm)
  sl$wp_c20  <- extract_sum(paths$wp_c20, sl, kyc$bbox_ll,  city_row$utm)
  sl$wp_g15  <- extract_sum(paths$wp_g15, sl, kyc$bbox_ll,  city_row$utm)
  sl$wp_g20  <- extract_sum(paths$wp_g20, sl, kyc$bbox_ll,  city_row$utm)
  sl$wp_pb20 <- extract_sum(paths$wp_pb20, sl, kyc$bbox_ll, city_row$utm)
  sl$lsg15   <- NA_real_
  sl$lsg20   <- extract_sum(paths$lsg20, sl, kyc$bbox_ll,   city_row$utm) #LandScan Global (2020)
  sl$lshd20  <- extract_sum(paths$lshd20, sl, kyc$bbox_ll,  city_row$utm) #LandScan HD (NGA only)
  sl$lsm20   <- extract_sum(paths$lsm,    sl, kyc$bbox_ll,  city_row$utm) #LandScan Mosaic (KEN, TZA, UGA, PHL only)
  sl$grid16  <- NA_real_
  sl$grid19  <- extract_sum(paths$grid19, sl, kyc$bbox_ll,  city_row$utm)
  
  # Areas 
  sl$area_m2   <- as.numeric(st_area(sl))
  sl$area_100m <- sl$area_m2 / 10000
  sl$area_km2  <- sl$area_m2 / 1000000
  
  # exclude South Africa HRSL due to error in dataset
  if (city_row$iso3 == "ZAF") sl$hrsl20 <- NA_real_  
  
  # Attach city code to facilitate downstream checks
  sl$CityCode <- city_row$city_code
  sl
}

# --- Run all cities ---
rows <- purrr::transpose(as.list(cities))
slums_list <- purrr::map(rows, summarize_city)


# --- Drop geometry column, Compute combos + densities ---
slums_list <- map(slums_list, function(x) {
  x <- st_drop_geometry(x)
  as.data.frame(x)
})

slums_df <- bind_rows(slums_list)
slums_df$Id <- seq_len(nrow(slums_df))
slums_df$KYC_Year <- as.numeric(slums_df$KYC_Year)

# Year-aligned combos 
slums_df$gpw_combo   <- ifelse(slums_df$KYC_Year >= 2017, slums_df$gpw20,   slums_df$gpw15)
slums_df$ghs_combo   <- ifelse(slums_df$KYC_Year >= 2017, slums_df$ghs20,   slums_df$ghs15)
slums_df$hrsl_combo  <- slums_df$hrsl20
#slums_df$wp_u_combo  <- ifelse(slums_df$KYC_Year >= 2017, slums_df$wp_u20,  slums_df$wp_u15)
slums_df$wp_c_combo  <- slums_df$wp_c20
slums_df$wp_g2_combo <- ifelse(slums_df$KYC_Year >= 2017, slums_df$wp_g20,  slums_df$wp_g15)
slums_df$wp_pb_combo <- slums_df$wp_pb20
slums_df$lsg_combo   <- slums_df$lsg20
slums_df$lshd_combo  <- slums_df$lshd20
slums_df$lsm_combo <- slums_df$lsm20
slums_df$grid_combo  <- slums_df$grid19

# Densities per 100x100m 
slums_df$dens_kyc    <- slums_df$KYC_Pop    / slums_df$area_100m
slums_df$dens_gpw    <- slums_df$gpw_combo  / slums_df$area_100m
slums_df$dens_ghs    <- slums_df$ghs_combo  / slums_df$area_100m
slums_df$dens_hrsl   <- slums_df$hrsl_combo / slums_df$area_100m
#slums_df$dens_wp_u   <- slums_df$wp_u_combo / slums_df$area_100m
slums_df$dens_wp_c   <- slums_df$wp_c_combo / slums_df$area_100m
slums_df$dens_wp_g2  <- slums_df$wp_g2_combo/ slums_df$area_100m
slums_df$dens_wp_pb  <- slums_df$wp_pb_combo/ slums_df$area_100m
slums_df$dens_lsg    <- slums_df$lsg_combo  / slums_df$area_100m
slums_df$dens_lshd   <- slums_df$lshd_combo / slums_df$area_100m
slums_df$dens_lsm <- slums_df$lsm_combo / slums_df$area_100m
slums_df$dens_grid   <- slums_df$grid_combo / slums_df$area_100m

# --- Save ---
out_path <- file.path(root, "kyc_campaign/COMBINED/slums_v13.csv")
write.csv(slums_df, file = out_path, row.names = FALSE)

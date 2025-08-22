# Packages ----

library(tidyverse)
library(tidycensus)
library(readxl)
library(openxlsx)
library(sf)
library(arcgisbinding)
library(fredr)

# Setting file paths ----

zillow_sf_home_price_data_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow Data/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv"

realtor_inventory_data_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow Data/RDC_Inventory_Core_Metrics_County_History.csv"

zip_shapefile_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/USAShapefiles/2023/ZCTAs/cb_2020_us_zcta520_500k.shp"

county_shapefile_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/USAShapefiles/2023/Counties/cb_2023_us_county_500k.shp"

output_filepath_for_zhvi_shapefile <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow Data/zillow_data.shp" 
output_filepath_for_inventory_shapefile <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow Data/realtor_inventory_data.shp"

# Reading in the empty shape files ----

zip_shapefile <- st_read(zip_shapefile_file_path) 

zip_shapefile <- zip_shapefile %>%
  rename(zip_code = ZCTA5CE20) %>%
  select(zip_code, geometry)

county_shapefile <- st_read(county_shapefile_file_path) 

county_shapefile <- county_shapefile %>%
  rename(county_fips = GEOID, county_name = NAME, state_name = STATE_NAME) %>%
  select(county_fips, county_name, state_name, geometry) 
# Clean zillow sf price data ----

zillow_home_price_sf_homes_raw <- read.csv(zillow_sf_home_price_data_file_path)

zillow_home_price_sf_homes <- zillow_home_price_sf_homes_raw %>%
  select(RegionID:CountyName, 
         X2024.11.30, X2024.10.31 , X2024.09.30, 
         X2023.11.30, X2023.10.31 , X2023.09.30, 
         X2019.11.30, X2019.10.31 , X2019.09.30) %>%
  rename(nov_24 = X2024.11.30, oct_24 = X2024.10.31, sep_24 = X2024.09.30,
         nov_23 = X2023.11.30, oct_23 = X2023.10.31, sep_23 = X2023.09.30,
         nov_19 = X2019.11.30, oct_19 = X2019.10.31, sep_19 = X2019.09.30,
         zip_code = RegionName) %>%
  mutate(zip_code = as.character(zip_code)) 


zillow_home_price_sf_homes <-zillow_home_price_sf_homes %>%
  mutate(zip_code = if_else(
    str_length(zip_code) == 4, paste0('0', zip_code), zip_code)
    )

zillow_home_price_sf_homes <- zillow_home_price_sf_homes %>%
  left_join(zip_shapefile, by = 'zip_code')

zillow_home_price_sf_homes <- zillow_home_price_sf_homes %>%
  mutate(trailing_three_months_24 = rowMeans(select(., nov_24, oct_24, sep_24), na.rm = TRUE),
         trailing_three_months_23 = rowMeans(select(., nov_23, oct_23, sep_23), na.rm = TRUE),
         trailing_three_months_19 = rowMeans(select(., nov_19, oct_19, sep_19), na.rm = TRUE))



zillow_home_price_sf_homes <- zillow_home_price_sf_homes %>%
  mutate(six_yr_gr = (trailing_three_months_24 - trailing_three_months_19) / trailing_three_months_19,
         one_yr_gr = (trailing_three_months_24 - trailing_three_months_23) / trailing_three_months_23)

# Clean realtor inventory data ----

realtor_inventory_raw <- read.csv(realtor_inventory_data_file_path)

realtor_inventory <- realtor_inventory_raw %>%
  mutate(date = as.character(month_date_yyyymm),
         county_fips = as.character(county_fips)) %>%
  filter(date %in% c('202411', '202410', '202409', '201911', '201910', '201909')) %>%
  select(date, county_fips, county_name, active_listing_count) %>%
  mutate(county_fips = if_else(str_length(county_fips) == 4, paste0('0', county_fips), county_fips))


realtor_inventory <- realtor_inventory %>%
  pivot_wider(names_from = date, values_from = active_listing_count, id_cols = c('county_fips', 'county_name'))
 # rename(nov_24 = `202411` , nov_23 = `202311`, nov_19 = `201911`)
 
realtor_inventory <- realtor_inventory %>%
  left_join(county_shapefile, by = 'county_fips')

realtor_inventory <- realtor_inventory %>%
  mutate(six_yr_gr = ((`202411` + `202410` + `202409`) - (`201911` + `201910` + `201909`)) / (`201911` + `201910` + `201909`))

realtor_inventory <- realtor_inventory %>%
  mutate(six_yr_gr = if_else(is.infinite(six_yr_gr), NA, six_yr_gr))

# Output data ----

zillow_home_price_sf_homes <- st_as_sf(zillow_home_price_sf_homes)
realtor_inventory <- st_as_sf(realtor_inventory)

arc.check_product()

arc.write(path = output_filepath_for_zhvi_shapefile, data = zillow_home_price_sf_homes, overwrite = T, validate = T)
arc.write(path = output_filepath_for_inventory_shapefile, data = realtor_inventory, overwrite = T, validate = T)


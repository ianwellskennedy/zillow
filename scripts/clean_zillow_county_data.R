# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "tidycensus", "ggmap", "sf", "openxlsx", "arcgisbinding", "conflicted")

# Function to check and install missing packages
install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Apply the function to each package
invisible(sapply(packages, install_if_missing))

# Load the packages
library(tidyverse)
library(tidycensus)
library(ggmap)
library(sf)
library(openxlsx)
library(arcgisbinding)
library(conflicted)
library(fredr)

# Prefer certain packages for certain functions
conflicts_prefer(dplyr::filter, dplyr::lag, lubridate::year, base::`||`, base::is.character, base::`&&`, stats::cor, base::as.numeric)

rm(install_if_missing, packages)

# Set the FRED API Key, if a new user is using this you will have to obtain an API key from here: https://fred.stlouisfed.org/docs/api/api_key.html
fredr_set_key(key = 'c1f7f3d38687246c6d6e5b83898af5a1')

# Setting file paths ----

zillow_zhvi_data_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow_Realtor_Redfin_Data/Inputs/Zillow/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.xlsx"

zillow_zori_data_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow_Realtor_Redfin_Data/Inputs/Zillow/Metro_zori_uc_sfrcondomfr_sm_month.xlsx"

metro_shapefile_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/shapefiles/2023/CBSAs/cb_2023_us_cbsa_500k.shp" # Input the file path for the shape file that you would like to read in. 

output_filepath_for_shapefile <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow_Realtor_Redfin_Data/shapefiles/zhvi_vs_zori.shp" 

zillow_census_metro_crosswalk_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow_Realtor_Redfin_Data/Inputs/Zillow/zillow_metro_crosswalk.xlsx"

# Reading in the empty shape files ----

metro_shapefile <- st_read(metro_shapefile_file_path)

metro_shapefile_geometry <- metro_shapefile %>%
  select(GEOID, geometry)

metro_shapefile_information <- metro_shapefile %>%
  st_drop_geometry() %>%
  select(-c(LSAD, ALAND, AWATER))

# Clean ZHVI data ----

zillow_zhvi_data <- read.xlsx(zillow_zhvi_data_file_path, sheet = 'clean_ttm_yoy')


zillow_zhvi_data <- zillow_zhvi_data %>%
  filter(RegionID != '102001') %>%
  select(RegionID, mar_25) %>%
  rename(zhvi_mar_25 = mar_25) %>%
  mutate(zillow_metro_code = as.character(RegionID)) %>%
  select(zillow_metro_code, zhvi_mar_25)


# Clean ZORI data ----

zillow_zori_data <- read.xlsx(zillow_zori_data_file_path, sheet = 'clean_ttm_yoy')

zillow_zori_data <- zillow_zori_data %>%
  filter(RegionID != '102001') %>%
  select(RegionID, mar_25) %>%
  rename(zori_mar_25 = mar_25) %>%
  mutate(zillow_metro_code = as.character(RegionID)) %>%
  select(zillow_metro_code, zori_mar_25)

# Join data ----

joined_data <- zillow_zhvi_data %>%
  left_join(zillow_zori_data, by = 'zillow_metro_code')

# Clean Zillow metro codes ----

zillow_census_metro_crosswalk <- read.xlsx(zillow_census_metro_crosswalk_file_path) %>%
  mutate(zillow_metro_code = as.character(zillow_metro_code))

joined_data <- joined_data %>% 
  left_join(zillow_census_metro_crosswalk, by = 'zillow_metro_code')

zillow_data <- zillow_data %>% 
  select(ends_with('metro_name'), GEOID, zillow_metro_code, everything())

# Output data ----

joined_data <- joined_data %>%
  left_join(county_shapefile, by = c('county_fips_code' = 'county_fips')) %>%
  select(county_name, Metro, state_name, State, zhvi_mar_25, zori_mar_25, geometry) 

joined_data <- st_as_sf(joined_data)

arc.check_product()

arc.write(path = output_filepath_for_shapefile, data = joined_data, overwrite = T, validate = T)


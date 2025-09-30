# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "tidycensus", "ggmap", "sf", "openxlsx", "arcgisbinding", "conflicted", "zoo")

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
library(zoo)

# Prefer certain packages for certain functions
conflicts_prefer(dplyr::filter, dplyr::lag, lubridate::year, base::`||`, base::is.character, base::`&&`, stats::cor, base::as.numeric)

rm(install_if_missing, packages)

# Set the FRED API Key, if a new user is using this you will have to obtain an API key from here: https://fred.stlouisfed.org/docs/api/api_key.html
fredr_set_key(key = 'c1f7f3d38687246c6d6e5b83898af5a1')

# Setting file paths ----

#zhvi_data_file_path <- "inputs/County_zori_uc_sfrcondomfr_sm_month.csv"

zori_data_file_path <- "inputs/County_zori_uc_sfrcondomfr_sm_month.csv"

county_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2020/Counties/cb_2020_us_county_5m.shp" # Input the file path for the shape file that you would like to read in. 

output_file_path_for_tabular_data <- "outputs/county_level_zori_data_cleaned.xlsx"

# Reading in the empty shape files ----

county_shp <- st_read(county_shp_file_path)

county_shp_geo <- county_shp %>%
  select(GEOID, geometry)

county_shp_info <- county_shp %>%
  st_drop_geometry() %>%
  select(-c(LSAD, ALAND, AWATER))

# Clean ZORI data ----

zori_data <- read.csv(zori_data_file_path)

zori_data <- zori_data %>%
  select(SizeRank, RegionName, State, Metro, StateCodeFIPS, MunicipalCodeFIPS, everything()) %>%
  rename(pop_rank = SizeRank, county_name = RegionName, state_abbr = State, metro_name = Metro, state_fips_code = StateCodeFIPS, county_fips_code = MunicipalCodeFIPS) %>%
  mutate(state_fips_code = as.character(state_fips_code),
         county_fips_code = as.character(county_fips_code))

zori_data <- zori_data %>%
  mutate(
    state_fips_code = case_when(
      str_length(state_fips_code) == 1 ~ paste0("0", state_fips_code), 
      T ~ state_fips_code
      ),
    county_fips_code = case_when(
      str_length(county_fips_code) == 1 ~ paste0("00", county_fips_code),
      str_length(county_fips_code) == 2 ~ paste0("0", county_fips_code),
      T ~ county_fips_code
  ),
  county_fips_code = paste0(state_fips_code, county_fips_code)
  ) %>%
  select(-c(state_fips_code, RegionID, RegionType, StateName))

zori_data <- zori_data %>%
  pivot_longer(
    cols = matches("^X"), 
    names_to = "month", 
    values_to = "zori"
  )

zori_data <- zori_data %>%
  mutate(month = str_remove(month, "X"),
         month = str_replace_all(month, pattern = "\\.", replacement = "-"))

zori_data$month <- as.Date(zori_data$month, format = "%m-%d-%Y")

# Create YOY and TTM values ----

zori_data <- zori_data %>%
  arrange(county_name, month) %>%
  group_by(county_fips_code) %>%
  mutate(
    # trailing 3-month average
    zori_ttm = rollmean(zori, k = 3, align = "right", fill = NA),
    
    # year-over-year change (12 months back)
    zori_yoy = (zori / lag(zori, 12)) - 1,
    
    # trailing 3-month YoY (compare 3m avg vs. 3m avg 12 months ago)
    zori_ttm_yoy = (zori_ttm / lag(zori_ttm, 12)) - 1) %>%
  ungroup()



# Output data ----

write.xlsx(zori_data, output_file_path_for_tabular_data)

joined_data <- zori_data %>%
  left_join(county_shp, by = c('county_fips_code' = 'GEOID'))

joined_data <- st_as_sf(joined_data)

arc.check_product()

arc.write(path = output_filepath_for_shapefile, data = joined_data, overwrite = T, validate = T)


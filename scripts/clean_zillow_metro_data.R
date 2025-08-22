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

# Setting file paths / environment variables ----

metro_shapefile_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/shapefiles/2023/CBSAs/cb_2023_us_cbsa_500k.shp" # Input the file path for the shape file that you would like to read in. 
# See 'R:/ADHOC-JBREC/Ian-K/USAShapefiles/2023' different geography options.
zhvi_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow_Realtor_Redfin_Data/Inputs/Zillow/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.xlsx"
zori_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow_Realtor_Redfin_Data/Inputs/Zillow/Metro_zori_uc_sfrcondomfr_sm_month.xlsx"

zillow_census_metro_crosswalk_file_path <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow_Realtor_Redfin_Data/Inputs/Zillow/zillow_metro_crosswalk.xlsx"

output_filepath_for_cleaned_data <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow_Realtor_Redfin_Data/Outputs/metro_zhvi_vs_zori.xlsx" # Change this to a file path where you would like to output a cleaned Excel file.

output_filepath_for_shapefile <- "C:/Users/ikennedy/OneDrive - JBREC/Documents/Projects_2023_2024/Mappy Mondays/Zillow_Realtor_Redfin_Data/shapefiles/metro_zhvi_vs_zori.shp" # Change this to a file path for where you would like to output a cleaned shape file. IGNORE IF NOT OUTPUTTING A SHAPEFILE!

# Read in data ----

zhvi_data <- read.xlsx(zhvi_file_path, sheet = 'ttm_yoy')
zori_data <- read.xlsx(zori_file_path, sheet = 'ttm_yoy')

metro_crosswalk <- read.xlsx(zillow_census_metro_crosswalk_file_path)

# Read in spatial files (ignore if not outputting a shapefile) ----

# Note, these files will contain geographies from US Territories (i.e. Puerto Rico, Guam, etc.). Remove them if need be!

metro_shapefile <- st_read(metro_shapefile_file_path)

metro_shapefile_geometry <- metro_shapefile %>%
  select(GEOID, geometry)

metro_shapefile_information <- metro_shapefile %>%
  st_drop_geometry() %>%
  select(-c(LSAD, ALAND, AWATER))


# Clean data ----


# Convert numeric-looking column names to actual Date
excel_date_origin <- as.Date("1899-12-30")  # Excel origin

colnames(zhvi_data) <- sapply(colnames(zhvi_data), function(x) {
  if (grepl("^[0-9]+$", x)) {
    # Convert to Date if it's numeric
    as.character(as.Date(as.numeric(x), origin = excel_date_origin))
  } else {
    x  # Leave non-numeric column names unchanged
  }
})
colnames(zori_data) <- sapply(colnames(zori_data), function(x) {
  if (grepl("^[0-9]+$", x)) {
    # Convert to Date if it's numeric
    as.character(as.Date(as.numeric(x), origin = excel_date_origin))
  } else {
    x  # Leave non-numeric column names unchanged
  }
})

zhvi_data <- zhvi_data %>%
  select(RegionID:StateName, `2025-05-31`) %>%
  rename(zhvi_may25 = `2025-05-31`)
zori_data <- zori_data %>%
  select(RegionID:StateName, `2025-05-31`) %>%
  rename(zori_may25 = `2025-05-31`)

joined_data <- zhvi_data %>%
  left_join(zori_data, by = c('RegionID', 'SizeRank', 'RegionName', 'RegionType', 'StateName')) %>%
  select(-c(StateName, RegionType, SizeRank))

joined_data <- joined_data %>%
  rename(zillow_metro_code = RegionID, zillow_metro_name = RegionName) %>%
  mutate(zillow_metro_code = as.character(zillow_metro_code))

# Clean zillow metro codes ----

metro_crosswalk <- metro_crosswalk %>%
  mutate(zillow_metro_code = as.character(zillow_metro_code)) %>%
  select(-zillow_metro_name)

joined_data <- joined_data %>% 
  left_join(metro_crosswalk, by = c('zillow_metro_code'))

joined_data <- joined_data %>% 
  select(ends_with('metro_name'), GEOID, zillow_metro_code, everything())

joined_data <- joined_data %>% 
  filter(!is.na(zhvi_may25) & !is.na(zori_may25))
# Output cleaned tabular data ----

write.xlsx(joined_data, output_filepath_for_cleaned_data)
# Create a spatial file and plot it! (ignore if not outputting a shapefile) ----

# Join the shapefile geometry to the summarized data by GEOID:
spatial_data <- joined_data %>%
  left_join(metro_shapefile_geometry, by = 'GEOID')

# Convert 'spatial_data' to an sf object!
spatial_data <- st_as_sf(spatial_data)

# Output spatial data (ignore if not outputting a shapefile) ----

# Check to make sure there is an Active ArcGIS Installation
arc.check_product()

# Output the ACS zip code data to the path specified
arc.write(path = output_filepath_for_shapefile, data = spatial_data, overwrite = TRUE, validate = TRUE)

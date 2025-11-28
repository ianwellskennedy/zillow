# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "tidycensus", "ggmap", "sf", "openxlsx", "arcgisbinding", "conflicted", "zoo")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Remove unneeded variables
rm(packages, installed_packages)

# Prefer certain packages for certain functions
conflicts_prefer(dplyr::filter, dplyr::lag, lubridate::year, base::`||`, base::is.character, base::`&&`, stats::cor, base::as.numeric)

# Setting file paths / environment variables ----

metro_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/CBSAs/cb_2024_us_cbsa_5m.shp"
zillow_data_file_path <- "inputs/Metro_invt_fs_uc_sfrcondo_month.xlsx"

zillow_census_metro_crosswalk_file_path <- "inputs/zillow_metro_crosswalk.xlsx"

output_filepath_for_cleaned_data <- "outputs/metro_inventory_data.xlsx" # Change this to a file path where you would like to output a cleaned Excel file.

output_filepath_for_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/zillow/shapefiles/metro_inventory_data.shp" # Change this to a file path for where you would like to output a cleaned shape file. IGNORE IF NOT OUTPUTTING A SHAPEFILE!

# Read in Zillow data ----

zillow_data <- read.xlsx(zillow_data_file_path)

zillow_data <- zillow_data %>%
  pivot_longer(names_to = 'date', values_to = 'inv', cols = `43131`:ncol(zillow_data))

zillow_data <- zillow_data %>%
  mutate(date = as.Date(as.numeric(date), origin = '1899-12-30'),
         RegionID = as.character(RegionID)) %>%
  filter(RegionType == 'msa')

zillow_data <- zillow_data %>%
  rename(zillow_metro_code = RegionID, pop_rank = SizeRank) %>%
  select(zillow_metro_code, pop_rank, date, inv) %>%
  arrange(zillow_metro_code, date)

zillow_data <- zillow_data %>%
  group_by(zillow_metro_code) %>%
  mutate(
    ttm_inv = rollsum(inv, k = 3, align = "right", fill = NA),
    yoy_growth = (inv - lag(inv, 12))/ lag(inv, 12),
    yoy_ttm_growth = (ttm_inv - lag(ttm_inv, 12))/ lag(ttm_inv, 12),
    inv_index = inv / inv[date == as.Date("2019-10-31")] * 100,
    ttm_inv_index = ttm_inv / ttm_inv[date == as.Date("2019-10-31")] * 100,
  ) %>%
  ungroup() 


# Join crossover file ----

zillow_census_metro_crosswalk <- read.xlsx(zillow_census_metro_crosswalk_file_path) %>%
  mutate(zillow_metro_code = as.character(zillow_metro_code))

zillow_data <- zillow_data %>% 
  left_join(zillow_census_metro_crosswalk, by = c('zillow_metro_code'))

zillow_data <- zillow_data %>% 
  select(ends_with('metro_name'), GEOID, zillow_metro_code, everything())

# Read in spatial files (ignore if not outputting a shapefile) ----

# Note, these files will contain geographies from US Territories (i.e. Puerto Rico, Guam, etc.). Remove them if need be!

metro_shapefile <- st_read(metro_shapefile_file_path)

metro_shapefile_geometry <- metro_shapefile %>%
  select(GEOID, geometry)

metro_shapefile_information <- metro_shapefile %>%
  st_drop_geometry() %>%
  select(-c(LSAD, ALAND, AWATER))

# Create the spatial file ----

zillow_data_spatial <- zillow_data %>%
  mutate(date = as.character(date)) %>%
  left_join(metro_shapefile_geometry, by = 'GEOID') %>%
  st_as_sf() %>%
  filter(!is.na(census_metro_name))

# Output the spatial file ----

# Check to make sure there is an Active ArcGIS Installation
arc.check_product()

# Output the ACS zip code data to the path specified
arc.write(path = output_filepath_for_shapefile, data = zillow_data_spatial, overwrite = TRUE, validate = TRUE)

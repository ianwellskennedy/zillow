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

# Set the FRED API Key, if a new user is using this you will have to obtain an API key from here: https://fred.stlouisfed.org/docs/api/api_key.html
fredr_set_key(key = 'c1f7f3d38687246c6d6e5b83898af5a1')

# Setting file paths / environment variables ----

census_api_key <- '6dd2c4143fc5f308c1120021fb663c15409f3757' # Provide the Census API Key, if others are running this you will need to get a Census API key here: https://api.census.gov/data/key_signup.html

acs_year <- 2023
acs_data_type <- 'acs1' # Define the survey to pull data from, 'acs5' for 5-year estimates, 'acs1' for 1 year estimates
geo_level_for_data_pull <- "cbsa" # Define the geography for the ACS data download. Other options include 'state', 'county', 'zcta, 'tract', 'block group', etc.
read_in_geometry <- FALSE # Change this to TRUE to pull in spatial data along with the data download 
show_api_call = TRUE # Show the call made to the Census API in the console, this will help if an error is thrown

acs_variables_file_path <- "inputs/acs_variables_2023_acs1.xlsx"

metro_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/CBSAs/cb_2024_us_cbsa_5m.shp"
zillow_data_file_path <- "inputs/Metro_total_monthly_payment_downpayment_0.20_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"

zillow_census_metro_crosswalk_file_path <- "inputs/zillow_metro_crosswalk.xlsx"

output_filepath_for_cleaned_data <- "outputs/metro_housing_affordability_data.xlsx" # Change this to a file path where you would like to output a cleaned Excel file.

output_filepath_for_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/zillow/shapefiles/metro_housing_affordability_data.shp" # Change this to a file path for where you would like to output a cleaned shape file. IGNORE IF NOT OUTPUTTING A SHAPEFILE!

# Create a variable list to read in ----

# Load the variables for the year / dataset selected above
# acs_variables <- load_variables(year = acs_year, dataset = acs_data_type)

variables <- read.xlsx(acs_variables_file_path, sheet = 'Income Variables')

# Select 'name' and 'amended_label' (and rename 'name' to code')
variables <- variables %>%
  select(name, amended_label) %>%
  rename(code = name)

# Create Codes, containing all of the preferred variable codes
variable_codes <- variables$code
# Create Labels, containing all of the amended labels
variable_labels <- variables$amended_label

# Read in the ACS data ----

data <- get_acs(
          geography = geo_level_for_data_pull,
          variables = variable_codes,
          year = acs_year,
          geometry = read_in_geometry,
          key = census_api_key,
          survey = acs_data_type,
          show_call = show_api_call
          )


data <- data %>%
  # Rename 'variable' to 'Code'
  rename(code = variable) %>%
  # Join the variable spreadsheet to the ACS data by 'Code'
  left_join(variables, by = 'code') %>%
  # Rename the listed 'Variable' with the 'AmendedLabel' from the variable spreadsheet
  rename(variable = amended_label) %>%
  # Drop the 'Code' column
  select(-code)

# Pivot the ACS data to a wide format, with columns named by variable. Each geography unit will have one row with one column per variable.
data <- data %>%
  pivot_wider(names_from = 'variable', values_from = 'estimate', id_cols = c('GEOID', 'NAME')) %>%
  arrange(pop)

# Read in inflation ----

wage_growth <- fredr(series_id = 'CIU1020000000000I', 
                     sort_order = 'asc', 
                     frequency = 'q', 
                     observation_start = as.Date('2023-01-01'), observation_end = Sys.Date()) %>%
  # Select the date and value
  select(date, value) 

wage_growth_current <- wage_growth %>%
  filter(date == '2025-04-01') 
wage_growth_current <- wage_growth_current$value
wage_growth <- wage_growth %>%
  mutate(index = wage_growth_current / value) 

wage_growth_to_2025 <- wage_growth %>%
  filter(date == '2023-10-01')
wage_growth_to_2025 <- wage_growth_to_2025$index

# Inflation adjust ACS data ----

data <- data %>%
  mutate(med_hh_inc_25 = med_hh_inc * wage_growth_to_2025,
         med_hh_inc_owners_25 = med_hh_inc_owners * wage_growth_to_2025, 
         med_hh_inc_renters_25 = med_hh_inc_renters * wage_growth_to_2025) 

# Read in Zillow data ----

zillow_data <- read.csv(zillow_data_file_path)
names(zillow_data) <- str_remove(names(zillow_data), "X")

zillow_data <- zillow_data %>%
  pivot_longer(names_to = 'date', values_to = 'paymnt', cols = `2012.01.31`:ncol(zillow_data))

zillow_data <- zillow_data %>%
  mutate(date = as.Date(date, format = "%Y.%m.%d"),
         RegionID = as.character(RegionID)) %>%
  filter(RegionType == 'msa')

# Convert numeric-looking column names to actual Date
#excel_date_origin <- as.Date("1899-12-30")  # Excel origin

# colnames(zillow_data) <- sapply(colnames(zillow_data), function(x) {
#   if (grepl("^[0-9]+$", x)) {
#     # Convert to Date if it's numeric
#     as.character(as.Date(as.numeric(x), origin = excel_date_origin))
#   } else {
#     x  # Leave non-numeric column names unchanged
#   }
# })

zillow_data <- zillow_data %>%
  rename(zillow_metro_code = RegionID, pop_rank = SizeRank) %>%
  select(zillow_metro_code, pop_rank, date, paymnt) %>%
  arrange(zillow_metro_code, date)

zillow_data <- zillow_data %>%
  group_by(zillow_metro_code) %>%
  mutate(
    yoy = (paymnt - lag(paymnt, 12))/ lag(paymnt, 12),
    ttm = rollmean(paymnt, k = 12, align = "right", fill = NA),
    ttm_yoy = (ttm - lag(ttm, 12))/ lag(ttm, 12)
    ) %>%
  ungroup() 

zillow_data <- zillow_data %>%
  rename(med_mortgage_payment_20_down_dec_2024 = `2024-12-31`, zillow_metro_code = metro_code) %>%
  mutate(zillow_metro_code = as.character(zillow_metro_code))

# Clean Zillow metro codes ----

zillow_census_metro_crosswalk <- read.xlsx(zillow_census_metro_crosswalk_file_path) %>%
  mutate(zillow_metro_code = as.character(zillow_metro_code))

zillow_data <- zillow_data %>% 
  left_join(zillow_census_metro_crosswalk, by = c('zillow_metro_code'))

zillow_data <- zillow_data %>% 
  select(ends_with('metro_name'), GEOID, zillow_metro_code, everything())

zillow_data_current_month <- zillow_data %>%
  filter(date == max(zillow_data$date))

# Join data ----

joined_data <- data %>%
  left_join(zillow_data_current_month, by = 'GEOID')

joined_data <- joined_data %>%
  filter(!is.na(zillow_metro_name)) %>%
  select(-c(pop_rank, starts_with('zillow_'), census_metro_name)) 

joined_data <- joined_data %>%
  mutate(ann_mort_pymnt = paymnt * 12,
         rnt_shr_of_med = ann_mort_pymnt / med_hh_inc_renters)

# Output tabular data ----

write.xlsx(joined_data, output_filepath_for_cleaned_data)

# Read in spatial files (ignore if not outputting a shapefile) ----

# Note, these files will contain geographies from US Territories (i.e. Puerto Rico, Guam, etc.). Remove them if need be!

metro_shapefile <- st_read(metro_shapefile_file_path)

metro_shapefile_geometry <- metro_shapefile %>%
  select(GEOID, geometry)

metro_shapefile_information <- metro_shapefile %>%
  st_drop_geometry() %>%
  select(-c(LSAD, ALAND, AWATER))

# Create a spatial file (ignore if not outputting a shapefile) ----

# Join the shapefile geometry to the summarized data by GEOID:
spatial_data <- joined_data %>%
  left_join(metro_shapefile_geometry, by = 'GEOID') %>%
  st_as_sf()

# Output spatial data (ignore if not outputting a shapefile) ----

# Check to make sure there is an Active ArcGIS Installation
arc.check_product()

# Output the ACS zip code data to the path specified
arc.write(path = output_filepath_for_shapefile, data = spatial_data, overwrite = TRUE, validate = TRUE)
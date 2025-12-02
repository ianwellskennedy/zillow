# Packages ----

# Set the packages to read in
packages <- c("tidyverse", "tidycensus", "ggmap", "sf", "openxlsx", "arcgisbinding", "conflicted", "zoo", "fredr")

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

acs_year <- 2024
acs_data_type <- 'acs1' # Define the survey to pull data from, 'acs5' for 5-year estimates, 'acs1' for 1 year estimates
geo_level_for_data_pull <- "cbsa" # Define the geography for the ACS data download. Other options include 'state', 'county', 'zcta, 'tract', 'block group', etc.
read_in_geometry <- FALSE # Change this to TRUE to pull in spatial data along with the data download 
show_api_call = TRUE # Show the call made to the Census API in the console, this will help if an error is thrown

acs_variables_file_path <- "inputs/acs_variables_2023_acs1.xlsx"

puma_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/PUMAs/cb_2020_us_puma20_500k.shp"
metro_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/CBSAs/cb_2024_us_cbsa_5m.shp"
zillow_data_file_path <- "inputs/Metro_total_monthly_payment_downpayment_0.20_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"

zillow_census_metro_crosswalk_file_path <- "inputs/zillow_metro_crosswalk.xlsx"

output_filepath_for_cleaned_data <- "outputs/metro_housing_affordability_data.xlsx" # Change this to a file path where you would like to output a cleaned Excel file.

output_filepath_for_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/zillow/shapefiles/metro_housing_affordability_data.shp" # Change this to a file path for where you would like to output a cleaned shape file. IGNORE IF NOT OUTPUTTING A SHAPEFILE!

# Read in zillow data ----

zillow_data <- read.csv(zillow_data_file_path)
names(zillow_data) <- str_remove(names(zillow_data), "X")

zillow_data <- zillow_data %>%
  pivot_longer(names_to = 'date', values_to = 'paymnt', cols = `2012.01.31`:ncol(zillow_data))

zillow_data <- zillow_data %>%
  mutate(date = as.Date(date, format = "%Y.%m.%d"),
         RegionID = as.character(RegionID)) %>%
  filter(RegionType == 'msa')

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

# Clean zillow metro codes ----

zillow_census_metro_crosswalk <- read.xlsx(zillow_census_metro_crosswalk_file_path) %>%
  mutate(zillow_metro_code = as.character(zillow_metro_code))

zillow_data <- zillow_data %>% 
  left_join(zillow_census_metro_crosswalk, by = c('zillow_metro_code'))

zillow_data <- zillow_data %>% 
  select(ends_with('metro_name'), GEOID, zillow_metro_code, everything())

zillow_data_current_month <- zillow_data %>%
  filter(date == max(zillow_data$date))

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
                     observation_start = as.Date('2024-01-01'), observation_end = Sys.Date()) %>%
  # Select the date and value
  select(date, value) 

wage_growth_current <- wage_growth %>%
  filter(date == '2025-04-01') 
wage_growth_current <- wage_growth_current$value
wage_growth <- wage_growth %>%
  mutate(index = wage_growth_current / value) 

wage_growth_to_2025 <- wage_growth %>%
  filter(date == '2024-10-01')
wage_growth_to_2025 <- wage_growth_to_2025$index

# Inflation adjust ACS data ----

data <- data %>%
  mutate(med_hh_inc_25 = med_hh_inc * wage_growth_to_2025,
         med_hh_inc_owners_25 = med_hh_inc_owners * wage_growth_to_2025, 
         med_hh_inc_renters_25 = med_hh_inc_renters * wage_growth_to_2025) 

# Join data ----

joined_data <- data %>%
  left_join(zillow_data_current_month, by = 'GEOID')

joined_data <- joined_data %>%
  filter(!is.na(zillow_metro_name)) %>%
  select(-c(pop_rank, starts_with('zillow_'), census_metro_name)) 

joined_data <- joined_data %>%
  mutate(ann_mort_pymnt = paymnt * 12,
         rnt_shr_of_med = (ann_mort_pymnt / med_hh_inc_renters)*100)

# # Reading in the empty shape files (ignore if not outputting a shape file) ----
# 
# puma_shp <- st_read(puma_shp_file_path) %>%
#   rename(STATE = STATEFP20, PUMA = PUMACE20, STATE_NAME = ST_NAME20, PUMA_NAME = NAMELSAD20)
# 
# puma_geo <- puma_shp %>%
#   select(STATE, PUMA, geometry)
# 
# puma_info <- puma_shp %>%
#   select(STATE, STATE_NAME, PUMA, PUMA_NAME) %>%
#   st_drop_geometry()
# 
# 
# 
# 
# 
# PUMS_survey_type <- 'acs1' # or 'acs5' for 5-year estimates
# state_selection <- 'RI' # or a vector of state FIPS codes --> c('CA', 'CO'), or 'all'
# puma_selection <- 'all' # Setting this to 'all overrides the argument for 'state' (i.e. all PUMAs' data will be read in regardless of the 'state_selection')
# which_replicate_weights_to_load <- 'none' # or one of the following: 'housing', 'person', 'both'
# 
# # Reading in PUMS data ----
# 
# # Set the variables to pull from PUMS data; add to this vector or create your own!
# pums_variables_of_interest <- c('SERIALNO', 'PUMA','RT', 'WGTP', 'ADJHSG', 'TYPEHUGQ', 'BLD', 'TEN', 'HFL', 'VALP',
#                                 # Costs 
#                                 'CONP', 'ELEP', 'FULP', 'GASP', 'WATP', 'INSP', 'TAXAMT')
# 
# # Retrieve the data
# data <- get_pums(
#   variables = pums_variables_of_interest,
#   year = 2023, 
#   survey = PUMS_survey_type, 
#   state = state_selection,
#   variables_filter = list(TEN = 1:2), # Filter for owned households
#   puma = puma_selection, 
#   rep_weights = which_replicate_weights_to_load,
#   recode = T,
#   show_call = T,
#   key = census_api_key
# )
# 
# # Your code to clean/analyze PUMS data ----
# 
# data_cleaned <- data %>%
#   mutate(
#     # ELEP == 2 (No charge or electricity not used)
#     ELEP_recode = if_else(ELEP == 2, 0, ELEP*12),
#     # WATP == 2 (No charge)
#     WATP_recode = if_else(WATP == 2, 0, WATP),
#     CONP_recode = CONP,
#     # GASP == 3 (No charge or gas not used)
#     GASP_recode = if_else(GASP == 3, 0, GASP*12),
#     # FULP == 2 (No charge or fuel other than gas or electricity not used)
#     FULP_recode = if_else(FULP == 2, 0, FULP)
#   ) %>%
#   distinct(SERIALNO, .keep_all = T)
# 
# data_cleaned <- data_cleaned %>%
#   filter(!BLD_label %in% c('Mobile home or trailer', 'Boat, RV, van, etc.')) %>%
#   group_by(STATE, PUMA) %>%
#   summarize(
#     sf_hh = sum(WGTP, na.rm = T),
#     avg_val = weighted.mean(VALP, w = WGTP, na.rm = T),
#     avg_ins = weighted.mean(INSP, w = WGTP, na.rm = T),
#     avg_tax = weighted.mean(TAXAMT, w = WGTP, na.rm = T),
#     avg_elec = weighted.mean(ELEP_recode, w = WGTP, na.rm = T),
#     avg_wat = weighted.mean(WATP_recode, w = WGTP, na.rm = T),
#     avg_gas = weighted.mean(GASP_recode, w = WGTP, na.rm = T),
#     avg_fuel = weighted.mean(FULP_recode, w = WGTP, na.rm = T)
#   ) %>%
#   ungroup()
# 
# data_final <- data_final %>%
#   mutate(avg_total = rowSums(select(., avg_ins, avg_tax, avg_elec, avg_wat, avg_gas, avg_fuel), na.rm = TRUE))
# 
# data_final <- data_final %>%
#   left_join(puma_info, by = c('PUMA', 'STATE')) %>%
#   mutate(PUMA_NAME = str_remove(PUMA_NAME, ' PUMA')) %>%
#   select(STATE, STATE_NAME, PUMA, PUMA_NAME, everything()) 
# 

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

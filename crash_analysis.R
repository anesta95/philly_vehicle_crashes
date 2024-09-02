library(purrr)
library(sf)
library(tidycensus)
library(dplyr)
library(crsuggest)
library(tigris)
library(stringr)
library(readr)
library(readxl)
library(tidyr)

# Fatal crashes are up substantively in Philadelphia: Averaging 144.25 fatalities
# a year since 2020. This is up over 48% from the 97.25 average from 2016 to 2019:
# https://crashinfo.penndot.pa.gov/PCIT/featuredReports.html?param=134#

# Using this query tool https://crashinfo.penndot.pa.gov/PCIT/queryTool.html 
# we see the number of crashes with injuries actually going
# down during the same time period. From an average of 8008.5 in 2016 - 2019
# to 6023.5 in 2020 to 2023, an almost 25% reduction. 

crash_chg_16_19_to_20_23_type <- read_excel(path = "PA_DOT_Downloaded_Data/Crash_Statistics.xlsx",
           skip = 2,
           col_names = T,
           col_types = "text") %>% 
  rename(crash_type = ...1) %>% 
  pivot_longer(cols = -crash_type, names_to = "year", values_to = "crashes") %>% 
  filter(as.numeric(year) >= 2016) %>% 
  mutate(crashes = as.numeric(crashes),
         year = as.numeric(year),
         year_type = if_else(year < 2020, "Pre-Pandemic", "Post-Pandemic")) %>% 
  group_by(crash_type, year_type) %>% 
  summarize(crashes = mean(crashes)) %>%
  mutate(pct_chg = ((crashes[year_type == "Post-Pandemic"] / crashes[year_type == "Pre-Pandemic"]) - 1) * 100) %>% 
  ungroup() %>% 
  filter(year_type != "Pre-Pandemic") %>% 
  arrange(desc(pct_chg))
  
crash_chg_16_19_to_20_23_type

# Every crash type had a decline from 2016 to 2019 averages to 2020 to 2023 averges
# except motorcycle crashes!

# Story questions  
# What parts of Philadelphia have crashes that result in injuries declined the most?
# If motorcycle crashes are on the rise, what intersections in the city are the
# most dangerous based on injury/fatal crashes from 2020 to 2023?

# Reading in Census api key
con <- file(description = "~/.api_keys/census.txt", open = "rt", blocking = F)
CENSUS_API_KEY <- readLines(con, n = 1)
close(con)

# Census tract map of Philly in 2017 with population
philly_tract_17 <- get_acs(
  geography = "tract",
  variables = c(total_pop = "B01001_001"),
  year = 2017,
  state = "Pennsylvania",
  county = "Philadelphia",
  geometry = T,
  survey = "acs5",
  key = CENSUS_API_KEY
)

# Census tract map of Philly in 2022 with population
philly_tract_22 <- get_acs(
  geography = "tract",
  variables = c(total_pop = "B01001_001"),
  year = 2022,
  state = "Pennsylvania",
  county = "Philadelphia",
  geometry = T,
  survey = "acs5",
  key = CENSUS_API_KEY
)


# Getting the most appropriate CRS for Philly:
philly_crs <- suggest_top_crs(philly_tract_22)

philly_tract_17_reproj <- philly_tract_17 %>% 
  st_transform(philly_crs)

st_crs(philly_tract_17_reproj)

philly_tract_22_reproj <- philly_tract_22 %>% 
  st_transform(philly_crs)

st_crs(philly_tract_17_reproj)

# Getting 2020 Census Blocks for the Population-weighted areal interpolation
# and re-projecting it. https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html#area-weighted-areal-interpolation

philly_blocks <- blocks(
  state = "PA",
  county = "Philadelphia",
  year = 2020
) %>% 
  st_transform(philly_crs)

st_crs(philly_blocks)

# Population-weighted interpolation of 2017 Philly census tracts in 2020 Philly census tract boundaries
# using tidycensus' interpolate_pw().

philly_interpolate_pw <- interpolate_pw(
  philly_tract_17_reproj,
  philly_tract_22_reproj,
  to_id = "GEOID",
  extensive = T,
  weights = philly_blocks,
  weight_column = "POP20",
  crs = philly_crs
)

# Reading in 2016 to 2023 Philly injury crashes and reprojecting to suggested
# Philly CRS
philly_vehicle_i_16_23 <- st_read(
  "shapefile_data/raw_vehicle_injury_crash_2016_2023/PCIT_SELECTED_MAP_CRASHES_V.shp"
  ) %>% 
  st_transform(philly_crs)

st_crs(philly_vehicle_i_16_23)

# Splitting out combined shapefile into two separate sf object of crashes
# from 2016 to 2019 and 2020 to 2023
philly_vehicle_i_16_19 <- philly_vehicle_i_16_23 %>% 
  filter(as.numeric(CRASH_YEAR) < 2020)
  
philly_vehicle_i_20_23 <- philly_vehicle_i_16_23 %>% 
  filter(as.numeric(CRASH_YEAR) >= 2020)

# Doing point-in-polygon join for each crash point shapefile with the respective
# Philly Census tract polygon shapefile

# 2016 to 2019 with the 2017 5-year ACS population interpolated to 2022 ACS
# Philly tracts.
crash_i_16_19_joined <- st_join(
  philly_vehicle_i_16_19,
  philly_interpolate_pw
) %>% 
  st_set_geometry(NULL)

crash_i_16_19_per_1K <- crash_i_16_19_joined %>% 
  group_by(GEOID) %>% 
  summarize(num_crash = n_distinct(CRN),
            estimate = unique(estimate)) %>% 
  mutate(crashes_per_1K_16_19 = (num_crash / estimate) * 100) %>% 
  select(GEOID, crashes_per_1K_16_19)

# 2020 to 2023 with the 2022 5-year ACS Philly tracts.
crash_i_20_23_joined <- st_join(
  philly_vehicle_i_20_23,
  philly_tract_22_reproj
) %>% 
  st_set_geometry(NULL)

crash_i_20_23_per_1K <- crash_i_20_23_joined %>% 
  group_by(GEOID) %>% 
  summarize(num_crash = n_distinct(CRN),
            estimate = unique(estimate)) %>% 
  mutate(crashes_per_1K_20_23 = (num_crash / estimate) * 100) %>% 
  select(GEOID, crashes_per_1K_20_23)

# Calculating the percent change in the crash rate per 1k for each Philly census tract
philly_tract_crash_chg <- inner_join(crash_i_16_19_per_1K, crash_i_20_23_per_1K, by = "GEOID") %>% 
  mutate(pct_chg_16_19_to_20_23 = ((crashes_per_1K_20_23 / crashes_per_1K_16_19) - 1) * 100,
         pct_chg_16_19_to_20_23 = if_else(pct_chg_16_19_to_20_23 %in% c(NA, Inf, NaN), NA, pct_chg_16_19_to_20_23)) 

# There were some tracts with no population but with accidents which are impossible to 
# population weight so they are hard-coded as NAs in the map.

# Joining onto initial Philly ACS '22 5-year tract shapefile and writing out.
philly_crash_chg_map <- left_join(philly_tract_22_reproj, philly_tract_crash_chg, by = "GEOID") %>% 
  mutate(NAME = str_remove(NAME, ";\\s+Philadelphia County;\\s+Pennsylvania")) %>% 
  select(GEOID, NAME, crashes_per_1K_16_19, crashes_per_1K_20_23, pct_chg_16_19_to_20_23)

# Writing out data without shapefile because I uploaded '22 Philly Census tract
# map to Datawrapper already

philly_crash_chg_map %>% 
  st_set_geometry(NULL) %>% 
  write_csv("./analysis_results_data/injury_crashes_chg_16_19_to_20_23.csv")

# TODO: But...fatal crashes keep going up. Why is that? Calculate motorcycle proportion of
# all fatal crashes from 2016 to 2023 and show it is the only increasing category
# from with average from 2016 to 2019 vs. 2020 to 2023.

philly_vehicle_f_16_23 <- st_read(
  "shapefile_data/raw_vehicle_fatal_crash_2016_2023/PCIT_SELECTED_MAP_CRASHES_V.shp"
)

philly_vehicle_f_16_23 %>% 
  st_set_geometry(NULL) %>% 
  group_by(CRASH_YEAR) %>% 
  summarize(tot_crash = n_distinct(CRN),
            num_motorcycle = sum(MOTORCYCLE),
            motorcycle_fatal_prop = num_motorcycle / tot_crash)

# Very interesting, from 2016 to 2019 motorcycles were involved in an average
# of ~20% of fatal vehicle crashes in Philly. The spike in fatalities in 2020 to
# 2023 saw this bump up to a peak of 24% in 2021 but is back down to under 11%
# as of 2023 with still an elevated level roadway fatalities.

# Analysis of the most dangerous roadways from motorcycles post pandemic
# and then put them on a locator map.

motorcycle_i_20_23 <- st_read(
  "shapefile_data/raw_motorcycle_injury_crash_2020_2023/PCIT_SELECTED_MAP_CRASHES_V.shp"
)

motorcycle_f_20_23 <- st_read(
  "shapefile_data/raw_motorcycle_fatal_crash_2020_2023/PCIT_SELECTED_MAP_CRASHES_V.shp"
)

motorcycle_if_20_23 <- bind_rows(motorcycle_i_20_23, motorcycle_f_20_23) %>% 
  st_transform(4326)

st_write(motorcycle_if_20_23,
         "shapefile_data/combined_motorcycle_crash_shapefile/motorcycle_if_20_23.geojson")

# Resources:
# Crash data dictionary: https://gis.penndot.gov/gishub/crashZip/Crash%20Data%20Dictionary%2005.2023.pdf
# Crash database primer: https://gis.penndot.gov/gishub/crashZip/OPEN%20DATA%20PORTAL%20Database%20Primer%2010-16.pdf
# https://www.youtube.com/watch?v=8vT8g_bG7dQ 2:25:51
# https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html#note-aligning-coordinate-reference-systems
# https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html#coordinate-reference-systems
# https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html#spatial-joins





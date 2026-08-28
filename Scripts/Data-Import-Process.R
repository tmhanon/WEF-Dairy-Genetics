################################################################################
#
# Milking Genetic Potential: Structural Change in the U.S. Dairy Industry
# 
# Alexander S. Hopkins and Tristan M. Hanon
# 
# Filename: Data-Import-Process.R
#
################################################################################

# Load Packages
library(mktnewsie)
library(rnassqs)
library(tidyverse)


##### Pull NASS Data #####

# Milk Per Cow Data
nass_mpc_raw <- nassqs(
  short_desc = "MILK - PRODUCTION, MEASURED IN LB / HEAD",
  freq_desc = "ANNUAL",
  agg_level_desc = c("NATIONAL", "STATE"),
  year__GE = 2000,
  progress_bar = FALSE
)

# Filter Milk Per Cow to National and State-level Data Frames
mpc_natl_raw <- nass_mpc_raw %>%
  filter(agg_level_desc == "NATIONAL")

mpc_states_raw <- nass_mpc_raw %>%
  filter(agg_level_desc == "STATE")

# Fat Test Data
nass_fat_test_raw <- nassqs(
  short_desc = "MILK - FAT TEST, MEASURED IN PCT",
  freq_desc = "ANNUAL",
  agg_level_desc = c("NATIONAL", "STATE"),
  year__GE = 2000,
  progress_bar = FALSE
)

# Filter Fat Test to National and State-level Data Frames
fat_test_natl_raw <- nass_fat_test_raw %>%
  filter(agg_level_desc == "NATIONAL")

fat_test_states_raw <- nass_fat_test_raw %>%
  filter(agg_level_desc == "STATE")


##### Pull Datamart Data #####

fmmo_components_raw <- list("Butterfat", "Protein", "Other Solids") %>%
  map(
    \(x) datamart_pull(3462, section = x, lastReports = NULL) %>% 
      mutate(Component = x)
  ) %>%
  bind_rows()


##### Process Data #####

# Process Milk Per Cow and Fat Test Data
mpc_fat_processed <- left_join(
  select(mpc_natl_raw, year, MPC = Value),
  select(fat_test_natl_raw, year, Fat_Test = Value), 
  by = "year"
) %>%
  mutate(
    Year = factor(year, levels = 2000:2025),
    MPC_Index = (MPC / MPC[Year == 2010]) * 100,
    Fat_Index = (Fat_Test / Fat_Test[Year == 2010]) * 100,
    Fat_Per_Cow = (Fat_Test / 100) * MPC,
    FPC_Index = (Fat_Per_Cow / Fat_Per_Cow[Year == 2010]) * 100,
  ) %>%
  arrange(Year)

# Process FMMO Component Data
fmmo_components_processed <- fmmo_components_raw %>%
  filter(is.na(Pool_order_No), report_year < 2026) %>%
  select(Year = report_year, Component, Percent = total) %>%
  mutate(
    Percent = as.numeric(Percent),
    Index = (Percent / Percent[Year == 2010]) * 100, 
    .by = Component
  )


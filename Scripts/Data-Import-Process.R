##%##########################################################################%##
#
# Milking Genetic Potential: Structural Change in the U.S. Dairy Industry
# 
# Alexander S. Hopkins and Tristan M. Hanon
# 
# Filename: Data-Import-Process.R
#
##%##########################################################################%##

# Load Packages
library(mktnewsie)
library(rnassqs)
library(tidyverse)


##### Pull NASS Data #####

# Annual Milk Per Cow and Fat Test Data
annual_nass_data_raw <- nassqs(
  short_desc = c(
    "MILK - PRODUCTION, MEASURED IN LB / HEAD", 
    "MILK - FAT TEST, MEASURED IN PCT"
  ),
  freq_desc = "ANNUAL",
  year__GE = 1980,
  agg_level_desc = "NATIONAL",
  progress_bar = FALSE
)

# Import raw monthly milk per cow data
mpc_data_raw <- nassqs(
  short_desc = "milk - production, measured in lb / head",
  agg_level_desc = "national",
  freq_desc = "monthly",
  year__GE = 1999,
  progress_bar = FALSE
)

# Import raw monthly fat test data
fat_data_raw <- nassqs(
  short_desc = "milk - fat test, measured in pct",
  agg_level_desc = "national",
  freq_desc = "monthly",
  year__GE = 1999,
  progress_bar = FALSE
)


##### Pull Datamart Data #####

fmmo_components_raw <- list("Butterfat", "Protein", "Other Solids") %>%
  map(
    \(x) datamart_pull(3462, section = x, lastReports = NULL) %>% 
      mutate(Component = x)
  ) %>%
  bind_rows()


##### Import Imputed MPC Values #####

mpc_data_assumed <- read.csv(here::here("Data", "Assumed-MPC-Values.csv")) %>%
  mutate(
    date = mdy(date),
    year = as.character(year)
  )


##### Process Data #####

# Process Annual Milk Per Cow and Fat Test Data
annual_mpc_fat_processed <- annual_nass_data_raw %>%
  mutate(
    Year = factor(year, levels = 1980:2025),
    Measure = case_when(
      str_detect(short_desc, "FAT TEST") ~ "Fat_Test",
      str_detect(short_desc, "PRODUCTION") ~ "MPC"
    )
  ) %>%
  select(Year, Measure, Value) %>%
  arrange(Year) %>%
  pivot_wider(names_from = Measure, values_from = Value) %>%
  mutate(
    MPC_Index = (MPC / MPC[Year == 2010]) * 100,
    Fat_Index = (Fat_Test / Fat_Test[Year == 2010]) * 100,
    Fat_Per_Cow = (Fat_Test / 100) * MPC,
    FPC_Index = (Fat_Per_Cow / Fat_Per_Cow[Year == 2010]) * 100,
    MPC_Growth_Rate = (MPC / lag(MPC) - 1) * 100,
    FPC_Growth_Rate = (Fat_Per_Cow / lag(Fat_Per_Cow) - 1) * 100
  )

# Process Annual FMMO Component Data
fmmo_components_processed <- fmmo_components_raw %>%
  filter(is.na(Pool_order_No), report_year < 2026) %>%
  select(Year = report_year, Component, Percent = total) %>%
  mutate(
    Percent = as.numeric(Percent),
    Index = (Percent / Percent[Year == 2010]) * 100, 
    .by = Component
  )

# Clean monthly milk per cow data (and merge in imputed values)
mpc_data <- mpc_data_raw %>%
  filter(begin_code == end_code) %>%
  mutate(
    date0 = ymd(paste0(year, reference_period_desc, "01")),
    date = ceiling_date(date0, unit = "months") - days(1),
    mpc_daily = Value / days_in_month(date)
  ) %>%
  select(date, year, month = reference_period_desc, mpc_daily) %>%
  bind_rows(mpc_data_assumed) %>%
  arrange(date)

# Clean monthly fat data
fat_data <- fat_data_raw %>%
  mutate(
    date0 = ymd(paste0(year, reference_period_desc, "01")),
    date = ceiling_date(date0, unit = "months") - days(1),
    fat_test = Value / 100
  ) %>%
  select(date, year, month = reference_period_desc, fat_test) %>%
  arrange(date)

# Clean protein data
protein_data <- fmmo_components_raw %>%
  filter(is.na(Pool_order_No), Component == "Protein") %>%
  select(year0 = report_year, jan:dec) %>%
  pivot_longer(!year0, names_to = "month0", values_to = "protein_percent") %>%
  mutate(
    date0 = ymd(paste0(year0, month0, "01")),
    date = ceiling_date(date0, unit = "months") - days(1),
    protein_test = as.numeric(protein_percent) / 100,
    month = str_to_upper(month0),
    year = as.character(year0)
  ) %>%
  select(date, year, month, protein_test) %>%
  arrange(date)

# Collect data in one data frame and calculate year-over-year changes
all_data <- mpc_data %>%
  left_join(fat_data, by = c("date", "year", "month")) %>%
  left_join(protein_data, by = c("date", "year", "month")) %>%
  mutate(
    fpc_daily = mpc_daily * fat_test,
    ppc_daily = mpc_daily * protein_test,
    mpc_yy = (mpc_daily / lag(mpc_daily, 12)) - 1,
    fpc_yy = (fpc_daily / lag(fpc_daily, 12)) - 1,
    ppc_yy = (ppc_daily / lag(ppc_daily, 12)) - 1
  ) %>%
  filter_out(date >= ymd(20260701))

# Create data frame of year-over-year changes
change_data <- all_data %>%
  select(date, mpc_yy, fpc_yy, ppc_yy) %>%
  drop_na()


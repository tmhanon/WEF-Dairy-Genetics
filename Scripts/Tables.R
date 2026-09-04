##%##########################################################################%##
#
# Milking Genetic Potential: Structural Change in the U.S. Dairy Industry
# 
# Alexander S. Hopkins and Tristan M. Hanon
# 
# Filename: Tables.R
#
##%##########################################################################%##

# Load Packages
library(gtsummary)
library(tidyverse)


##### Annual Milk and Fat Per Cow Inline Values #####

growth_1980_2025 <- pluck(
  annual_mpc_fat_processed, 
  "MPC", 
  nrow(annual_mpc_fat_processed)
) / 
  pluck(annual_mpc_fat_processed, "MPC", 1) - 1

growth_rate_1981_2010 <- annual_mpc_fat_processed %>%
  filter(Year %in% 1981:2010) %>%
  pluck("MPC_Growth_Rate") %>%
  mean()

growth_rate_2011_2025 <- annual_mpc_fat_processed %>%
  filter(Year %in% 2011:2025) %>%
  pluck("MPC_Growth_Rate") %>%
  mean()

fat_growth_2011_2025 <- annual_mpc_fat_processed %>%
  filter(Year %in% 2011:2025) %>%
  pluck("FPC_Growth_Rate") %>%
  mean()


##### Table 1: Annual Growth Rates #####

growth_rate_table <- annual_mpc_fat_processed %>%
  mutate(
    Year_Group = case_when(
      Year %in% 1981:1990 ~ "1981-1990",
      Year %in% 1991:2000 ~ "1991-2000",
      Year %in% 2001:2010 ~ "2001-2010",
      Year %in% 2011:2020 ~ "2011-2020",
      Year %in% 2021:2025 ~ "2021-2025",
    )
  ) %>%
  filter_out(is.na(MPC_Growth_Rate)) %>%
  tbl_summary(
    by = Year_Group, 
    include = c(MPC_Growth_Rate, FPC_Growth_Rate),
    label = list(
      MPC_Growth_Rate ~ "Milk per Cow", 
      FPC_Growth_Rate ~ "Fat per Cow"
    ),
    statistic = all_continuous() ~ "{mean}"
  ) %>%
  add_overall(
    last = TRUE,
    statistic = ~"{mean}"
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_spanning_header(
    all_stat_cols() ~ "**Average Annual Growth Rate (%)**"
  ) %>%
  remove_footnote_header() %>%
  modify_source_note(
    "Source: USDA-NASS Milk Production and Agricultural Prices"
  )


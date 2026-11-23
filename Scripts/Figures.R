##%##########################################################################%##
#
# Milking Genetic Potential: Structural Change in the U.S. Dairy Industry
# 
# Alexander S. Hopkins and Tristan M. Hanon
# 
# Filename: Figures.R
#
##%##########################################################################%##

##### Setup #####

# Load Packages
library(brand.yml)
library(tidyverse)

# Set default ggplot2 theme:
theme_ams <- function() {
  
  theme_bw(
    base_size = 12,
    base_family = "sans"
  ) +
    theme(
      legend.position = "bottom"
    )
  
}

theme_set(theme_ams())

# Read _brand.yml for colors:
brand <- read_brand_yml("_brand.yml")


# Figure 1: Index of Milk Per Cow and Fat Per Cow

fig_mpc_fat_index <- annual_mpc_fat_processed %>%
  select(Year, MPC_Index, FPC_Index) %>%
  pivot_longer(
    -Year, 
    names_to = "Measure", 
    values_to = "Index", 
    names_pattern = "(.*)_Index"
  ) %>%
  ggplot(aes(x = Year, y = Index, color = Measure, group = Measure)) +
  geom_line(aes(linetype = Measure), key_glyph = "timeseries") +
  scale_x_discrete(breaks = seq(1980, 2025, by = 5)) +
  scale_color_discrete(
    palette = c(brand$color$palette$green, brand$color$palette$blue),
    labels = c("FPC" = "Butterfat per Cow", "MPC" = "Milk per Cow")
  ) +
  scale_linetype_manual(
    values = c("solid", "longdash"),
    labels = c("FPC" = "Butterfat per Cow", "MPC" = "Milk per Cow")
  ) +
  labs(x = NULL, y = "Index (2010 = 100)")


# Figure 2: Index of Milk Component Levels

fig_fmmo_comp_index <- fmmo_components_processed %>%
  mutate(Year = factor(Year, levels = 2000:2025)) %>%
  ggplot(aes(x = Year, y = Index, color = Component, group = Component)) +
  geom_line(aes(linetype = Component), key_glyph = "timeseries") +
  scale_x_discrete(breaks = seq(2000, 2025, by = 5)) +
  scale_color_discrete(
    palette = c(
      brand$color$palette$green,
      brand$color$palette$blue, 
      brand$color$palette$red
    )
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "longdash")
  ) +
  labs(x = NULL, y = "Index (2010 = 100)")


##%##########################################################################%##
# Plot breaks                                                               ####
##%##########################################################################%##

## Create plots of breaks in differenced time series ----

# Create levels
mpc1 <- coef(mpc_bp, breaks = 3)[1]
mpc2 <- coef(mpc_bp, breaks = 3)[2]
mpc3 <- coef(mpc_bp, breaks = 3)[3]
mpc4 <- coef(mpc_bp, breaks = 3)[4]

fpc1 <- coef(fpc_bp, breaks = 2)[1]
fpc2 <- coef(fpc_bp, breaks = 2)[2]
fpc3 <- coef(fpc_bp, breaks = 2)[3]

ppc1 <- coef(ppc_bp, breaks = 0)[1]

# Create data frame
graph_data <- all_data %>%
  filter(date >= "2001-01-01" & date <= "2026-06-30") %>%
  mutate(
    mpc_1 = case_when(date <= "2005-01-31" ~ mpc1),
    mpc_2 = case_when((date >= "2005-02-01" & date <= "2011-01-31") ~ mpc2),
    mpc_3 = case_when((date >= "2011-02-01" & date <= "2021-07-31") ~ mpc3),
    mpc_4 = case_when(date >= "2021-08-01" ~ mpc4),
    fpc_1 = case_when(date <= "2005-01-31" ~ fpc1),
    fpc_2 = case_when((date >= "2005-02-01" & date <= "2016-02-29") ~ fpc2),
    fpc_3 = case_when(date >= "2016-03-01" ~ fpc3),
    ppc_1 = ppc1
  ) 

# Plot of Milk Per Cow Growth Rates and Levels
mpc_changes_plot <- ggplot(data = graph_data, aes(x = date)) +
  geom_line(aes(y = mpc_yy), color = brand$color$palette$darkgray) +
  geom_line(aes(y = mpc_1), color = brand$color$palette$red, linewidth = 1) +
  geom_line(aes(y = mpc_2), color = brand$color$palette$red, linewidth = 1) +
  geom_line(aes(y = mpc_3), color = brand$color$palette$red, linewidth = 1) +
  geom_line(aes(y = mpc_4), color = brand$color$palette$red, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = NULL, y = "Year-over-Year Growth Rate")

# Plot of Butterfat Per Cow Growth Rates and Levels
fpc_changes_plot <- ggplot(data = graph_data, aes(x = date)) +
  geom_line(aes(y = fpc_yy)) +
  geom_line(aes(y = fpc_1), color = brand$color$palette$red, linewidth = 1) +
  geom_line(aes(y = fpc_2), color = brand$color$palette$red, linewidth = 1) +
  geom_line(aes(y = fpc_3), color = brand$color$palette$red, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = NULL, y = "Year-over-Year Growth Rate")

# Plot of Protein Per Cow Growth Rates and Levels
ppc_changes_plot <- ggplot(data = graph_data, aes(x = date)) +
  geom_line(aes(y = ppc_yy)) +
  geom_line(aes(y = ppc_1), color = brand$color$palette$red, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = NULL, y = "Year-over-Year Growth Rate")


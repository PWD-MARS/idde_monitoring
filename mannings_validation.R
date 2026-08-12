#### Script to calculate velocity using Manning's Equation and validate against measured velocities

#### Setup

## Library necessary packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(openxlsx)
library(gridExtra)

## Helper function to calculate hydraulic radius given pipe radius and water level
# Calculation for pipe less than half full
get_rh_lt_half <- function(D, wl) {
  r <- D/2
  h <- wl
  theta <- 2 * acos((r-h)/r)
  A <- (r^2*(theta - sin(theta)))/2
  P = r*theta
  return(A/P)
} 
# Calculation for pipe greater than half full
get_rh_gt_half <- function(D, wl) {
  r <- D/2
  h <- D-wl
  theta <- 2 * acos((r-h)/r)
  A <- pi*r^2 - (r^2*(theta - sin(theta)))/2
  P = 2*pi*r - r*theta
  return(A/P)
} 
# Overall logic
get_rh <- function(D, wl) {
  rh = case_when(
    wl == D/2 ~ D/4,
    (wl > 0 & wl < D/2) ~ get_rh_lt_half(D, wl),
    (wl > D/2 & wl < D) ~ get_rh_gt_half(D, wl),
    .default = NA
  )
  return(rh)
}

## Set parameters
loc <- 'P-091-10'
xl_file_name <- 'P091-10-0010 (Q1-26).xlsm'
xl_sheet_name <- 'Flow Data'
n = 0.015 # Roughness factor for brick sewer
S = 0.069 # Slope (ft/ft)
k = 1.49 # Manning's conversion factor for imperial units
D_ft = 3.5 # Pipe diameter (ft)


## Read in data and do some basic cleaning
mon_data <- read_excel(paste0(loc, '/data/', xl_file_name), 
                       sheet = xl_sheet_name,
                       range = cell_limits(c(14, 1),c(NA, 22)),
                       col_types = c('date', rep('skip', 19), 'numeric', 'numeric'),
                       col_names = c('dtime','wl_in','v_meas_fps')) %>%
  # Convert wl to ft
  mutate(wl_ft = as.numeric(wl_in) / 12) %>%
  # Remove wl_in
  select(-wl_in)
  
## Use Manning's Equation to calculate velocity
mon_data <- mon_data %>%
  mutate(Rh_ft = get_rh(D_ft, wl_ft)) %>%
  mutate(v_calc_fps = k/n*Rh_ft^(2/3)*S^(1/2))

## Generate some comparisons and print them
mon_data <- mon_data %>%
  mutate(v_diff_abs_fps = abs(v_calc_fps - v_meas_fps)) %>%
  mutate(v_diff_abs_rel = case_when(
    v_meas_fps == 0 ~ NA,
    .default = abs(v_diff_abs_fps / v_meas_fps)))

print(fivenum(mon_data$v_diff_abs_fps))
print(fivenum(na.omit(mon_data$v_diff_abs_rel)))

## Plot v_calc - v_meas
v_diff_plot <- 
  ggplot(mon_data, aes(x = dtime, y = v_diff_abs_fps)) +
  geom_point(size = 0.5) + 
  ggtitle('Measured - Calculated Velocities at P-091-10 for 26Q1') +
  ylab("Velocity Difference (feet per second)") +
  xlab("Date (2026)") 
v_diff_plot
ggsave(paste0(loc, '/output/v_diff_plot.png'))

## Reshape data
mon_data <- mon_data %>%
  pivot_longer(cols = c(v_meas_fps, v_calc_fps), names_to = 'v_type', values_to = 'v_fps')

## Plot measured and calculated velocities
v_plot <- 
  ggplot(mon_data, aes(x = dtime, y = v_fps, color = v_type)) +
  geom_point(size = 0.5) + 
  scale_color_discrete(
    labels = c('v_calc_fps' = 'calculated','v_meas_fps' = 'measured')) + 
  ggtitle('Measured and Calculated Velocities at P-091-10 for 26Q1') +
  labs(color = 'Velocity type') + 
  ylab("Velocity (feet per second)") +
  xlab("Date (2026)") + 
  theme(legend.position = 'inside',
        legend.position.inside = c(0.1, 0.9))
v_plot
ggsave(paste0(loc, '/output/v_plot.png'))



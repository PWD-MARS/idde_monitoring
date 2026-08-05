#### Script to calculate velocity using Manning's Equation and validate against measured velocities

#### Setup

# Library necessary packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(gridExtra)

# # Helper function to calculate hydraulic radius given pipe radius and water level

# # Old version with if/else 
# hydr_rad <- function(D, wl) {
#   r <- D/2
#   # Case 1: Pipe less than half full
#   if (wl < r) {
#     h <- wl
#     theta <- 2 * acos((r-h)/(r))
#     A <- (r^2*(theta - sin(theta)))/2
#     P = r*theta
#   }
#   else {
#     h <- D - wl
#     theta <- 2 * acos((r-h)/(r))
#     A <- pi * r^2 - (r^2*(theta - sin(theta)))/2
#     P = 2*pi*r - r*theta
#   }
#   return(A/P)
# }

# New version
get_rh_lt_half <- function(D, wl) {
  r <- D/2
  h <- wl
  theta <- 2 * acos((r-h)/r)
  A <- (r^2*(theta - sin(theta)))/2
  P = r*theta
  return(A/P)
} 

get_rh_gt_half <- function(D, wl) {
  r <- D/2
  h <- D-wl
  theta <- 2 * acos((r-h)/r)
  A <- pi*r^2 - (r^2*(theta - sin(theta)))/2
  P = 2*pi*r - r*theta
  return(A/P)
} 

get_rh <- function(D, wl) {
  rh = case_when(
    between(wl, 0, D/2) ~ get_rh_lt_half(D, wl),
    between(wl, D/2, D) ~ get_rh_gt_half(D, wl)
  )
  return(rh)
}

# Set parameters
loc <- 'P-091-10'
xl_file_name <- 'P091-10-0010 (Q1-26).xlsm'
xl_sheet_name <- 'Flow Data'
n = 0.015 # Roughness factor for brick sewer
S = 0.069 # Slope (ft/ft)
k = 1.49 # Manning's conversion factor for imperial units
D_ft = 3.5 # Pipe diameter (ft)


# Read in data and convert water level from inches to feet
mon_data <- read_excel(paste0(loc, '/data/', xl_file_name), sheet = xl_sheet_name, skip = 11) %>%
  # Remove unit header row
  slice(-1) %>%
  # Select only corrected water level and velocity
  select(wl_in = 'Corrected Level', v_meas_fps = 'Corrected Velocity') %>%
  # Convert wl to ft
  mutate(wl_ft = as.numeric(wl_in) / 12) %>%
  # Remove wl_in
  select(-wl_in)
  
# Use Manning's Equation to calculate velocity
## ***** Assumes pipe is less than half full
mon_data <- mon_data %>%
  mutate(Rh_ft = get_rh(D_ft, wl_ft)) %>%
  mutate(v_calc_fps = k/n*Rh_ft^(2/3)*S^(1/2))

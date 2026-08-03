#### Script to generate timeseries plots from MARS monitoring data collected at S-059-03 Outfall

#### Setup

# Library necessary packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(gridExtra)

# Helper function to determine if datetime is multiple of n minutes
ismult_dt <- function(dt, n) {
  min <- minute(dt)
  return(min %% n == 0)
}

# Set parameters
loc <- 'S-059-03'
start_dt <- ymd_hms('2026-07-10 08:52:00')
xl_file_name <- 'S-059-03/data/QAQC_S-059-03_JB_202607030.xlsx'
xl_sheet_name <- 'OW1_LVL1_Data' 

# Read in data
mon_data <- read_excel(xl_file_name, sheet = xl_sheet_name, skip = 1) %>%
  select(dtime = 'Standard Dtime', wl_ft = 'Water Depth (ft)')

# Discard data from before monitoring began
mon_data <- mon_data %>%
  filter(dtime >= start_dt)

# Plot data as-is
mon_data_1min_plot <- 
  ggplot(mon_data, aes(x = dtime, y = wl_ft)) +
  geom_point(size = 0.5) + 
  ggtitle('1 minute interval') +
  ylab("Water Level (ft)") +
  xlab("Date (2026)") +
  ylim(0, 1) + 
  scale_x_datetime(
    date_breaks = '1 day',
    date_minor_breaks = '6 hours',
    date_labels = "%m-%d"
  ) + 
  theme(plot.title = element_text(size = 11, face = 'bold'),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Plot data at 5-min interval
mon_data_5min_plot <- 
  ggplot(filter(mon_data, ismult_dt(dtime, 5)), aes(x = dtime, y = wl_ft)) +
  geom_point(size = 0.5) + 
  ggtitle('5 minute interval') +
  ylab("Water Level (ft)") +
  xlab("Date (2026)") +
  ylim(0, 1) + 
  scale_x_datetime(
    date_breaks = '1 day',
    date_minor_breaks = '6 hours',
    date_labels = "%m-%d"
  )+ 
  theme(plot.title = element_text(size = 11, face = 'bold'),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Plot data as-is
mon_data_15min_plot <- 
  ggplot(filter(mon_data, ismult_dt(dtime, 15)), aes(x = dtime, y = wl_ft)) +
  geom_point(size = 0.5) + 
  ggtitle('15 minute interval') +
  ylab("Water Level (ft)") +
  xlab("Date (2026)") +
  ylim(0, 1) + 
  scale_x_datetime(
    date_breaks = '1 day',
    date_minor_breaks = '6 hours',
    date_labels = "%m-%d"
  )+ 
  theme(plot.title = element_text(size = 11, face = 'bold'),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))

g <- arrangeGrob(mon_data_1min_plot, mon_data_5min_plot, mon_data_15min_plot, 
                 ncol = 1, 
                 top = paste0('Water Level at ', loc))
ggsave(paste0(loc, '/output/', loc, '_wl_response.png'), g)

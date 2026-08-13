#### Script to generate time and frequency domain plots from storm sewer/outfall monitoring data

#### Setup

## Library necessary packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(gridExtra)

## Helper function to determine if datetime is multiple of n minutes
ismult_dt <- function(dt, n) {
  mins <- minute(dt)
  return(mins %% n == 0)
}

## Set parameters
loc <- 'S-059-03'
start_dt <- ymd_hms('2026-07-10 08:52:00')
xl_file_name <- 'QAQC_S-059-03_JB_202607030.xlsx'
xl_sheet_name <- 'OW1_LVL1_Data' 

## Read in data
mon_data <- read_excel(paste0(loc, '/data/', xl_file_name), sheet = xl_sheet_name, skip = 1) %>%
  select(dtime = 'Standard Dtime', wl_ft = 'Water Depth (ft)')

## Discard data from before monitoring began
mon_data <- mon_data %>%
  filter(dtime >= start_dt)

## Plot data as-is
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

## Plot data at 5-min interval
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

## Plot data at 15-min interval
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


## Do Fourier transform on data

# Determine sampling period, length, and mean wl 
samp_period_min <- as.numeric(difftime(mon_data$dtime[2], mon_data$dtime[1], units = 'mins'))
samp_freq_per_min <- 1/samp_period_min
N <- length(mon_data$dtime) 
mean_wl <- mean(mon_data$wl_ft)

# Create data frame to hold results
fft_data <- data.frame(matrix(ncol = 2, nrow = N))
colnames(fft_data) <- c('freq_per_min', 'amp_ft')

# Calculate frequencies
fft_data$freq_per_min <- (freq_per_min = (0:(N-1))*samp_freq_per_min/N)

# Calculate amplitudes and scale by 1/N
# Note that original data is adjusted to have a mean of 0
fft_data$amp_ft <- Mod(fft(mon_data$wl_ft - mean_wl))/N

# Plot full fft and label max value
ggplot(fft_data, aes(x = freq_per_min, y = amp_ft)) + 
  geom_point() +
  ggtitle(paste0('Fourier Transform of Mean-Adjusted Water Level Data from ', loc)) +
  xlab("Frequency (inverse minutes)") +
  ylab("Amplitude (ft)") +
  # Add dashed line at 24 hr period
  geom_vline(xintercept = 1/24/60, color = 'red', linetype = 'longdash')
ggsave(paste0(loc, '/output/', loc, '_fft_full.png'))

# Replot with only lower frequencies
ggplot(fft_data, aes(x = freq_per_min, y = amp_ft)) + 
  geom_point() +
  ggtitle(paste0('Fourier Transform of Mean-Adjusted Water Level Data from ', loc)) +
  xlim(0, 0.05) + 
  xlab("Frequency (inverse minutes)") +
  ylab("Amplitude (ft)") +
  # Add dashed line at 24 hr period
  geom_vline(xintercept = 1/24/60, color = 'red', linetype = 'longdash')
ggsave(paste0(loc, '/output/', loc, '_fft_low_freq.png'))
  
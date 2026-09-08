#### Script to generate time and frequency domain plots from storm sewer/outfall monitoring data

#### Setup

## Library necessary packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(gridExtra)
library(pwdgsi)
library(pool)
library(ggpubr)

# Create database connection
mars_con <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_prod",
  user = Sys.getenv("mars_uid"),
  password = Sys.getenv("mars_pwd"),
  timezone = NULL
)

## Helper function to determine if datetime is multiple of n minutes
ismult_dt <- function(dt, n) {
  mins <- minute(dt)
  return(mins %% n == 0)
}

## Set parameters
loc <- 'S-059-03'
tz <- 'Etc/GMT+5'
start_dt <- ymd_hms('2026-07-10 09:15:00', tz = tz)
end_dt <- ymd_hms('2026-07-22 11:22:00', tz = tz)
qaqc_file_name <- 'QAQC_S-059-03_JB_202607030.xlsx'
qaqc_sheet_name <- 'OW1_LVL1_Data' 
baro_file_name <- 'S-059-03_OW1_BARO_20285180.csv'
rain_smp_id <- '63654' # Nearby SMP for rainfall script

## Get start and end dates from dts
start_date <- as.Date(start_dt)
end_date <- as.Date(end_dt)

## Read in data, correct timezone, and filter based on start/end times
# Get water level data from QAQC sheet
qaqc_data <- read_excel(paste0(loc, '/data/', qaqc_file_name), sheet = qaqc_sheet_name, skip = 1) %>%
  select(dtime = 'Standard Dtime', temp_water_f = 'Temp BW (°F)', wl_ft = 'Water Depth (ft)') %>%
  # Set time zone to UTC-5
  mutate(dtime = force_tz(dtime, tz = tz)) %>%
  # Discard data from before/after monitoring period
  filter(between(dtime, start_dt, end_dt))
# Get baro temps from baro csv
baro_data <- read_csv(paste0(loc, '/data/', baro_file_name), skip = 1, show_col_types = FALSE) %>%
  select(2,4) %>%
  rename(dtime = 1, temp_air_f = 2) %>%
  mutate(dtime = mdy_hms(dtime, tz = tz))
# Get rain data from db
rain_data <- marsFetchRainfallData(
  mars_con,
  target_id = rain_smp_id,
  start_date = start_date,
  end_date = end_date,
  'gage') %>%
  select(dtime, rainfall_in) %>%
  mutate(dtime = with_tz(dtime, tz = tz))


## Join all data into single df and replace missing rainfall values with 0s
mon_data <- qaqc_data %>%
  left_join(baro_data, by = 'dtime') %>%
  left_join(rain_data, by = 'dtime') %>%
  mutate(rainfall_in = replace_na(rainfall_in, 0))


## Plot data at 1-min interval
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

## Arrange 1-min, 5-min, and 15-min plots together and save
g <- arrangeGrob(mon_data_1min_plot, mon_data_5min_plot, mon_data_15min_plot, 
                 ncol = 1, 
                 top = paste0('Water Level at ', loc))
ggsave(paste0(loc, '/output/', loc, '_wl_response.png'), g)


## Do Fourier transform on data

# Determine sampling period, length, mean wl, and diurnal_freq
samp_period_min <- as.numeric(difftime(mon_data$dtime[2], mon_data$dtime[1], units = 'mins'))
samp_freq_per_min <- 1/samp_period_min
N <- length(mon_data$dtime) 
mean_wl <- mean(mon_data$wl_ft)
target_freq_per_min <- 1/24/60 

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
  geom_vline(xintercept = target_freq_per_min, color = 'red', linetype = 'longdash') + 
  annotate('text', x = target_freq_per_min, y = max(fft_data$amp_ft)*0.7, label = '\nDiurnal Frequency', color = 'red', angle = 90)
 
ggsave(paste0(loc, '/output/', loc, '_fft_full.png'))

# Replot with only lower frequencies
ggplot(fft_data, aes(x = freq_per_min, y = amp_ft)) + 
  geom_point() +
  ggtitle(paste0('Fourier Transform of Mean-Adjusted Water Level Data from ', loc)) +
  xlim(0, 0.02) + 
  xlab("Frequency (inverse minutes)") +
  ylab("Amplitude (ft)") +
  # Add dashed line at 24 hr period
  geom_vline(xintercept = target_freq_per_min, color = 'red', linetype = 'longdash') + 
  annotate('text', x = target_freq_per_min, y = max(fft_data$amp_ft)*0.7, label = '\nDiurnal Frequency', color = 'red', angle = 90) + 
  # Add dashed line at 12 hr period
  geom_vline(xintercept = target_freq_per_min/2, color = 'red', linetype = 'longdash') + 
  annotate('text', x = 0, y = max(fft_data$amp_ft)*0.7, label = '\nSemi-diurnal Frequency', color = 'red', angle = 90)

ggsave(paste0(loc, '/output/', loc, '_fft_low_freq.png'))
 

 
## Filter data to 15-min interval and make combined plot
mon_data <- mon_data %>%
  filter(ismult_dt(dtime, 15))

## Create rainfall plot
rainfall_plot <- ggplot(mon_data, aes(dtime)) +
  geom_col(aes(y = rainfall_in)) +
  labs(y = 'Rainfall (in)',
       title = 'S-059-03 Monitoring Data from July 2026') + 
  scale_x_datetime(
    date_breaks = '1 day',
    date_minor_breaks = '6 hours',
    date_labels = "%m-%d"
  )+ 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

## Create water level plot
wl_plot <- 
  ggplot(mon_data, aes(x = dtime, y = wl_ft)) +
  geom_point(size = 0.5) + 
  labs(y = 'Water Level (ft)') +
  ylim(0, 1) + 
  scale_x_datetime(
    date_breaks = '1 day',
    date_minor_breaks = '6 hours',
    date_labels = "%m-%d"
  )+ 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

## Prep data for temps plot
mon_data_longer <- mon_data %>%
  rename('air' = 'temp_air_f', 'water' = 'temp_water_f') %>%
  pivot_longer(cols = c('air', 'water'), names_to = 'temp_type', values_to = 'temp_f') 

## Create temps plot
temps_plot <- 
  ggplot(mon_data_longer, aes(x = dtime, y = temp_f, color = temp_type)) +
  geom_point(size = 0.5) + 
  labs(color = 'Temperature Type',
       x = 'Date (2026)',
       y = 'Temperature (°F)') + 
  scale_color_discrete(labels = c('Air', 'Water')) + 
  scale_x_datetime(
    date_breaks = '1 day',
    date_minor_breaks = '6 hours',
    date_labels = "%m-%d"
  )+ 
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = 'bottom')

## Create combined plot and save
comp_plot <- ggarrange(
  rainfall_plot,
  wl_plot,
  temps_plot,
  nrow = 3,
  align = 'v',
  legend = "bottom")

ggsave(paste0(loc, '/output/', loc, '_composite_plot.png'), comp_plot)

## Close database connection
poolClose(mars_con)

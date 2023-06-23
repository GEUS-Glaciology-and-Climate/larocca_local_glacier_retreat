# This script was written by Laura Larocca for the publication:
# Larocca et al. (in review). Greenland–wide accelerated retreat of local glaciers in the twenty–first century.

library(tidyverse)
library(lubridate)
library(dplyr)

# MAR data presented in main paper (Figures 1 and 2)
# Decadal mean by RGI (summer temperature, snowfall) and region (SMB)

# Import MAR_MonthlyTemperature data file
# Wide to long format
Temperature_long <- gather(MAR_MonthlyTemperature, Year_Month, Temperature, `1950-1`:`2021-12`) 

# Add separate month and year columns 
Temperature_long$month <- substring(Temperature_long$Year_Month, nchar(Temperature_long$Year_Month)-1+1, nchar(Temperature_long$Year_Month))
Temperature_long$year <- substring(Temperature_long$Year_Month, 1,4)

# Select only summer months 6,7,8 (June; July; and August)
Temp_JJA <- subset(Temperature_long, month %in% c('6','7','8'))

# Compute mean JJA temp by year and RGI ID
JJAtemp_yearmean <- Temp_JJA %>%
  group_by(RGI_ID, year) %>%
  summarise(JJAtemp_yearly = mean(Temperature)) %>%
  glimpse()

# Remove years 2020 and 2021
JJAtemp_yearmean <- JJAtemp_yearmean[!(JJAtemp_yearmean$year=="2020" | JJAtemp_yearmean$year=="2021"),]

# Add decade column 
decade <- rep(c("1950-59", "1960-69", "1970-79", "1980-89", "1990-99", "2000-09", "2010-19"), each=10)
decade <- data.frame(decade)
decade <- decade %>% 
  slice(rep(1:n(), 977))
JJAtemp_yearmean <- cbind(JJAtemp_yearmean, decade)

# Compute decadal mean JJA temperature by RGI ID
JJAtemp_decademean <- JJAtemp_yearmean %>%
  group_by(RGI_ID, decade) %>%
  summarise(JJAdecade_mean = mean(JJAtemp_yearly)) %>%
  glimpse()

# Compute baseline (JJA temp btw 1971-2000) by RGI ID
subset_1971_2000 <- subset(JJAtemp_yearmean, year > "1970" & year < "2001")
JJAmean1971_2000 <- subset_1971_2000 %>%
  group_by(RGI_ID) %>%
  summarise(JJAmean1971_2000 = mean(JJAtemp_yearly)) %>%
  glimpse()

#replicate values for each decade (x7)
JJAmean1971_2000 <- JJAmean1971_2000[rep(seq_len(nrow(JJAmean1971_2000)), each = 7), ]

# Add JJAmean1971_2000 to JJAtemp_decademean by RGI ID
JJAtemp_decademean$JJAmean1971_2000 <- JJAmean1971_2000$JJAmean1971_2000

# Compute decadal anomaly [baseline: 1971-2000]
JJAtemp_decademean$anom_1971_2000 <- (JJAtemp_decademean$JJAdecade_mean - JJAtemp_decademean$JJAmean1971_2000)

# Remove JJAmean1971_2000 column 
JJAtemp_decademean = subset(JJAtemp_decademean, select = -c(JJAmean1971_2000) )

# Add lat, lon locations and rename columns. Result is the data file, MAR_SummerTemperature
MAR_SummerTemperature <- merge(JJAtemp_decademean, MAR_MonthlyTemperature[, 1:4], by = "RGI_ID")
colnames(MAR_SummerTemperature)[colnames(MAR_SummerTemperature) == "JJAdecade_mean"] <- "JJA_Temp"
colnames(MAR_SummerTemperature)[colnames(MAR_SummerTemperature) == "anom_1971_2000"] <- "JJA_Temp_anomaly"
colnames(MAR_SummerTemperature)[colnames(MAR_SummerTemperature) == "decade"] <- "DECADE"
custom_order <- c("RGI_ID", "REGION", "LAT", "LON", "DECADE",  "JJA_Temp", "JJA_Temp_anomaly")
MAR_SummerTemperature <- MAR_SummerTemperature[, match(custom_order, names(MAR_SummerTemperature))]

###

# Import MAR_MonthlySF data file
# Wide to long format
SF_long <- gather(MAR_MonthlySnowfall, Year_month, Snowfall, `1950-1`:`2021-12`) 

# Add month and year columns
SF_long$month <- substring(SF_long$Year_month, nchar(SF_long$Year_month)-1+1, nchar(SF_long$Year_month))
SF_long$year <- substring(SF_long$Year_month, 1,4)

# Compute SF sum by year and RGI ID
SF_yearsum <- SF_long %>%
  group_by(RGI_ID, year) %>%
  summarise(SFyear_sum = sum(Snowfall)) %>%
  glimpse()

# Remove years 2020 and 2021
SF_yearsum <- SF_yearsum[!(SF_yearsum$year=="2020" | SF_yearsum$year=="2021"),]

# Add decade column
decade <- rep(c("1950-59", "1960-69", "1970-79", "1980-89", "1990-99", "2000-09", "2010-19"), each=10)
decade <- data.frame(decade)
decade <- decade %>% 
  slice(rep(1:n(), 977))
SF_yearsum <- cbind(SF_yearsum, decade)

# Compute decadal mean SF sum by RGI ID
SF_decademean <- SF_yearsum %>%
  group_by(RGI_ID, decade) %>%
  summarise(SFdecade_mean = mean(SFyear_sum)) %>%
  glimpse()

# Compute baseline (SF btw 1971-2000) by RGI ID
subset_1971_2000 <- subset(SF_yearsum, year > "1970" & year < "2001")
SFmean1971_2000 <- subset_1971_2000 %>%
  group_by(RGI_ID) %>%
  summarise(SFmean1971_2000 = mean(SFyear_sum)) %>%
  glimpse()

# Replicate values for each decade (x7)
SFmean1971_2000 <- SFmean1971_2000[rep(seq_len(nrow(SFmean1971_2000)), each = 7), ]

# Add SFmean1971_2000 to SF_decademean by RGI ID
SF_decademean$SFmean1971_2000 <- SFmean1971_2000$SFmean1971_2000

# Compute decadal anomaly [baseline: 1971-2000]
SF_decademean$anom_1971_2000 <- (SF_decademean$SFdecade_mean - SF_decademean$SFmean1971_2000)

# Remove SFmean1971_2000 column
SF_decademean = subset(SF_decademean, select = -c(SFmean1971_2000) )

# Add lat, lon locations and rename columns. Result is the data file, MAR_Snowfall
MAR_Snowfall <- merge(SF_decademean, MAR_MonthlySnowfall[, 1:4], by = "RGI_ID")
colnames(MAR_Snowfall)[colnames(MAR_Snowfall) == "SFdecade_mean"] <- "SF"
colnames(MAR_Snowfall)[colnames(MAR_Snowfall) == "anom_1971_2000"] <- "SF_anomaly"
colnames(MAR_Snowfall)[colnames(MAR_Snowfall) == "decade"] <- "DECADE"
custom_order <- c("RGI_ID", "REGION", "LAT", "LON", "DECADE",  "SF", "SF_anomaly")
MAR_Snowfall <- MAR_Snowfall[, match(custom_order, names(MAR_Snowfall))]

###

# Import MAR_MonthlySMB data file
# Wide to long format
SMB_long <- gather(MAR_MonthlySMB, Year_month, SMB, `1950-1`:`2021-12`) 

# Add month and year columns 
SMB_long$month <- substring(SMB_long$Year_month, nchar(SMB_long$Year_month)-1+1, nchar(SMB_long$Year_month))
SMB_long$year <- substring(SMB_long$Year_month, 1,4)

# Compute SMB sum by year and region
SMB_yearsum <- SMB_long %>%
  group_by(REGION, year) %>%
  summarise(SMByear_sum = sum(SMB)) %>%
  glimpse()

# Remove years 2020 and 2021
SMB_yearsum <- SMB_yearsum[!(SMB_yearsum$year=="2020" | SMB_yearsum$year=="2021"),]

# Add decade column
decade <- rep(c("1950-59", "1960-69", "1970-79", "1980-89", "1990-99", "2000-09", "2010-19"), each=10)
decade <- data.frame(decade)
decade <- decade %>% 
  slice(rep(1:n(), 8))
SMB_yearsum <- cbind(SMB_yearsum, decade)

# Compute decadal mean SMB sum by region
SMB_decademean <- SMB_yearsum %>%
  group_by(REGION, decade) %>%
  summarise(SMBdecade_mean = mean(SMByear_sum)) %>%
  glimpse()

# Compute baseline (SMB btw 1971-2000) by region
subset_1971_2000 <- subset(SMB_yearsum, year > "1970" & year < "2001")
SMBmean1971_2000 <- subset_1971_2000 %>%
  group_by(REGION) %>%
  summarise(SMBmean1971_2000 = mean(SMByear_sum)) %>%
  glimpse()

# Replicate values for each decade (x7)
SMBmean1971_2000 <- SMBmean1971_2000[rep(seq_len(nrow(SMBmean1971_2000)), each = 7), ]

# Add SMBmean1971_2000 to SMB_decademean by region
SMB_decademean$SMBmean1971_2000 <- SMBmean1971_2000$SMBmean1971_2000

# Compute decadal anomaly [baseline: 1971-2000]
SMB_decademean$anom_1971_2000 <- (SMB_decademean$SMBdecade_mean - SMB_decademean$SMBmean1971_2000)

# Remove SMBmean1971_2000 column
SMB_decademean = subset(SMB_decademean, select = -c(SMBmean1971_2000) )

# Rename columns. Result is the data file, MAR_SMB
colnames(SMB_decademean)[colnames(SMB_decademean) == "SMBdecade_mean"] <- "SMB"
colnames(SMB_decademean)[colnames(SMB_decademean) == "anom_1971_2000"] <- "SMB_anomaly"
colnames(SMB_decademean)[colnames(SMB_decademean) == "decade"] <- "DECADE"
MAR_SMB <- SMB_decademean

###

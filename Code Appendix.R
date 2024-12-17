# Code Appendix

ocean_data <- read.csv("~/Desktop/ENVST_FINAL/AN_OCEAN_2007-2024.csv") 

odata <- ocean_data[-(1:3),] # Dropping Data descriptions in first rows

# Loading libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(stargazer)
##### Odata Cleaning
# Parsing Date
odata$dateF <- dmy_hms(odata$Time..UTC.)

# Creating daily / monthly data

odata$doy <- yday(odata$dateF)
odata$month <- month(odata$dateF)
odata$year <- year(odata$dateF)

odata$Salinity <- as.numeric(odata$Salinity)
odata$Chlorophyll <- as.numeric(odata$Chlorophyll)
odata$Temperature <- as.numeric(odata$Temperature)
odata$Turbidity <- as.numeric(odata$Turbidity)
odata$Oxygen <- as.numeric(odata$Oxygen)

odata <- odata %>% rename(Sigwave_height = Significant.wave.height) 

odata$Sigwave_height <- as.numeric(odata$Sigwave_height)

# Putting data in daily format by taking the mean of all the observations in one day
# also grouping by year, month, and doy
daily_odata <- 
  odata %>% 
  group_by(year,month,doy) %>% 
  summarise(daily_salinity = mean(Salinity, na.rm = TRUE),
            daily_chlorophyll = mean(Chlorophyll, na.rm = TRUE),
            daily_temperature = mean(Temperature, na.rm = TRUE),
            daily_turbidity = mean(Turbidity, na.rm = TRUE),
            daily_oxygen = mean(Oxygen, na.rm = TRUE),
            daily_Sigwave = mean(Sigwave_height, na.rm = TRUE),
            n_salinity = sum(!is.na(Salinity)), 
            n_chlorophyll = sum(!is.na(Chlorophyll)),
            n_temperature = sum(!is.na(Temperature)),
            n_turbidity = sum(!is.na(Turbidity)), 
            n_oxygen = sum(!is.na(Oxygen)),
            n_Sigwave = sum(!is.na(Sigwave_height)))

# Filtering for observations where there are at least 15 hours of data
daily_odata <- daily_odata %>% filter(n_salinity >= 15,
                                      n_chlorophyll >= 15,
                                      n_temperature >= 15,
                                      n_turbidity >= 15,
                                      n_oxygen >= 15,
                                      n_Sigwave >= 15)

# Formatting date
daily_odata$date <- ymd(paste0(daily_odata$year, "-01-01")) + days(daily_odata$doy - 1)


#Making data frame specifically for graphing
all_dates <- tibble(
  date = seq(
    from = min(daily_odata$date, na.rm = TRUE), 
    to = max(daily_odata$date, na.rm = TRUE), 
    by = "day"
  )
)


graphing_odata <- full_join(all_dates, daily_odata, by = "date")

# Chlorophyll Plot 
ggplot(graphing_odata, aes(x = date, y = daily_chlorophyll)) + geom_line() + ggtitle("Line Graph of Chlorophyll Levels from 2010 to 2022") + labs(x = "Date", y = "Daily Chlorophyll Levels (ug/L)")

# Time Series Decomp for Chlorophyll
chlorophyll_ts <- ts(daily_odata$daily_chlorophyll, start = c(2010, 1), frequency = 365)

chlorophyll_decomp <- stl(chlorophyll_ts, s.window = "periodic")

plot(chlorophyll_decomp, main = "Chlorophyll Time Series Decomposition")

# Plots of Independent Variables
ggplot(graphing_odata, aes(x = date, y = daily_temperature)) + geom_line() + ggtitle("Line Graph of Daily Temperature from 2010 to 2022") + labs(x = "Date", y = "Daily Temperature in Celcius")

ggplot(graphing_odata, aes(x = date, y = daily_salinity)) + geom_line() + ggtitle("Line Graph of Water Salinity from 2010 to 2022") + labs(x = "Date", y = "Daily Salinity Levels PSU")

ggplot(graphing_odata, aes(x = date, y = daily_turbidity)) + geom_line() + ggtitle("Line Graph of Turbidity Levels from 2010 to 2022") + labs(x = "Date", y = "Daily Turbidity Levels NTU")

ggplot(graphing_odata, aes(x = date, y = daily_oxygen)) + geom_line() + ggtitle("Line Graph of Oxygen Levels from 2010 to 2022") + labs(x = "Date", y = "Daily Oxygen Levels ml/L")


# Creating AR4 Model for Chlorophyll - Recall time series creation above 

#chlorophyll_ts <- ts(daily_odata$daily_chlorophyll, start = c(2010, 1), frequency = 365)

#chlorophyll_decomp <- stl(chlorophyll_ts, s.window = "periodic")

ar4_model <- arima(chlorophyll_ts, order = c(4, 0, 0)) 
stargazer(ar4_model, type = 'latex', title = "AR4", out = "ar4.latex")

plot(residuals(ar4_model), main = "Residuals of AR(4) Model", ylab = "Residuals")

acf(residuals(ar4_model), main = "ACF of Residuals")

# Creating ARX Model
oxygen_ts <- ts(daily_odata$daily_oxygen, frequency = 365, start = c(2010,1))
turbidity_ts <- ts(daily_odata$daily_turbidity, frequency = 365, start = c(2010,1))
salinity_ts <- ts(daily_odata$daily_salinity, frequency = 365, start = c(2010,1))
temperature_ts <- ts(daily_odata$daily_temperature, frequency = 365, start = c(2010,1))

lagged_oxygen <- stats::lag(oxygen_ts,1)
lagged_turbidity <- stats::lag(turbidity_ts,1)
lagged_salinity <- stats::lag(salinity_ts,1)
lagged_temperature <- stats::lag(temperature_ts,1)

xreg <- cbind(lagged_oxygen,lagged_turbidity,lagged_salinity,lagged_temperature)

arx_model <- arima(chlorophyll_ts, order = c(1,0,0),xreg = xreg)

stargazer(arx_model, type = 'latex', title = "1-Period Lag ARX Model", out = "arx1.latex")


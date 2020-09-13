library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)
data <- read.csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
data <- pivot_longer(data, cols = seq(5, ncol(data), 1), names_to = "Date")
data$Date <- str_replace_all(data$Date, "X", "0")
data$Date <- as.Date(data$Date, format = "%m.%d.%y")
data$Province.State <- as.factor(data$Province.State)
data$Country.Region <- as.factor(data$Country.Region)
data_worldwide <- data %>% group_by(Date) %>% summarize(value = sum(value))
data_worldwide$day <- seq(1, nrow(data_worldwide), 1)

ggplot(data = data_worldwide, aes(x = Date, y = value)) + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%d") + xlab("") + 
  ggtitle("COVID19 confirmed worldwide") + theme_bw() 


fit <- nls(value ~ SSlogis(day, Asym, xmid, scale), data = data_worldwide)
kibble(summary(fit))


last_date <- as.Date("31.12.20", format = "%d.%m.%y")
days <- seq(1, as.numeric(last_date - data_worldwide$Date[1]) + 1, 1)
data_worldwide$day <- days
data_predicted <- tibble(day = days, 
                         Date = seq(data_worldwide$Date[1], last_date, 1), 
                       predicted_value = predict(fit, newdata = data.frame(day = days)))
data_worldwide <- full_join(data_predicted, data_worldwide, by = c("day" = "day", "Date" = "Date"))
View(pivot_longer(data_worldwide, c(2, 3)))

ggplot(data = data_worldwide, aes(x = Date, y = value)) + geom_line(aes(col = "Real data"), size = 1.2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%d",
               limits = c(data_worldwide$Date[1], last_date)) + xlab("") + 
  ylab("Number of cases") +
  ggtitle("COVID19 time series") +
  geom_line(aes(x = Date, y = predicted_value, col = "Predicted data"), size = 1.4, linetype = "dashed") +
  theme_clean()


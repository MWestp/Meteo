library(readr)
OpenCampusSales <- read_delim("C:/Users/westp/OneDrive/Desktop/Data_Science/Meteolytix/OpenCampusSales.csv", 
                              ";", escape_double = FALSE, locale = locale(date_names = "de", 
                                                                          decimal_mark = ",", grouping_mark = "."), 
                              trim_ws = TRUE)



OpenCampusWetter <- read_delim("C:/Users/westp/OneDrive/Desktop/Data_Science/Meteolytix/OpenCampusWetter.csv", 
                               ";", escape_double = FALSE, locale = locale(date_names = "de", 
                                                                           decimal_mark = ",", grouping_mark = "."), 
                               trim_ws = TRUE)

data_compl <- merge(OpenCampusWetter, OpenCampusSales, by = "Date")

data_compl$day <- weekdays(as.Date(data_compl$Date))


mod <- lm(Sales ~ day + ttav + nnav, data_compl)

summary(mod)


library(dplyr)
library(lubridate)

data_m <- data_compl %>%
  mutate(month = month (Date)) %>%
  group_by(month) %>%
  summarise(total = sum(Sales))

mod_m <- lm(total ~ month, data_m)

summary(mod_m)


###############

data_m_y <- data_compl %>%
  mutate(month = floor_date(Date, unit = "month"), year = floor_date(Date, unit = "year")) %>% #neue Spalte "month" & "year" werden angelegt
  group_by(month, year) %>% # gruppieren nach Monat und Jahr
  summarise(total = sum(Sales))

data_m_y$month_name <- month(as.Date(data_m_y$month))

mod_m <- lm(total ~ month_name, data_m_y)

summary(mod_m)

################


data_m_y$year_name <- year(as.Date(data_m_y$year))

mod_m <- lm(total ~ year_name, data_m_y)

summary(mod_m)












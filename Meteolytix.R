hallo

setwd("C:/Users/westp/OneDrive/Desktop/Data_Science/Meteolytix/")

# Dateien einlesen

library(readr)


OpenCampusSales <- read_delim("OpenCampusSales.csv", 
                              ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                          grouping_mark = "."), trim_ws = TRUE)

OpenCampusWetter <- read_delim("OpenCampusWetter.csv", 
                               ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                           grouping_mark = "."), trim_ws = TRUE)





# Beide Dateien verknüpfen anhand des Datums
data_compl <- merge(OpenCampusWetter, OpenCampusSales, by = "Date")

#Unwichtiger Kram
#library(DataExplorer)
#create_report(data_compl)

library(lubridate) # Für die Datums Manipulation
library(dplyr) # Zur Tabellen Manipulation
library(ggplot2) # Für die Graphische Darstellung



data_m_y <- data_compl %>%
  mutate(month = floor_date(Date, unit = "month"), year = floor_date(Date, unit = "year")) %>% #neue Spalte "month" & "year" werden angelegt
  group_by(month, year) %>% # gruppieren nach Monat und Jahr
  summarise(total = sum(Sales)) # Die Verkäufe beim Gruppieren summieren und Spalte "total" bilden

ggplot(data_m_y, aes(x=month, y=total)) + geom_point() # Summe der Verkäufe je Monat wird im Punktdiagramm dargestellt


data_m <- data_compl %>%
  mutate(month = floor_date(Date, unit = "month")) %>%
  group_by(month) %>%
  summarise(total = sum(Sales))

ggplot(data_m, aes(x=month, y=total)) + geom_point()


data_y <- data_compl %>%
  mutate(year = floor_date(Date, unit = "year")) %>%
  group_by(year) %>%
  summarise(total = sum(Sales))

ggplot(data_y, aes(x=year, y=total)) + geom_point()



git remote add origin https://github.com/MWestp/Data-Science.git
git config remote.origin.url git@github.com:MWestp/Data-Science.git
git pull -u origin master
git push -u origin master


install.packages("esquisse")
library(esquisse)
esquisser()


my_sum <- data_compl %>%
  mutate(year = floor_date(Date, unit = "year")) %>%
  group_by(year) %>%
  summarise( 
    n=n(),
    mean=mean(Sales),
    sd=sd(Sales)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


ggplot(my_sum) +
  geom_bar( aes(x=year, y=mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar( aes(x=year, ymin=mean-sd, ymax=mean+sd), width=100, colour="orange", alpha=0.9, size=1) +
  ggtitle("barplot mean of sales per year with standard deviation")

ggplot(data = dat) +
  aes(x = year, y = mean) +
  geom_line(color = "#0c4c8a") +
  theme_minimal()


#############################

library(dplyr)

data(mtcars)

tibble(mtcars)

mtcars <- mpg %>%
  group_by(cyl) %>%
  summarise(n(), t.test(cty,hwy)$p.value)

economics %>%
  mutate(weekday=wday(date))


data_m <- data_compl %>%
  mutate(month = month (Date)) %>%
  group_by(month) %>%
  summarise(total = sum(Sales))

ggplot(data_m, aes(x=month, y=total)) + geom_point() # Summe der Verkäufe je Monat (alle Jahre aggregiert) wird im Punktdiagramm dargestellt


ggplot(data = data_m) +
  aes(x = month, y = total) +
  geom_line(color = "#ef562d") +
  labs(title = "Aggregierte Verkäufe pro Monat",
       x = "Monate",
       y = "Euro") +
  theme_grey()



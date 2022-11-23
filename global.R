# loading packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(dashboardthemes)
library(shinycssloaders)
library(readr)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(leaflet)
library(plotly)

# Data preprocessing
# reading the data from github repo
global_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

global_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

global_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

shapes <- read_sf("shape files")

# converting data from wide to long
global_confirmed <- global_confirmed %>% gather(key = "Date", value = "Confirmed", -names(global_confirmed)[1:4])
global_recovered <- global_recovered %>% gather(key = "Date", value = "Recovered", -names(global_recovered)[1:4])
global_deaths <- global_deaths %>% gather(key = "Date", value = "Deaths", -names(global_deaths)[1:4])

# merging the data
merged_data <- full_join(global_confirmed, global_deaths, by = c("Province/State","Country/Region","Lat","Long","Date"))

merged_data <- full_join(merged_data, global_recovered, by = c("Province/State","Country/Region","Lat","Long","Date"))

# parsing the dates
merged_data$Date <- as.Date(merged_data$Date, format = "%m/%d/%y")

# dropping cruise ship data
index <- which(merged_data[,2] == "Grand Princess" | merged_data[,2] == "Diamond Princess" | merged_data[,2] == "MS Zaandam")
index <- c(index, which(merged_data[,1] == "Grand Princess" | merged_data[,1] == "Diamond Princess" | merged_data[,1] == "MS Zaandam"))

merged_data <- merged_data[-index,]

# cleaning missing values
merged_data$Confirmed[is.na(merged_data$Confirmed)] <- 0
merged_data$Deaths[is.na(merged_data$Deaths)] <- 0
merged_data$Recovered[is.na(merged_data$Recovered)] <- 0

# creating column for active cases
merged_data$active <- merged_data$Confirmed - merged_data$Deaths - merged_data$Recovered

# aggregating data by Country and Date
merged_data <- merged_data %>%
  group_by(`Country/Region`, Date) %>%
  summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered), Active = sum(active))

# creating new cases, new deaths and new recovered
merged_data <- merged_data %>% group_by(`Country/Region`) %>%
  mutate(New_cases= Confirmed - lag(Confirmed),
         New_deaths = Deaths - lag(Deaths),
         New_recovered = Recovered - lag(Recovered))
index <- which(is.na(merged_data$New_cases))
merged_data$New_cases[index] <- merged_data$Confirmed[index]
merged_data$New_deaths[index] <- merged_data$Deaths[index]
merged_data$New_recovered[index] <- merged_data$Recovered[index]

total_recovered <- merged_data %>% group_by(`Country/Region`) %>% filter(Recovered == max(Recovered)) %>% select(`Country/Region`, Recovered) %>% distinct() %>% ungroup() %>% summarise(total_recovered = sum(Recovered))

# Modifying country names so they all match up
merged_data[merged_data$`Country/Region`=="Korea, South",1] <- "South Korea"
merged_data[merged_data$`Country/Region`=="Burma",1] <- "Myanmar"
merged_data[merged_data$`Country/Region`=="Serbia",1] <- "Republic of Serbia"
merged_data[merged_data$`Country/Region`=="Taiwan*",1] <- "Taiwan"
merged_data[merged_data$`Country/Region`=="Timor-Leste",1] <- "East Timor"
merged_data[merged_data$`Country/Region`=="Bahamas",1] <- "The Bahamas"
merged_data[merged_data$`Country/Region`=="Congo (Brazzaville)",1] <- "Republic of the Congo"
merged_data[merged_data$`Country/Region`=="Congo (Kinshasa)",1] <- "Democratic Republic of the Congo"
merged_data[merged_data$`Country/Region`=="Cote d'Ivoire",1] <- "Ivory Coast"
merged_data[merged_data$`Country/Region`=="Eswatini",1] <- "eSwatini"
merged_data[merged_data$`Country/Region`=="Sao Tome and Principe",1] <- "São Tomé and Principe"
merged_data[merged_data$`Country/Region`=="Serbia",1] <- "Republic of Serbia"
merged_data[merged_data$`Country/Region`=="Tanzania",1] <- "United Republic of Tanzania"
merged_data[merged_data$`Country/Region`=="US",1] <- "United States of America"

shapes <- left_join(shapes, merged_data[merged_data$Date==max(merged_data$Date),], by=c("SOVEREIGNT"="Country/Region"))
shapes <- shapes %>% mutate(pop_ups = paste("<b>", SOVEREIGNT, "</b>", "<br>", "Date:", Date, "<br>", "Active Cases: ", Active, "<br>", "Total Confirmed :", Confirmed, "<br>", "Deaths :", Deaths, "<br>", "Recovered :", Recovered, "<br>", "New Cases :", New_cases, "<br>", "New Deaths :", New_deaths, "<br>", "New Recovered :", New_recovered, sep = ""))

# visual elements of the app
covid_live <- shinyDashboardLogoDIY(
  boldText = "COVID-19",
  mainText = "",
  textSize = 16,
  badgeText = "LIVE",
  badgeTextColor = "white",
  badgeTextSize = 2,
  badgeBackColor = "red",
  badgeBorderRadius = 3
)

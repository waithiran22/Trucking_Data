library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

rm(list=ls())
#setting out working directory
setwd('C:/Users/HP/OneDrive/Documents/DATA 332/Excel Files')

df_truck <- read_excel('~/DATA 332/Excel Files/NP_EX_1-2.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')

#selecting columns
df <- df_truck[,c(4:15)]

#deselect columns
df <- subset( df, select = -c(...10))

#getting difference in days within columns
date_min <- min(df$Date)
date_max <- max(df$Date)

number_of_days_on_the_road <- date_max - date_min
print(number_of_days_on_the_road)

days <- difftime(max(df$Date), min(df$Date))
print(days)
number_of_days_recorded <- nrow(df)

total_hours <- sum(df$Hours)
avg_hrs_per_day_rec <- round(total_hours / number_of_days_recorded, digits = 3)
print(avg_hrs_per_day_rec)

df$fuel_cost <- df$Gallons * df$Price.per.Gallon
df$other_expenses <- df$Misc + df$Tolls
df$total_expenses <- df$fuel_cost + df$other_expenses
df$total_gallons <- sum(df$Gallons)
df$total_miles <- sum(df$Miles)
df$miles_per_gallon <- df$total_miles / df$total_gallons
df$cost_per_mile <- df$total_expenses / df$total_miles
df[c('warehouse', 'starting_city_state')] <-
  str_split_fixed(df$Starting.Location, ',', 2)

df$starting_city_state <- gsub(',', "", df$starting_city_state)

# df[c('col1', 'col2')] <-
#   str_split_fixed(df$city_state, ' ', 2)

df_starting_pivot <- df %>%
  group_by(starting_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_starting_pivot, aes(x = starting_city_state, y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))

df[c('Delivery_City', 'Delivery_State')] <- str_split_fixed(df$Delivery.Location, ',', 2)
df$Delivery_State <- gsub(' ', "", df$Delivery_State)

df_delivery_pivot <- df %>%
  group_by(Delivery_State) %>%
  summarize(
    count = n(),
    mean_size_hours = mean(Hours, na.rm = TRUE),
    sd_hours = sd(Hours, na.rm = TRUE),
    total_hours = sum(Hours, na.rm = TRUE),
    total_gallons = sum(Gallons, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(df_delivery_pivot, aes(x = Delivery_State, y = count)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))





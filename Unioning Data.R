library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

rm(list=ls())
#setting out working directory
setwd('C:/Users/HP/OneDrive/Documents/DATA 332/Excel Files')

df_truck_0001 <- read_excel('~/DATA 332/Excel Files/truck data 0001.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_0369 <- read_excel('~/DATA 332/Excel Files/truck data 0369.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1226 <- read_excel('~/DATA 332/Excel Files/truck data 1226.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1442 <- read_excel('~/DATA 332/Excel Files/truck data 1442.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1478 <- read_excel('~/DATA 332/Excel Files/truck data 1478.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1539 <- read_excel('~/DATA 332/Excel Files/truck data 1539.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1769 <- read_excel('~/DATA 332/Excel Files/truck data 1769.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_pay <- read_excel('~/DATA 332/Excel Files/Driver Pay Sheet.xlsx', .name_repair = 'universal' )
df <- rbind(df_truck_0001, df_truck_0369, df_truck_1226, df_truck_1442, df_truck_1478, df_truck_1539, df_truck_1769)

df_starting_Pivot <- df %>%
  group_by(Truck.ID) %>%
  summarize(count = n())

df <- left_join(df, df_pay, by = c('Truck.ID'))

df$fuel_cost <- df$Gallons * df$Price.per.Gallon
df$other_expenses <- df$Misc + df$Tolls
df$total_expenses <- df$fuel_cost + df$other_expenses

df$total_miles_driven<- (df$Odometer.Ending -  df$Odometer.Beginning)
df$Wages_per_driver<- (df$total_miles_driven * df$labor_per_mil)


df_Wages_per_driver <- df %>%
  group_by(Truck.ID) %>%
  summarize(count = n(),
            salary_by_driver= sum(Wages_per_driver, na.rm = TRUE)
  )

ggplot(df_Wages_per_driver, aes(x = Truck.ID, y = salary_by_driver)) +
  geom_col(color= "black")+ 
  scale_fill_manual(values = c("red", "green", "blue", "yellow", "orange", "purple", "pink", fill=Truck.ID)) +
    labs(title = "Wages per Driver", x = "Truck_ID", y = "Salary") +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))
  


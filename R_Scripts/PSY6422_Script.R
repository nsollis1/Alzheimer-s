# PSY6422 Mini Project Assessment
# This project focuses on the worldwide death rate caused by Alzheimer's disease

knitr::include_graphics("~/My_project/Assessment/PSY6422_Assignment/Alzheimer-s/R_Markdowns/image.jpg")

# Research Question: How does Alzheimer's disease mortality rate in the world's 
# three most populated countries (India, China, United States of America)
# compare across the 2000–2021 period?

# First you need to install a variety of packages and load them into the console
library(tidyverse)
library(here)
library(ggplot2)
library(readr)
library(scatterplot3d)
library(plotly)

# Next you need to import the raw dataset into R and give it a more convenient name
death_rate <- read_csv("R_Markdowns/Raw_Data/death_rate.csv")

# Summary of the data table
summary(death_rate)

# Names of the columns
names(death_rate)

# Look at the first few rows of raw data
head(death_rate, 10)


# Define the countries and the global entity you want to keep
target_countries <- c("India", "China", "United States", "Global")

# Rename the long column name (I renamed mine Mortality_rate)
death_rate <- death_rate %>%
  rename(Mortality_rate = `Death rate from alzheimer disease and other dementias among both sexes`)

# Look at the new names
names(death_rate)

#Filter the death_rate dataset to include only the target countries and the years 2000 to 2021
# This assumes your 'Country' column holds the name of the entity and your 'Year' column holds the year. Adjust names if needed based on names(death_rate).
filtered_data <- death_rate %>%
  filter(Entity %in% target_countries) %>%
  filter(Year >= 2000 & Year <= 2021)

filtered_data <- na.omit(filtered_data) 

head(filtered_data)
summary(filtered_data)

ggplot(filtered_data, aes(x = Year, y = Mortality_rate, colour = Entity)) +
  geom_line(linewidth = 2) +  # Thicker lines
  theme_minimal() +
  labs(
    title = "Alzheimer’s Mortality Rate (2000–2021)",
    x = "Year",
    y = "Deaths per 100,000",
    colour = "Country"
  ) +
  scale_colour_manual(values = c(
    "India" = "lightpink",
    "China" = "purple",
    "United States" = "lightblue"
  )) +
  theme(
    text = element_text(size = 14)  # makes labels easier to read
  )

plot_ly(filtered_data, 
        x = ~Year, 
        y = ~Mortality_rate, 
        color = ~Entity, 
        colors = c("India" = "lightpink",
                   "China" = "purple",
                   "United States" = "lightblue"),
        type = 'scatter', 
        mode = 'lines+markers') %>%
  layout(
    title = "Alzheimer’s Mortality Rate (2000–2021)",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Deaths per 100,000"),
    legend = list(title = list(text='Country'))
  )

plot_ly(filtered_data, 
        x = ~Entity, 
        y = ~Mortality_rate, 
        color = ~Entity, 
        colors = c("India" = "lightpink",
                   "China" = "purple",
                   "United States" = "lightblue"),
        type = "box") %>%
  layout(
    title = "Distribution of Mortality Rates (2000–2021)",
    yaxis = list(title = "Deaths per 100,000")
  )


p <- ggplot(filtered_data, aes(x = Year, y = Entity, fill = Mortality_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "hotpink") +  # light pink → dark purple
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  ) +
  labs(
    title = "Heatmap of Alzheimer’s Mortality Rate (2000–2021)",
    x = "Year",
    y = "Country",
    fill = "Deaths per 100k")

ggplotly(p, tooltip = c("x", "y", "fill"))







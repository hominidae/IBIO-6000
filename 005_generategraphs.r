# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# This script is intended to produce graphs of available data using ggplot
library(tidyverse)
library(ggplot2)
library(skimr)
library(viridis)

# First though, let's load our data
working_copy <- read_csv("E:/2021_UoG/IBIO 6000/src/Data/corrected_data.csv")
# A small problem, it doesn't contain some of the information we need to process.
# Let's load the original specimen_data from DS-ITLP.txt too.
specimen_data <- read_tsv("E:/2021_UoG/IBIO 6000/src/data/DS-ITLP.txt")

# To start with, We want to graph the following data:
# 1 - recordID (unique key)
# 2 - family_name
# 3 - country_origin
# 4 - intercept_state
familydata <- data.frame(
  working_copy$recordID,
  working_copy$country_origin,
  working_copy$intercept_state,
  specimen_data$order_name,
  specimen_data$family_name,
  specimen_data$subfamily_name,
  specimen_data$genus_name,
  specimen_data$species_name
  )

# We haven't lost any records yet, so we'll change the names
names(familydata) <- c("recordID","country_origin","intercept_state","order_name","family_name","subfamily_name","genus_name","species_name")

# Next, drop anywhere there are NA's in country_origin and intercept_state
strip_familydata <- familydata %>% drop_na(country_origin,intercept_state)
# How many families of Lepidoptera are present?
strip_familydata$family_name %>% unique
# How about subfamilies?
strip_familydata$subfamily_name %>% unique
# How about genus?
strip_familydata$genus_name %>% unique

# What about some graphs?
ggplot(familydata, aes(intercept_state, country_origin, colour = family_name)) +
  geom_point()

# Let's remove a couple of the outliers that haven't been identified yet from the data to make our graphs cleaner.
strip_familydata2 <- familydata %>% drop_na(country_origin,intercept_state,family_name)

# Next, graph it.
ggplot(strip_familydata2, aes(intercept_state, country_origin, colour = family_name)) +
  geom_point()

# Let's look at the raw data
familydata %>% skim()
# With the NA's removed for country_origin and intercept_state
strip_familydata %>% skim()
# Now with NA's removed for country_origin, intercept_state, and family_name
strip_familydata2 %>% skim()
# Interesting, from 241 original records to 197 with complete country_origin and intercept_state information
# However, let's check if there are duplicates introduced.
strip_familydata2 %>%
  match[country_origin,intercept_state]

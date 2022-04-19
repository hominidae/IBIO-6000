# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# This script is intended to produce graphs of available data using ggplot
library(tidyverse)
library(ggplot2)
library(skimr)


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

# Let's remove a couple of the outliers that haven't been identified yet from the data to make our graphs cleaner.
strip_familydata2 <- familydata %>% drop_na(country_origin,intercept_state,family_name)

# We need to remove one rather annoying anomoly. San Jose cannot be both a source and a destination.
# The issue relates to user error on data entry.
strip_familydata3 <- strip_familydata2 %>%
  filter(!country_origin == "San Jose")

# Let's look at the raw data
familydata %>% skim()
# With any rows where NA's were removed from country_origin and intercept_state
strip_familydata %>% skim()
# Now with NA's removed for country_origin, intercept_state, and family_name
strip_familydata2 %>% skim()
# Interesting, from 241 original records to 197 with complete country_origin and intercept_state information
# That goes down to 190 records if we remove any data that doesn't have complete country_origin, intercept_state, and family_name
strip_familydata3 %>% skim()
# And now down to 189 usable records. We had to remove 52 records because of incomplete information.
# That's nearly 21.6% of the original records lost due to various issues with data entry, errors, etc

# Alas, on to the graphing.

# Let's get a count of unique families of moth species represented in the data.
familycount <- count(strip_familydata3, family_name)
familycount
countrycount <- count(strip_familydata3, country_origin)
countrycount
interceptcount <- count(strip_familydata3, intercept_state)
interceptcount

# 1. Generate a graph showing the approximate number of families of Lepidoptera present in the data
# Todo:
#  - Replace geom_points with bars
#  - Replace the labels for x and y axis labels
#  - More accurate n value ranges
familycount %>%
  ggplot(aes(x = n, y = reorder(family_name, -n)))+
  geom_point(aes(n))

# 2. Generate a graph showing the approximate number of countries of origin present in the data
# Todo:
#  - Replace geom_points with bars
#  - Replace the labels for x and y axis labels
#  - More accurate n value ranges
countrycount %>%
  ggplot(aes(x = n, y = reorder(country_origin, -n)))+
  geom_point(aes(n))

# 3. Generate a graph showing the approximate number of US states present in the data
# Todo:
#  - Replace geom_points with bars
#  - Replace the labels for x and y axis labels
#  - More accurate n value ranges
interceptcount %>%
  ggplot(aes(x = n, y = reorder(intercept_state, -n)))+
  geom_point(aes(n))

# 4. Generate a graph showing country of origin with intercept state and family name in the same graph with an approximation

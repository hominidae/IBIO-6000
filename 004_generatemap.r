# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# This script does one thing and one thing only, it generates a map from collected data.

# Install some packages if you don't already have 'em
install.packages('tidyverse')
install.packages('ggmap')

# Load libraries
library(tidyverse)
library(ggmap)

# Read the csv containing data into working_copy
working_copy <- read_csv("E:/2021_UoG/IBIO 6000/src/Data/corrected_data.csv")

# Copy working_copy country_origin to sourcesnotdistinct
sourcesnotdistinct <- working_copy$country_origin

# Copy working_copy intercept_state to destinationsnotdistinct
destinationsnotdistinct <- working_copy$intercept_state

# Remove any NA's from country_origin
sourcesnotdistinct <- sourcesnotdistinct[!is.na(sourcesnotdistinct)]

# Remove any NA's from intercept_state
destinationsnotdistinct <- destinationsnotdistinct[!is.na(destinationsnotdistinct)]

# Next, let's make two lists containing just the unique entries from each and save them as lists
sources <- unique(sourcesnotdistinct)
destinations <- unique(destinationsnotdistinct)

# Turn them into data frame's so we can peruse them easily
sourcescheck <- data.frame(sources)
destinationscheck <- data.frame(destinations)
# Very interesting. There are only 52 countries and 20 US states represented in the data.

# Next, we need to remove any entries that have no origin or no destination.
stripcopy <- working_copy %>% drop_na(country_origin, intercept_state)
# Interesting. stripcopy now has 185 records from 241 originally where there is a country_origin and intercept_state

# Now we need to take GPS coords with country_origin and match against GPS coords to intercept_state

# Create a  copy of stripcopy but only with country_origin and intercept_state columnns
source2destination <- stripcopy %>%
  select(country_origin,intercept_state)

# Next, let's move on to mapping.

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

# Remove any NA's from country_origin vector list
sourcesnotdistinct <- sourcesnotdistinct[!is.na(sourcesnotdistinct)]

# Remove any NA's from intercept_state vector list
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

# Next, let's move on to mapping our data. Most of this code was borrowed from Alicia Halhed's ggmaptutorial R Markdown script
# However, instead of mapping R's built-in crime_data we'll be mapping our processed insect data.
# First, let's load the fixed countries data
countries <- read_csv("E:/2021_UoG/IBIO 6000/src/data/countries_utf8.txt")

# Let's copy our normalized US state data too.
states <- read_csv("E:/2021_UoG/IBIO 6000/src/Data/corrected_states.csv")

# Copy source2destination as a csv
write_csv(x = source2destination, "E:/2021_UoG/IBIO 6000/src/Data/src2dest.csv")

# We're going to a need to map the whole planet. Since we have 52 countries represented within country_origin
# The United States will need to be in the center.

# We need to adjust countries to only contain country.name, country.lat, country.long
countries <- data.frame(
  country.name = countries$country.name,
  country.lat = countries$country.lat,
  country.long = countries$country.long
)

states <- data.frame(
  state.name = states$state.name,
  state.lat = states$state.lat,
  state.long = states$state.long
)

# Before we do that though, let's create a dataframe from our data containing the country_origin, intercept_state, and the GPS coords for both
# We'll need to remove country.abbr and state.abb though since they aren't necessary now.
src2dest <- working_copy %>%
  merge(countries, by.x = "country_origin", by.y = "country.name") %>%
  merge(states, by.x = "intercept_state", by.y = "state.name")

# Adjust the order of columns for aesthetic purposes
src2dest <- data.frame(
  recordID = src2dest$recordID,
  country_origin = src2dest$country_origin,
  intercept_state = src2dest$intercept_state,
  country.lat = src2dest$country.lat,
  country.long = src2dest$country.long,
  state.lat = src2dest$state.lat,
  state.long = src2dest$state.long
)

# Now, let's try creating a map with geom_Line!

# Let's populate GPS vectgor lists forf x and GPS y coordinates. x is long, y is lat
country.x <- src2dest$country.long
country.y <- src2dest$country.lat
state.x <- src2dest$state.long
state.y <- src2dest$state.lat

mp <- NULL # Initalize mp
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() + mapWorld # Call ggplot and mapworld over it
# Now, layer cities on top
mp <- mp+ geom_point(aes(x=country.x, y=country.y) ,color="blue", size=1)
mp

# Let's do the same with destination states
mp <- mp+ geom_point(aes(x=state.x, y=state.y), color="green", size=1)
mp

mp <- mp+ geom_segment(aes(x = country.x, y=country.y, xend = state.x, yend = state.y),
                       color = "yellow", size = 0.5, alpha = 0.8, lineend = "round")
mp

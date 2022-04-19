# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# This script does one thing and one thing only, it generates several maps from the processed data.

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

# Now let's add some lines to link the country of origin with the destination states
mp <- mp+ geom_segment(aes(x = country.x, y=country.y, xend = state.x, yend = state.y),
                       color = "yellow", size = 0.5, alpha = 0.8, lineend = "round")
mp
# Somehow, the result is not quite what we want.

# Instead, let's focus on drawing from major continents one at a time.

# To do that, we'll need to classify each country by it's continent by adding a continent field.
# I originally did this by hand, but I found a library that simplifies things.

# To make that easier, let's install the countrycode library
install.packages("countrycode") 
# Load it.
library(countrycode)

# Let's try countrycode. The calling convention is the source, the item, and the desired output.
countrycode(sourcevar = sources, origin = 'country.name', destination = 'continent')
# As you can see, we have an issue. San Jose was incorrectly entered as a country of origin and returns an NA.
# However, this is sufficient for our purposes as it provides a method to assign continents using an R library.

# So let's get on with the mapping. First, let's clear out the previous maps generated with mp
mp = NULL

# Let's create multiple region gps coords. We'll split src2dest off into multiple regions

continents2dest <- src2dest %>%
  mutate(
    recordID = src2dest$recordID,
    country_origin = src2dest$country_origin, # The line below looks crazy to me, but hey, it actually works.
    continent = countrycode(sourcevar = src2dest$country_origin, origin = 'country.name', destination = 'continent'),
    intercept_state = src2dest$intercept_state,
    country.lat = src2dest$country.lat,
    country.long = src2dest$country.long,
    state.lat = src2dest$state.lat,
    state.long = src2dest$state.long
  )

# Now that we have continents2dest we can simply map by each group as needed.
# I haven't figured out how to simplify this portion yet though. So we'll do it manually.

# First, let's only select north and south america.
america2dest <- continents2dest %>%
  filter(continent == "Americas")

# This is the part I haven't figured out to simplify. We'll need new calls for each continent. State remains the same throughout though.
# We still need to assign is as it's own thing though because it needs to match to a corresponding value.
america.x <- america2dest$country.long
america.y <- america2dest$country.lat
state1.x <- america2dest$state.long
state1.y <- america2dest$state.lat

# So, let's map america.
mp <- ggplot() + mapWorld # Call ggplot and mapworld over it
# Now, layer cities on top
mp <- mp+ geom_point(aes(x=america.x, y=america.y) ,color="blue", size=1)
mp

# Let's do the same with destination states
mp <- mp+ geom_point(aes(x=state.x, y=state.y), color="green", size=1)
mp

# Now let's add some lines to link the country of origin with the destination states
mp <- mp+ geom_segment(aes(x = america.x, y=america.y, xend = state1.x, yend = state1.y),
                       color = "yellow", size = 0.5, alpha = 0.8, lineend = "round")
mp
# Great, we should have a cool map with just links in north and south america. Let's move on to the other continents.
mp = NULL

# After America, let's map Europe.
europe2dest <- continents2dest %>%
  filter(continent == "Europe")

# This'll get boring eventually.
europe.x <- europe2dest$country.long
europe.y <- europe2dest$country.lat
state2.x <- europe2dest$state.long
state2.y <- europe2dest$state.lat

# In fact, it already has.
mp <- ggplot() + mapWorld # Call ggplot and mapworld over it
mp <- mp+ geom_point(aes(x=europe.x, y=europe.y) ,color="blue", size=1)
mp <- mp+ geom_point(aes(x=state2.x, y=state2.y), color="green", size=1)
mp <- mp+ geom_segment(aes(x = europe.x, y=europe.y, xend = state2.x, yend = state2.y),
                       color = "yellow", size = 0.5, alpha = 0.8, lineend = "round")
mp
# I was convinced this would look better. But it isn't. It's tedious and ugly. But I wanted maps so Allez allez allez. On y va.
# BTW That is the limit of the french that I learned while studying at the Royal Military College of Canada

# Let's repeat the same for Asia and we're done.
asia2dest <- continents2dest %>%
  filter(continent == "Asia")

# More assignments
asia.x <- asia2dest$country.long
asia.y <- asia2dest$country.lat
state3.x <- asia2dest$state.long
state3.y <- asia2dest$state.lat

# More ggplotting.
mp = NULL
mp <- ggplot() + mapWorld # Call ggplot and mapworld over it
mp <- mp+ geom_point(aes(x=asia.x, y=asia.y) ,color="blue", size=1)
mp <- mp+ geom_point(aes(x=state3.x, y=state3.y), color="green", size=1)
mp <- mp+ geom_segment(aes(x = asia.x, y=asia.y, xend = state3.x, yend = state3.y),
                       color = "yellow", size = 0.5, alpha = 0.8, lineend = "round")
mp

# In summary, we have done the following:
# 1 - Mapped a representation of the data
# 2 - There are some issues. Primarily we should map intensity of those links too.

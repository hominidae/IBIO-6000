# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# First, install some packages
install.packages('tidyverse') # Self-explanatory
install.packages('maps') # We will need this for a list of US city names to compare against content
install.packages('dplyr')

library(tidyverse)
library(maps)
library(dplyr)

## Data import ----------
specimen_data <- read_tsv("E:/2021_UoG/IBIO 6000/src/Data/DS-ITLP.txt")
specimen_data

## Let's import some US city names and other related information
# Use maps library to provide a list of US cities and remove the abbreviation
# Retain useful information for later, specifically the GPS coordinates for those cities for mapping later.
uscities <- us.cities %>%
  select(name, country.etc, pop, lat, long, capital)

# But first remove the redundant abbreviated state code from list of US cities
# Since it already exists as country.etc it's superfluous information
uscities$name <- gsub('.{2}$', '', as.character(uscities$name))

# R comes with a list of US state names and their abbreviated two letter alphanumeric code
# Combine those two vectors into a single data frame
states <- data.frame(state.abb, state.name)

# extract collection notes information
newdata <- specimen_data %>%
  select(recordID, collection_note, notes) %>% 
  separate(collection_note, sep = "Border Interception, ",
           into = c("border", "has_country")) %>% 
  separate(has_country, sep = "Suspected country of origin: ",
           into = c("country", "B")) %>% 
  separate(B, sep = ", Interception location: ",
           into = c("country_origin", "C")) %>% 
  separate(C, sep = ", Interception Number: ",
           into = c("intercept_location", "intercept_number")) %>% 
  separate(intercept_location, sep = ",",
           into = c("intercept_state", "intercept_country")) %>%
  select(recordID, border, country_origin, intercept_state, intercept_country, intercept_number)

# Let's separate the city + state by spaces. Note that city names with a space inside are separated too. So that needs to be fixed.
fixstates <- newdata %>%
  select(recordID, intercept_state) %>%
  separate(intercept_state, sep = " ",
           into = c("intercept_city", "intercept_state"))

# If you look at fixstates, you'll notice that "New York" is split into "New" for intercept_state and "York" for intercept_city.
# Same for "Los" and "Angeles" for "Los Angeles" Far from ideal. But we'll deal with that later.

# Replace anywhere there are two capital letters in "intercept_state" with the full state name from the states dataframe.

# Create a vector from fixstates that we can work with
vector_states <- fixstates$intercept_state
# Check that list against state.abb, but note that anything that isn't matching is now NA.
state.name[match(vector_states,state.abb)]
# We can work with this, create a new vector from that.
cleanstates <- state.name[match(vector_states,state.abb)]
coalesce(cleanstates,vector_states)

# Double-check that nothing has been changed badly, after a cursory examination it appears it worked! They match where they need to.
checkstates <- data.frame(cleanstates,vector_states)

# Now, we need to combine the cleanstates vector list back into the existing "fixstates" dataframe
# Move the vector list cleanstates back into fixstates for the "intercept_state" column
newstates <- coalesce(cleanstates,vector_states)

# Assign newstates to "intercept_state_check" in fixstates
fixstates$intercept_state <- newstates

# We'll do something interesting, match against list of states and if it is a state move it into "intercept_state"
# But first we need a copy of "intercept_city"
vector_city <- fixstates$intercept_city
clean_city2state <- state.name[match(vector_city,state.name)]
coalesce(clean_city2state,vector_city)

# Let's perform a sanity check
checkcity2state <- data.frame(clean_city2state,vector_city)
# Looks okay. Save this for later

# Can we do this a second time to double-check?
# We want to isolate where the separate function split "New York" into "New" and "York" for intercept_city and intercept_state respectively
# We can do that by creating a third list that combines intercept_city and intercept_state together then checking it against the list
citysanity_check <- paste(fixstates$intercept_city, fixstates$intercept_state, sep=" ")
# Obvs something went wrong, because if you look at citysanity_check, there are "NA NA"s and "California NA"s

# Let's try using this function from sysilviakim called Kmisc
# First install it.
install.packages("remotes")
# Then install it.
remotes::install_github("sysilviakim/Kmisc")

# pasta_na removes the " NA" and "NA NA" because that's obviously not something we want.
citysanity_check <- paste_na(fixstates$intercept_city, fixstates$intercept_state, sep=" ")

# use the same matching function as before to confirm
citysanity_check <- state.name[match(citysanity_check,state.name)]


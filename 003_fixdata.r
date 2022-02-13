# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# Compartmentalize!

# This script does one thing and one thing only, it processes country_origin and intercept_state information from 001_collection_note.r and 002_notes.r


# Upon completion, it will save that normalized data as csv file

# Load library
library(tidyverse)

# Load processed data from previous two scripts
notes <- read_csv("E:/2021_UoG/IBIO 6000/src/Data/notes_processed.csv")

# We need to load a list of country names containing their geographical center. We can get that data from Google.
# Available here: https://developers.google.com/public-data/docs/canonical/countries_csv
# Prior to using that csv data, we do need to convert it to UTF-8 though.
# I opened the saved CSV using Notepad++ and selected "Convert to UTF-8" under "Encoding" in the main "File" window
countries <- read_csv("E:/2021_UoG/IBIO 6000/src/data/countries_utf8.txt")

# We have a built-in within R that contains state names, abbreviated state names, and their geographical center.
# Let's create a data frame with that information
states <- data.frame(state.abb,state.name,state.center)
# Then let's rename those columns
names(states) <- c("state.abb","state.name","state.long","state.lat")

# A couple of fixes needed. First, Puerto Rico is not considered a US state.
# So let's fix that for our purposes.
notrealstate <- data.frame("PR", "Puerto Rico","-66.4314","18.2270")
names(notrealstate) <- c("state.abb", "state.name","state.long","state.lat")

# U.S. Virgin Ilands Centre required too!
# Also requires lat/long fixes for Alaska, Delaware, Hawaii, Rhode Island, U.S. Virgin Islands

# Append it to the end of the states dataframe
states <- rbind(states,notrealstate)

# Let's move the lat/long columns
statesfixed <- data.frame(states$state.abb,states$state.name,states$state.lat,states$state.long)
names(statesfixed) <- c("state.abb","state.name","state.lat","state.long")

# Create two copies of notes country_origin and intercept_state
sourcesnotdistinct <- data.frame(notes$country_origin)
destinationsnotdistinct <- data.frame(notes$intercept_state)

# Let's replace incorrectly formatted country_origin values from the standardized list from Google.
# Again, that data is available here: https://developers.google.com/public-data/docs/canonical/countries_csv
names(sourcesnotdistinct) <- c("name")
sourcesnotdistinct$name[sourcesnotdistinct == "The United States of America"] <- "United States"
sourcesnotdistinct$name[sourcesnotdistinct == "US Virgin Islands"] <- "U.S. Virgin Islands"
sourcesnotdistinct$name[sourcesnotdistinct == "Democratic Republic of Congo"] <- "Congo [DRC]"
sourcesnotdistinct$name[sourcesnotdistinct == "United Kingdom of Great Britain and N. Ireland"] <- "Great Britain"

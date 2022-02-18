# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# Compartmentalize!

# This script does one thing and one thing only, it processes country_origin and intercept_state information from 001_collection_note.r and 002_notes.r
# There a small number of fixes to the data that are required for normalization purposes.
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
notrealstate <- data.frame(
  state.abb = c("PR","VI"),
  state.name = c("Puerto Rico","U.S. Virgin Islands"),
  state.long = c("-66.4314","-64.840971"),
  state.lat = c("18.2270","18.060930")
  )

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

# We also have an issue where several states from the states built-in have the incorrect geographic center. So this is a good time to fix those.
# Not ideal, but I'll have to figure out how to do join operations by a specific row ID at some point.
# Alaska
statesfixed$state.lat[statesfixed$state.lat=="49.25"] <- "64.73166" # Replace Alaska lat with correct lat
statesfixed$state.long[statesfixed$state.long=="-127.25"] <- "-152.47" # Replace Alaska long with correct long
# Delaware
statesfixed$state.lat[statesfixed$state.lat=="38.6777"] <- "38.9896" # Replace Delaware lat with correct lat
statesfixed$state.long[statesfixed$state.long=="-74.9841"] <- "-75.505" # Replace Delaware long with correct long
# Rhode Island
statesfixed$state.lat[statesfixed$state.lat=="41.5928"] <- "41.6762" # Replace Rhode Island lat with correct lat
statesfixed$state.long[statesfixed$state.long=="-71.1244"] <- "-71.5562" # Replace Rhode Island long with correct long
# Yay. statesfixed is now correct and proper.

# Finally, export the processed and normalized data
write_csv(x = notes_final, "E:/2021_UoG/IBIO 6000/src/Data/alldata_processed.csv")

# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# Todo:
# Compartmentalize! 000_master.r currently contains all code.
# Move code over into separate sections as each objective is complete.
# This script should do one thing and one thing only, sort through the "collection_note" column of specimen_data
# and extract both country_origin and intercept_state information.
# Upon completion, it will save that data as csv file

# Status:
# Code is functional, unit test confirms entries are correct.
# However, a refactor test of collection_notes and notes needs to be confirmed.

# Load tidyverse library
library(tidyverse)

# Load the original data set that we will be working with.
specimen_data <- read_tsv("E:/2021_UoG/IBIO 6000/src/data/DS-ITLP.txt") # Change to wherever you downloade the DS-ITLP.txt file from the github

# Populate a dataframe, col_note with recordID and values from collection_note within the DS-ITLP.txt
col_note <- specimen_data %>%
  select(recordID, collection_note)

# Strip ""Border Interception," if present from text and save it as it's own column called rmborder within a dataframe called originstart
originstart <- col_note %>%
  mutate(
    rmborder = str_extract(collection_note, "(?<=Border Interception,).*") # This will select an retain all text after "Border Interception," in rmborder
  )

# Populate another working dataframe, sans "Border Interception, and "Suspected country of origin: " removed, with everything afterwards gone too.
# There are some irregularities in the data, so origin2 and origin3 catch those exceptions.
origin <- originstart %>% # Make a copy of originstart called origin
  mutate(
    origin1 = str_extract(rmborder, "(?<=Suspected country of origin: ).*(?=, Interception location)"), # Catch anything between. Matches 50% of records
    origin2 = str_extract(rmborder, "(?<=Suspected country of origin: )Puerto Rico"), # Catch "Puerto Rico" outlier.
    origin3 = str_extract(rmborder, "(?<=Suspected country of origin: )US Virgin Islands") # Catch "US Virgin Islands" outlier.
  ) # This will select and retain all the text after "Border Interception," in origin1, origin2, and origin3

# Use the coalesce function to utilize the first non-NA field that it encounters.
# This combines everything caught from origin1, origin2, and origin3 into it's own vector list
originfixedlist <- coalesce(origin$origin1,origin$origin2,origin$origin3)

# Creata a new dataframe to perform a visual inspection of the columns against one another to confirm they are correct with the combined list last
# It's also used to visually determine when and where the matches against the regular expression list occurs.
origintest <- data.frame(origin$recordID,col_note$collection_note,origin$origin1,origin$origin2,origin$origin3,originfixedlist)

# Once confident in results, assign origintest to origin and do some garbage collection
origin <- origintest
# Change the names of columns to reflect actual names
names(origin) <- c("recordID","collection_note","origin1","origin2","origin3","originfixed")

# Perfect, origin_country is done. Next, we still need to extract intercept_state from collection_note too
# But let's copy originstart to interceptstart
interceptstart <- originstart

# Note that we're still working with col_note
interceptloc <- interceptstart %>%
  mutate(
    state1 = str_extract(rmborder, "(?<=Interception location: ).*(?=, USA)"), # Match anything between intercept location and ", USA"
    state2 = str_extract(rmborder, "(?<=Interception location: ).*(?=,U)"), # Match between loc and ",U"
    state3 = str_extract(rmborder, "(?<=Interception location: )California"), # Match "California"
    state4 = str_extract(rmborder, "(?<=Interception location: )New York"), # Match "New York
    state5 = str_extract(rmborder, "([A-Z][A-Z](?!A))") # Match space + capital letter + capital letter
  )

# First, load state names from a built-in dataset in R. Make it a data frame that we can match against
states <- data.frame(state.abb, state.name)
# We need to match 2 letter states from state4 in interceptloc that match the built-in list of US state abbreviations
# So, create stateabbrv for that. Then assign the column state4 to it.
stateabbrv <- interceptloc$state5

# Couple of fixes still needed though based on the data before we do anything else..
# NZ is Arizona. Incorrect entry. Data is from the United States. Expect some level of illiteracy.
# PL is Pennsylvania. Incorrect entry. Data is from the United States. Expect some level of illiteracy.
# PR is Puerto Rico. Understandable because while Puerto Rico is a US territory, it is not an official US state.
# Nonetheless, we want all those fixed. This all needs to happen before we make our final list and save it though.

# The fix is pretty simple. Append those entries to the states data frame we created
notrealstate <- data.frame("PR", "Puerto Rico")
notrealstate2 <- data.frame("NZ", "Arizona")
notrealstate3 <- data.frame("PL", "Pennsylvania")
names(notrealstate) <- c("state.abb", "state.name")
names(notrealstate2) <- c("state.abb", "state.name")
names(notrealstate3) <- c("state.abb", "state.name")
# Once we create rules, we want to add  those rules to the states dataframe
states <- rbind(states,notrealstate)
states <- rbind(states,notrealstate2)
states <- rbind(states,notrealstate3)

# Next, we match against our list and set it's precedence during a coalesce
expandstates <- state.name[match(stateabbrv,state.abb)]

# We need to coalesce and append the fixed results to interceptloc, since interceptloc comes before interceptloc$state4, the full name is used instead.
interceptlocfixedlist <- coalesce(interceptloc$state1,interceptloc$state2,interceptloc$state3,expandstates,interceptloc$state4)

# Now we need to create intercepttest for a visual inspection to confirm that it all looks right
intercepttest <- data.frame(col_note$recordID,col_note$collection_note,interceptloc$state1,interceptloc$state2,interceptloc$state3,interceptloc$state4,interceptlocfixedlist)

# Once we're confident that intercept_state is okay, let's save it as a dataframe called final containing country_origin as well
# Remember, we still need to process any intercept_state information present in the notes column of specimen_data as well.
final <- data.frame(col_note$recordID,origin$originfixed,interceptlocfixedlist)

names(final) <- c("recordID","country_origin","intercept_state")

# Now, we're done with collection_note let's save the data.
write_csv(x = final, "E:/2021_UoG/IBIO 6000/src/Data/collection_note_processed.csv")

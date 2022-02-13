# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# Compartmentalize!

# This script does one thing and one thing only, sort through "notes" for any country_origin and intercept_state data missed in "001_collection_note.r"
# Since the "notes" column only contains intercept_state information, no worries on country_origin.

# Upon completion, it will save the good data as csv file

# Load tidyverse library
library(tidyverse)

# Load the data set.
specimen_data <- read_tsv("E:/2021_UoG/IBIO 6000/src/Data/DS-ITLP.txt") # Change to wherever you downloade the DS-ITLP.txt file from the github

# Load previous work from 001_collecction_note.r
working_copy <- read_csv("E:/2021_UoG/IBIO 6000/src/Data/collection_note_processed.csv")

notes <- specimen_data %>%
  select(recordID, notes)

# Since we processed "collection_note" in 001_collection_note.r, we're going to focus on cleaning up and extracting anything "notes" in specimen_data

# This list only has intercept location, no country of origin present in all data. So, data from here should never pollute country_origin
# Small note: Use unique variable names to avoid major problems costing several days of debugging. Source: Took me 2 days to find out what went wrong due to a namespace issue.
notes2 <- notes %>%
  mutate( # Intercepted in California,United States: original id as Gelechiidae
    intercept_state1 = str_extract(notes, "(?<=Intercepted in ).*(?=,)"), # Matches most
    intercept_state2 = str_extract(notes, "(?<=Intercepted in ,).*(?=:)"), # Catches a few errors from Puerto Rico
    intercept_state3 = str_extract(notes, "(?<=Intercepted in )Puerto Rico(?=:)"), # Match only "Intercepted in Puerto Rico: Cydia splendana"
    intercept_state4 = str_extract(notes, "Puerto Rico"),
    intercept_state5 = str_extract(notes, "US Virgin Islands")
  )

# Before we move on however, I noticed that one field is wrong. It's because coalesce uses the first non-empty/NA.
# Since it's an empty string, it uses the empty string first.
# We could however change precedence to see if that fixes the issue before trying to find another solution so let's try that first.
intercept3fixed <- coalesce(notes2$intercept_state2,notes2$intercept_state1,notes2$intercept_state3,notes2$intercept_state4,notes2$intercept_state5)
notes_check <- data.frame(notes$recordID,notes$notes,intercept3fixed)

# That seemed to work. Let's go back to square one and combine all the countries of origin together.
# Remember, we're working on country_origin now, not intercept_state
notes_semifinal <- data.frame(notes2$recordID,specimen_data$collection_note,notes$notes,working_copy$country_origin,working_copy$intercept_state,intercept3fixed)
names(notes_semifinal) <- c("recordID","collection_note","notes","country_origin","intercept_state1","intercept_state2")

# Great, looks good. Now run a coalesce on intercept_state1 from processed collection_note and intercept_state from notes
notes_semifinalfixedlist <- coalesce(intercept3fixed,working_copy$intercept_state)

# Create a dataframe containing all the good data.
notes_final <- data.frame(specimen_data$recordID,working_copy$country_origin,notes_semifinalfixedlist)
# Rename the columns of notes_final before saving
names(notes_final) <- c("recordID","country_origin","intercept_state")

# Save as a CSV
write_csv(x = notes_final, "E:/2021_UoG/IBIO 6000/src/Data/notes_processed.csv")

# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# Compartmentalize!

# This script does one thing and one thing only, sort through "notes" for any country_origin and intercept_state data missed in "001_collection_note.r"

# Upon completion, it will save the good data as csv file

# Load tidyverse library
library(tidyverse)

# Load the data set.
specimen_data <- read_tsv("E:/2021_UoG/IBIO 6000/src/Data/DS-ITLP.txt") # Change to wherever you downloade the DS-ITLP.txt file from the github

notes <- specimen_data %>%
  select(recordID, notes)

# Since we processed "collection_note" in 001_collection_note.r, we're going to focus on cleaning up and extracting anything "notes" in specimen_data
interceptloc <- notes %>%
  mutate(
    state1 = str_extract(notes, "(?<=Intercepted in ).*(?=,)"), # Matches most but catches "New York" as an error for some reason.
    state2 = str_extract(notes, "Puerto Rico(?=:)"),
    state3 = str_extract(notes, "US Virgin Islands")
  )

# Those str_extract's capture 95%  of the right data.
# Before we move on however, let's use coalesce to normalize the list
# state2 before state1 due to an empty string being matched. No biggie.
interceptfixedlist <- coalesce(interceptloc$state2,interceptloc$state1,interceptloc$state3)

# Visual comparison
interceptfinal <- data.frame(notes$recordID,notes$notes,interceptfixedlist)
names(interceptfinal) <- c("recordID","notes","intercept_state")

# Since the "notes" column does not contain any origin_country data, we're not going to worry about.
# We will however, save the data
write_csv(x = interceptfinal, "E:/2021_UoG/IBIO 6000/src/Data/notes_processed.csv")

# Garbage collection
rm(col_note,interceptfinal,interceptloc,notes,specimen_data,interceptfixedlist)

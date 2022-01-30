# Load tidyverse library
library(tidyverse)

# These are test cases from "collection_note"
siru <- 
  data.frame(col = c("Border Interception, Suspected country of origin: Peru, Interception location: California, USA", 
                     "Border Interception, Suspected country of origin: Ecuador, Interception location: New York, USA",
                     "Border Interception, Suspected country of origin: Puerto Rico",
                     "Border Interception",
                     "Border Interception, Suspected country of origin: Mexico, Interception location: California, USA",
                     "Border Interception, Suspected country of origin: United Kingdom of Great Britain and N. Ireland, Interception location: Maryland, USA",
                     "Border Interception, Suspected country of origin: Mexico, Interception location: Pharr TX, Interception Number: APLTX130790838001",
                     "Border Interception, Suspected country of origin: Guatemala, Interception location: Los Angeles CA, Interception Number: APLCA131360461001",
                     "Border Interception, Suspected country of origin: US Virgin Islands",
                     "Border Interception, Interception location: Blaine WA, Interception Number:  APSWA100625553001"))

# Actual data, working set
siru <- col_note %>%
  select(recordID, collection_note, notes)

# A working set, sans "Border Interception, "
siru2 <- siru %>%
  mutate(
    rmborder = str_extract(collection_note, "(?<=Border Interception,).*")
  ) # This will select and retain all the text after "Border Interception," in rmborder to work with

# Create rules for matching the types of origin formats
siru3 <- siru2 %>%
  mutate(
    origin1 = str_extract(rmborder, "(?<=Suspected country of origin: ).*(?=, Interception location)"), # Matches 50% of records
    origin2 = str_extract(rmborder, "(?<=Suspected country of origin: )Puerto Rico"), # Catch "Puerto Rico". TODO: Change to match only "Capital letter + word"
    origin3 = str_extract(rmborder, "(?<=Suspected country of origin: )US Virgin Islands") # Catch "US Virgin Islands" TODO: Change to match "Capital + word + word
  ) # This will find most matches, but added a few individual catches for isolated outliers like "Puerto Rico" or "US Virgin Islands"

# Let's move those three into originfixed
originfixed <- coalesce(siru3$origin1,siru3$origin2,siru3$origin3)

# Create new dataframe to compare recordID, rmborder and a fixed list from origin country
siru5 <- data.frame(siru2$recordID,siru2$rmborder,originfixed)

# Next up, let's tackle intercept_state
siru6 <- siru2 %>%
  mutate(
    state1 = str_extract(rmborder, "(?<=Interception location: ).*(?=, USA)"), # Match ", USA"
    state2 = str_extract(rmborder, "(?<=Interception location: ).*(?=,U)"), # Match between loc and ",U"
    state3 = str_extract(rmborder, "(?<=Interception location: )California"), # Match "California"
    state4 = str_extract(rmborder, "(?<=Interception location: )New York"),
    state5 = str_extract(rmborder, "(?<=Interception location: ).*(?=, Interception Number)") # Match between inter loc and inter num
  )

# Todo: Strip city out, retain two letter state code from siru6


siru5 <- siru4 %>%
  mutate(
    intercept_number = str_extract(col, "(?<=Interception Number: ).*")
  )

# Load tidyverse library
library(tidyverse)
library(maps)

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
  select(recordID, notes,collection_note)

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
    state4 = str_extract(rmborder, "(?<=Interception location: )New York"), # Match "New York
    state5 = str_extract(rmborder, "([A-Z][A-Z](?!A))") # Match space + capital letter + capital letter
  )

# Replace two letter state code with full state name instead
# First, load state names and state abbreviation into a dataframe
states <- data.frame(state.abb, state.name)
stateabbrv <- siru6$state5

# Next, let's fix state5 by taking the state code and replacing it with the full name
# We're using match to replace but within a vector
expandstates <- state.name[match(stateabbrv,state.abb)]

# Now we need to use coalesce to combine state1, state2, state3, state4, and the fixed state5
statesfixed <- coalesce(siru6$state1,siru6$state2,siru6$state3,siru6$state4,expandstates)

# Next, make a new dataframe called siru7
siru7 <- data.frame(siru2$recordID,siru2$rmborder,originfixed,statesfixed)

# It appears we need to add Puerto Rico as a state so "PR" shows up correctly.
notrealstate <- data.frame("PR", "Puerto Rico")
notrealstate2 <- data.frame("NZ", "Arizona")
names(notrealstate) <- c("state.abb", "state.name")
names(notrealstate2) <- c("state.abb", "state.name")
states <- rbind(states,notrealstate)
states <- rbind(states,notrealstate2)

# Next, let's run that matching shit all over again.
expandstates <- states$state.name[match(stateabbrv,states$state.abb)]
statesfixed <- coalesce(siru6$state1,siru6$state2,siru6$state3,siru6$state4,expandstates)

# All done! We have country of origin and intercept state.
siru7 <- data.frame(siru2$recordID,siru2$rmborder,originfixed,statesfixed)

# Load tidyverse library
library(tidyverse)
library(maps)

# The data set.
specimen_data <- read_tsv("../DS-ITLP.txt") # Change to wherever you downloade the DS-ITLP.txt file from the github

# Take specimen_data and copy specific columns to work on
col_note <- specimen_data %>%
  select(recordID, collection_note, notes)

# These are test cases from "collection_note" if you don't want to load the file for some reason.
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

# Actual data, working set. Will fail unless you actually load specimen_data
siru <- col_note %>%
  select(recordID, collection_note)

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

names(siru7) <- c("recordID", "col_note", "origincountry", "intercept_state")

# Now that we've done that, let's move on and process the "notes" field now too
# First, populate siru8 with the recordID and notes field
siru8 <- col_note %>%
  select(recordID, notes)

# First, let's extract intercept_state
siru9 <- siru8 %>%
  mutate( # Intercepted in California,United States: original id as Gelechiidae
    origin1 = str_extract(notes, "(?<=Intercepted in ).*(?=,)"), # Matches most
    origin2 = str_extract(notes, "(?<=Intercepted in ,).*(?=:)"), # Catches a few errors from Puerto Rico
    origin3 = str_extract(notes, "(?<=Intercepted in )Puerto Rico(?=:)") # Match only "Intercepted in Puerto Rico: Cydia splendana"
  ) # That looks to be about it, recordID 3271851 might be an error since it returns "United States" which is false but we can remove that in post.

# So, as per before we need to coalesce these into a master list.
origin2fixed <- coalesce(siru9$origin1,siru9$origin2,siru9$origin3)

# Let's put these into a new dataframe and compare visually to confirm
siru10 <- data.frame(siru2$recordID,siru9$notes,origin2fixed)

# Before we move on however, I noticed that one field is wrong. It's because coalesce uses the first non-empty/NA. Since it's an empty string, it uses the empty string first. We could however change precedence to see if that fixes the issue before trying to find another solution so let's try that first.
origin2fixed <- coalesce(siru9$origin2,siru9$origin1,siru9$origin3)
siru11 <- data.frame(siru2$recordID,siru9$notes,origin2fixed)

# That seemed to work. Let's go back to square one and combine all the countries of origin together.
origin3fixed <- coalesce(originfixed,origin2fixed)
siru11 <- data.frame(siru2$recordID,origin3fixed,statesfixed)
names(siru11) <- c("recordID","country_origin","intercept_state")

# We're going to need a dataframe containing recordID, country_origin, intercept_state, BIN_URI (aka unique BOLD ID's), family_name, subfamily_name, genus, species
# as well as lifestage.
finalcopy <- data.frame(specimen_data$recordID,origin3fixed,statesfixed,specimen_data$bin_uri,specimen_data$family_name,specimen_data$subfamily_name,specimen_data$genus_name,specimen_data$species_name,specimen_data$lifestage)

# I noticed an issue with lifestage not matching. Change L to Larvae, A to Adult, P to Pupae.
# Create a test dataframe to try this out on
lifestage <- data.frame(specimen_data$lifestage)
names(lifestage) <- c("lifestage")

# We need to convert lifestage to a f
lifestage$lifestage <- as.character(lifestage$lifestage)

# Next, replace A with Adult, L with Larvae, P with Pupae
lifestage$lifestage[lifestage$lifestage == "A"] <- "Adult"
lifestage$lifestage[lifestage$lifestage == "L"] <- "Larvae"
lifestage$lifestage[lifestage$lifestage == "P"] <- "Pupae"

# Convert back to factor
lifestage$lifestage <- as.factor(lifestage$lifestage)

# Re-integrate into final
# Populate final
finalcopy <- specimen_data %>%
  select(recordID, bin_uri,family_name,subfamily_name,genus_name,species_name)

# Use cbind to add correct lifestage, and the origin and intercept state
final <- cbind(finalcopy,lifestage,origin3fixed,statesfixed)

# Finally, change names of country_origin and intercept_state
names(final) <- c("recordID","bin_uri","family_name","subfamily_name","genus_name","species_name","lifestage","country_origin","intercept_state")

writeme <- final

# That's it. We're finally done. Let's write it to a CSV.
write_csv(x = writeme, "E:/2021_UoG/IBIO 6000/src/Data/working_copy.csv")

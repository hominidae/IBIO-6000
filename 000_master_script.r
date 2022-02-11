# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# So far, this script includes everything.
# Split this into logical stages.

# Individual R scripts will by splitting 000_mutate.r this way:
# 001_collection_note.r will output a csv containing recordID, country_origin, and intercept_state
# 002_notes.r will output a csv containing recordID, and intercept_state
# 003_fixdata.r will take the input from both those and normalize it appropriately.
# 004_exploratoryanalysis.r will perform exploratory analysis of the newly normalized dataset.

# Load tidyverse library
library(tidyverse)
library(maps)

# The data set.
specimen_data <- read_tsv("E:/2021_UoG/IBIO 6000/src/data/DS-ITLP.txt") # Change to wherever you downloade the DS-ITLP.txt file from the github

# Take specimen_data and copy specific columns to work on
col_note <- specimen_data %>%
  select(recordID, collection_note, notes)

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
kin6 <- siru2 %>%
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
stateabbrv <- kin6$state5

# Next, let's fix state5 by taking the state code and replacing it with the full name
# We're using match to replace but within a vector
expandstates <- state.name[match(stateabbrv,state.abb)]

############################################################################################# Maybe try match with the src2destination
# Union also possible

# Now we need to use coalesce to combine state1, state2, state3, state4, and the fixed state5
statesfixed <- coalesce(kin6$state1,kin6$state2,kin6$state3,kin6$state4,expandstates)

# Next, make a new dataframe called siru7
sirukin7 <- data.frame(siru2$recordID,siru2$rmborder,originfixed,statesfixed)

# It appears we need to add Puerto Rico as a state so "PR" shows up correctly.
notrealstate <- data.frame("PR", "Puerto Rico")
notrealstate2 <- data.frame("NZ", "Arizona")
notrealstate3 <- data.frame("PL", "Pennsylvania")
names(notrealstate) <- c("state.abb", "state.name")
names(notrealstate2) <- c("state.abb", "state.name")
names(notrealstate3) <- c("state.abb", "state.name")

states <- rbind(states,notrealstate)
states <- rbind(states,notrealstate2)
states <- rbind(states,notrealstate3)

# Next, let's run that matching shit all over again.
expandstates <- states$state.name[match(stateabbrv,states$state.abb)]
statesfixed <- coalesce(kin6$state1,kin6$state2,kin6$state3,kin6$state4,expandstates)

# All done! We have country of origin and intercept state.
sirukin7 <- data.frame(siru2$recordID,siru2$rmborder,originfixed,statesfixed)

names(sirukin7) <- c("recordID", "col_note", "origincountry", "intercept_state")

# Now that we've done that, let's move on and process the "notes" field now too
# First, populate siru8 with the recordID and notes field
notes8 <- col_note %>%
  select(recordID, notes)

# First, let's extract intercept_state
# This list only has intercept location, no country of origin present in all data.
# So, data from here should never pollute country_origin
# Small note: Use unique variable names to avoid major problems costing several days of debugging.
notes9 <- notes8 %>%
  mutate( # Intercepted in California,United States: original id as Gelechiidae
    intercept_state1 = str_extract(notes, "(?<=Intercepted in ).*(?=,)"), # Matches most
    intercept_state2 = str_extract(notes, "(?<=Intercepted in ,).*(?=:)"), # Catches a few errors from Puerto Rico
    intercept_state3 = str_extract(notes, "(?<=Intercepted in )Puerto Rico(?=:)") # Match only "Intercepted in Puerto Rico: Cydia splendana"
  ) # That looks to be about it, recordID 3271851 might be an error since it returns "United States" which is false but we can remove that in post.

# Before we move on however, I noticed that one field is wrong. It's because coalesce uses the first non-empty/NA.
# Since it's an empty string, it uses the empty string first.
# We could however change precedence to see if that fixes the issue before trying to find another solution so let's try that first.
intercept3fixed <- coalesce(notes9$intercept_state2,notes9$intercept_state1,notes9$intercept_state3)
sirukin11 <- data.frame(siru2$recordID,notes8$notes,intercept3fixed)

# That seemed to work. Let's go back to square one and combine all the countries of origin together.
# Remember, we're working on country_origin now, not intercept_state
sirukin11 <- data.frame(siru2$recordID,specimen_data$collection_note,notes9$notes,originfixed,intercept3fixed)
names(sirukin11) <- c("recordID","collection_notes","notes","country_origin","intercept_state")

# We're going to need a dataframe containing recordID, country_origin, intercept_state, BIN_URI (aka unique BOLD ID's), family_name, subfamily_name, genus, species
# as well as lifestage.
semifinalcopy <- data.frame(specimen_data$recordID,originfixed,intercept3fixed,specimen_data$bin_uri,specimen_data$family_name,specimen_data$subfamily_name,specimen_data$genus_name,specimen_data$species_name,specimen_data$lifestage)

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
semifinalcopy2 <- specimen_data %>%
  select(recordID, bin_uri,family_name,subfamily_name,genus_name,species_name)

# Use cbind to add correct lifestage, and the origin and intercept state
final <- cbind(semifinalcopy2,lifestage,originfixed,statesfixed)
# Finally, change names of country_origin and intercept_state
names(final) <- c("recordID","bin_uri","family_name","subfamily_name","genus_name","species_name","lifestage","country_origin","intercept_state")

# Write the data
writeme <- final
# That's it. We're finally done. Let's write it to a CSV.
write_csv(x = writeme, "E:/2021_UoG/IBIO 6000/src/Data/working_copy.csv")


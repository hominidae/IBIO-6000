# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# Compartmentalize!

# This script does one thing and one thing only, sort through "notes" for country_origin and intercept_state

# Upon completion, it will save the good data as csv file

# Load tidyverse library
library(tidyverse)

# Load the data set.
specimen_data <- read_tsv("E:/2021_UoG/IBIO 6000/src/Data/DS-ITLP.txt") # Change to wherever you downloade the DS-ITLP.txt file from the github

# Popualte col_note with specimen_data
col_note <- specimen_data %>%
  select(recordID, collection_note)

# A working set, sans "Border Interception, and "Suspected country of origin: " removed, with everything afterwards gone too.
origin <- col_note %>%
  mutate(
    origin1 = str_extract(collection_note, "(?<=Suspected country of origin: ).*(?=, Interception location)"), # Matches 50% of records
    origin2 = str_extract(collection_note, "(?<=Suspected country of origin: )Puerto Rico"), # Catch "Puerto Rico".
    origin3 = str_extract(collection_note, "(?<=Suspected country of origin: )US Virgin Islands") # Catch "US Virgin Islands"
  ) # This will select and retain all the text after "Border Interception," in rmborder to work with

# Use coalesce to use the first non-NA field it encounters
originfixedlist <- coalesce(origin$origin1,origin$origin2,origin$origin3)

# Perform a visual inspection of the columns against one another
origintest <- data.frame(origin$recordID,origin$collection_note,origin$origin1,origin$origin2,origin$origin3,originfixedlist)

# Once you're confident, assign origintest to origin and do some garbage collection
origin <- origintest
names(origin) <- c("recordID","collection_note","origin1","origin2","origin3","originfixed")

# Garbage collection, should only be left with specimen_data and origin
rm(origintest,originfixedlist)

# Perfect, origin_country is done. Next, we still need to extract intercept_state from collection_note too

# Note that we're still working with col_note
interceptloc <- col_note %>%
  mutate(
    state1 = str_extract(collection_note, "(?<=Interception location: ).*(?=, USA)"), # Match anything between intloc and ", USA"
    state2 = str_extract(collection_note, "(?<=Interception location: )California"), # Match "California"
    state3 = str_extract(collection_note, "(?<=Interception location: )New York"), # Match "New York
    state4 = str_extract(collection_note, "([A-Z][A-Z](?!A))") # Match space + capital letter + capital letter, incorrectly matches USA but no biggie.
  )

# We need to coalesce and append the fixed results to interceptloc
interceptlocfixedlist <- coalesce(interceptloc$state1,interceptloc$state2,interceptloc$state3,interceptloc$state4)

# Now we need to create intercepttest for a visual inspection to confirm that it all looks right
intercepttest <- data.frame(col_note$recordID,col_note$collection_note,interceptloc$state1,interceptloc$state2,interceptloc$state3,interceptloc$state4,interceptlocfixedlist)

final <- data.frame(col_note$recordID,origin$originfixed,interceptlocfixedlist)

names(final) <- c("recordID","country_origin","intercept_state")

# Now, we're done with collection_note let's save the data.
write_csv(x = final, "E:/2021_UoG/IBIO 6000/src/Data/collection_note_processed.csv")

# Clear up environment
rm(col_note,final,intercept,interceptloc,intercepttest,origin,specimen_data,writeme,interceptlocfixedlist)

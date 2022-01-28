# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# First, let's load tidyverse
library(tidyverse)

# Next, let's load up our data.
# Since the data is in tab seperated format, use read_tsv()
specimen_data <- read_tsv("E:/2021_UoG/IBIO 6000/src/Data/DS-ITLP.txt")

# Here is a list of the columns:
# 1 - processid
# 2 - sampleid
# 3 - recordID
# 4 - catalognum
# 5 - fieldnum
# 6 - institution_storing
# 7 - collection_code
# 8 - bin_uri
# 9 - phylum_taxID
# 10 - phylum_name
# 11 - class_taxID
# 12 - class_name
# 13 - order_taxID
# 14 - order_name
# 15 - family_taxID
# 16 - family_name
# 17 - subfamily_taxID
# 18 - subfamily_name
# 19 - genus_taxID
# 20 - genus_name
# 21 - species_taxID
# 22 - species_name
# 23 - subspecies_taxID
# 24 - subspecies_name
# 25 - identification_provided_by
# 26 - identification_method
# 27 - identification_reference
# 28 - tax_note
# 29 - voucher_status
# 30 - tissue_type
# 31 - collection_event_id
# 32 - collectors
# 33 - collectiondate_start
# 34 - collectiondate_end
# 35 - collectiontime
# 36 - collection_note
# 37 - site_code
# 38 - sampling_protocol
# 39 - lifestage
# 40 - sex
# 41 - reproduction
# 42 - habitat
# 43 - associated_specimens
# 44 - associated_taxa
# 45 - extrainfo
# 46 - notes
# 47 - lat
# 48 - long
# 49 - coord_source
# 50 - coord_accuracy

# So, now that we have an idea of how the data is laid out. Let's simplify working on it.
# First, let's produce a subset that makes it easier to work on what we're interested in.
notes <- specimen_data[, c("recordID","collection_note","notes")]
# What this did was create a new dataframe called "notes" which contains only the columns
# recordID, collection_note, and notes
# We're going to preserve recordID as it's a unique identifier for that data
# collection_note contains:
# NA
# Border Interception
# Border Interception, Suspected country of origin: <Country>, Interception location: <STATE, COUNTRY>

# So, in order to make this simple let's remove all NA's and put them into their own subset.
# new_DF <- DF[is.na(DF$Var),]
# In our case, we'll need:
notes_collectionnote_isna <- notes[is.na(notes$collection_note),]
# Running that and looking at notes_collectionnote_isna reveals that there are 8 records with NA in the collection_note field
# Let's do the same with notes too.
notes_notes_isna <- notes[is.na(notes$notes),]
# That's also 8 records long. But looking at both reveals that there are NA's in different recordID's between the two.
# So there are 8 NA's present in collection_note and 8 NA's present in notes.

# Rather than omit NA"s, let's just put both aside to create new dataframe containing just whole data.
whole_collectionnote <- na.omit(notes, cols = "collection_note")
# whole_collectionnote will now omit any NA's from notes (which is itself, just recordID, collection_note, notes from specimen_data)

# Let's do the same with notes too.
whole_notes <- na.omit(notes, col ="notes")

# So, now that we have two new dataframes where collection_note and notes both have data present.
# whole_collectionnote is where there are no NA's present in the collection_notes column
# whole_notes is where there are no NA's present in the notes column
# So coincidentally, they are both identical and we'll see why shortly.

# First, are there matching recordID's in both? AKA are there data with NA's in both collection_note and notes?
# Let's use intersect() to find out. Here, we take intersect and if there is matching data in x and y, it prints that matching data.
intersect(notes_collectionnote_isna,notes_notes_isna)
# In this case, it will print out 5 records. Coincidentally, these data records have NA's present in both collection_note and notes.
# This is the data that we don't want since we can't really work with it.

# Looking at the data, it appears as if there were not singular NA's present in collection_note or notes with data in the other.
# That means we don't need to do much more than separate NA's from either collection_note or notes.
# Just doing it once is enough.
# So let's do some clean-up.
rm(notes_collectionnote_isna)
rm(notes_notes_isna)
# Now we have specimen_data, our core dataset.
# We also have notes, which is a subset with data from recordID, collection_note, and the notes column.
# And since the missing values for collection_note are similar to the notes column in both we only need one.
# So let's remove the duplicate data.
rm(whole_notes)

# We're left with whole_collectionnote containing valid data in both "collection_note" and "notes" as a result.
# Since as indicated earlier, the missing data coincidentally intersects both.

# More importantly, can we take the text strings within notes and collection_note and split them off into their own columns?

# For example, let's describe the collection_note column first.
# There are complex strings present that look like this:
# Border Interception, Suspected country of origin: India, Interception location: San Francisco CA, Interception Number: APSCA123205625001
# Border Interception, Suspected country of origin: India, Interception location: San Francisco CA
# Border Interception, Suspected country of origin: Mexico, Interception location: California, USA
# Border Interception, Suspected country of origin: Puerto Rico
# Border Interception
# Since we've removed NA's, we don't need to worry about them when working with whole_collectionnote

# So, we want to create a new column titled "intercept type" with "Border Interception" moved there.
# We also want a new column titled "suspected_origin_country" with "India" if present.
# We also want a new column titled "intercept_location" with "San Francisco, CA" if present.
# The reason we want if present is because some just have "Border Interception" and that's it.

# To do that, we're going to need to use extract() to do this.
# What will need to be done is to first take any columns with _just_ "Border Interception" and placing them into a new dataframe on their own.
# We will use extract() from tidyr for this.
# First, let's make a working copy. Aptly enough, let's call it workcopy.
workcopy <- whole_collectionnote
# workcopy will be what we try out this extract() function on.
# Let's try to regex the first six characters, a space, followed by 12 characters.
extract(workcopy, c("collection_note"), into = "intercept_type", regex = "([[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:space:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]])")
# Fantastic, we've successfullly regex'd "Border Interception" into it's own column called "intercept_type"
# Let's make a new dataframe and look at it.
testcopy <- extract(workcopy, c("collection_note"), into = "intercept_type", regex = "([[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:space:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]][[:alnum:]])")
# Small problem identified, we've eliminated the other useful data that we want. Scooby would say "R'uh r'oh" about now but this is good...
# Let's examine that. We need to move match any columns that have "Border Interception" and only that text exactly and move it into a new dataframe. Then we'll move onto the other cases.

# We could also try separate() instead. Let's look at what that does.
separate(workcopy, c("collection_note"), into = "intercept_type", sep = ",")
# If you run this, you should get a warning that additional pieces were discarded in 201 rows. 230 - 201 = 29.
# So that means 29 instances had just "Border Interception" as the data. Interesting.
# Next, let's move that over to a dataframe called testcopy2
testcopy2 <- separate(workcopy, c("collection_note"), into = "intercept_type", sep = ",")
# So, pretty much effectively what was done with extract() and the regular expression matching only "Border Interception" and discarding the rest.
# If you look back to the previous example of what is contained in collection_note you will notice that there are a few ways the data is represented.
# The first is just the sting "Border Interception"
# The next is anything with text after "Border Interception" with a trailing ,
# The next data is a ":" followed by another "," and by another ":" and finally another "," if there's an Interception number.
# This will make the regexing fun.
# Before we do that though, let's try having separate() do the work for us by creating use case scenarios.
separate(workcopy, c("collection_note"), into = c("intercept_type", "suspected_origin_country", "intercept_location"), sep =",")
# Great, that looks kinda like what we want.
testcopy3 <- separate(workcopy, c("collection_note"), into = c("intercept_type", "suspected_origin_country", "intercept_location", "country", "interception_number"), sep =",")
# You will notice though that "Suspected country of origin: " is still present.
# You will also notice that "Interception location: " is also present. It's also stripped the country data from that.
# Since I'm lazy, we'll pursue the path of least resistance. We can work with this!
# We just need to clean-up the existing values and combine them as necessary by using regular expressions to repair the damage.

# First, let's remove the "Suspected country of origin: " from the suspected_country_origin column.
separate(testcopy3, c("suspected_origin_country"), into ="suspected_origin_country", sep =":")
# What we want basically is to remove "Suspected country of origin: " and preserve whatever is left.
extract(testcopy3, c("suspected_origin_country"), into = "suspected_origin_country", fixed("Suspected country of origin: "))
# Hmm, we seem to be running into a regex'ing issue. Namely, we can get this to work by statically declaring the size of the text string but it's tricky
# And it doesn't work in every instance. So here is when I took a step back and drank a few beers to think about the problem.
# The problem is that we don't want regular expressions per character, we want regular expressions applied to whole words and some limited intelligence applied to those words to direct them.

# Take this value for collection_note as an example:
# Border Interception, Suspected country of origin: India, Interception location: San Francisco CA, Interception Number: APSCA123205625001
# Here, we have "Border Interception" followed by a "," and a space.
# A space followed by "Suspected country of origin" followed by a ":" and a country
# Then there's a space and "Interception location" followed by a ":" and a state, country.
# Then "Interception Number" followed by a ":" and a 17 character alpha-numeric string.

# So let's look at an alternative solution.
# First let's find every instance of "Border Interception" in workcopy 
# To do that, let's do something simple first. Let's write a loop that iterate's over each row value in collection_note and counts the number of characters.
# We'll use nchar for that.
for (i in seq(1,230)) {
  print(nchar(workcopy[i,2]))
}
# Interesting. We now want that as a list of values.

my_list <- list() # Let's make my_list, an empty list.
for (i in seq(1,230)) { # This is the iteration. I couldn't figure out seq_along()
  print(nchar(workcopy[i,2])) # This is the part that iterates over each row of column 2.
  # Right, that seems to work just dandy. Now how do we stuff that into a vector?
  output <- nchar(workcopy[i,2])
  my_list[[i]] <- output
} 
my_list

# Great. We now have a list with the length of characters in each column.
# Let's do something easy and copy off any that are of length 19.

# Let's see if we can use a for loop to solve this.
# The for loop while need to iterate over each character in collection_note.
# As the for loop iterates over each character, it starts at the first character of "Border Interception" and stops at the "n:" characters.


# Next, let's look at the information contained in the notes column:
# Intercepted in California,United States: original id as Gelechiidae
# Looks like it's for the most part a duplication of the collection_note but if we look at the two "collection_note" field and "notes" field of the original specimen_data dataframe
# we will notice something interesting.
# Namely, collection_note will have "Border Interception" and that's it. However, looking at "notes" we will notice that it has data present that corresponds to an extract of collection_note.
# Intercepted in ,Puerto Rico: Gelechiidae
# So, that gives us another possible source for "intercept_location" in the notes column.

# Put differently, "intercept_type" will be pretty ubiquitous across data.
# But "intercept_location" can come from either collection_note or the notes column.

# Why do we want this information? And why bother regexing it?
# Well, primarily because we're interested in the following:
# 1 - recordID (for uniqueness)
# 2 - order_name (should all be lepidoptera as that was the point of the DS-ITLP study)
# 3 - family_name (once DNA barcoded and ID'd, this is interesting.)
# 4 - subfamily_name, genus, species (depends on how far we want to go at analysis)
# 5 - intercept_type (not necessarily that important)
# 6 - intercept_location (where was the moth collected?)
# 7 - suspected_origin_country (where did the moth come from?)

# From there, can we produce an approximation of the following information to answer these questions:
# A) What US port of entry had the most invasive moths collected?
# B) What countries of origin had the most invasive moth species entering the US?
# C) Can we draw a map with lines from US ports to origin countries?

# Let's try this suggestion using strsplit. Replace the first underscore with a dot and then split on dots:
# spl <- strsplit(sub("_", ".", x), ".", fixed = TRUE)
# sapply(spl, "[", 2)
# If we move every item that matches a certain character length for "Border Interception" into a new dataset, we will be left with three sets.
# All "Border Interception" which is 19 characters into new dataset.
# Border Interception

# I've added numbers to indicate placement of special charcters beyond alphanumeric characters.
# Border Interception, Suspected country of origin: Puerto Rico
#                    1                            2
# So, some data will have two special characters.

# Border Interception, Suspected country of origin: India, Interception location: San Francisco CA
#                    1                            2      3                      4
# Some data will have 4.

# Border Interception, Suspected country of origin: Mexico, Interception location: California, USA
#                    1                            2       3                      4           5
# Other will have five.

# Border Interception, Suspected country of origin: India, Interception location: San Francisco CA, Interception Number: APSCA123205625001
#                    1                            2      3                      4                5                     6
# In unique cases, there will be five special characters.

# How do we go about using a for loop to move each into a respective dataframe for processing?

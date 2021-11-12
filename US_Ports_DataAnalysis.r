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

# First, are there matching recordID's in both? AKA are there data with NA's in both collection_note and notes?
# Let's use intersect() to find out. Here, we take intersect and if there is matching data in x and y, it prints that matching data.
intersect(notes_collectionnote_isna,notes_notes_isna)
# In this case, it will print out 5 records. Coincidentally, both collection_note and notes have NA's present.

# Can we take the text within whole_notes and whole_collectionnote and split them into their own?

# For example, let's describe the collection_note column first. There are complex strings present that look like this:
# Border Interception, Suspected country of origin: India, Interception location: San Francisco CA, Interception Number: APSCA123205625001
# Border Interception, Suspected country of origin: India, Interception location: San Francisco CA
# Border Interception, Suspected country of origin: Mexico, Interception location: California, USA
# Border Interception, Suspected country of origin: Puerto Rico
# Border Interception

# So, we want to create a new column titled "intercept type" with "Border Interception" moved there.
# We also want a new column titled "suspected_origin_country" with "India" if present.
# We also want a new column titled "intercept_location" with "San Francisco, CA" if present.
# The reason we want if present is because some just have "Border Interception" and that's it.

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

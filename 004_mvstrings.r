library(tidyverse)

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
siru2 <- siru %>%
  mutate(
    rmborder = str_extract(col, "(?<=Border Interception,).*")
  ) # This will remove "Border Interception," and replace "Border Interception" with an NA

siru3 <- siru2 %>%
  mutate(
     rmsuspected = str_extract(col, "(?<=Suspected country of origin: ).*")
  )

siru4 <- siru3 %>%
  mutate(
    rminterceptloc = str_extract(col, "(?<=Interception location: ).*")
  )

siru5 <- siru4 %>%
  mutate(
    rminterceptnum = str_extract(col, "(?<=Interception Number: ).*")
  )

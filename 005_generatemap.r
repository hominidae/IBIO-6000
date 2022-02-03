# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# Install some packages if you don't already have 'em
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('rayshader')
install.pacakges('rayrender')
install.packages('sf')
install.packages('ggmap')

# Load libraries
library(sf)
library(rayrender)
library(rayshader)
library(ggplot2)
library(tidyverse)
library(maps)
library(mapdata)
library(ggmap)
library(dplyr)

# Read the csv containing data into working_copy
working_copy <- read_csv("E:/2021_UoG/IBIO 6000/src/Data/working_copy.csv")

# We're going to need a much more simplified version of the json used in the geojson.r example.

# So instead of using the sumbarine cable map JSON, we're going to use our own points.
# We'll need GPS coords for US states and GPS coords for countries

# We can use the built-in state package for GPS coords
# Let's put the two letter state abbreviation, state name, and GPS center in a dataframe
states <- data.frame(state.abb, state.name,state.center)
# but let's rename x, Y to long, lat
names(states) <- c("state.abb","state.name","state.long","state.lat")

# We also need to add Puerto Rico though, even if it isn't an official US state; it is represented in our data.
# We need a two letter code, "PR, Puerto Rico, -66.4314, 18.2270
# We also might need the US Virgin Islands, "VI", US Virgin Islands, -64.8671,18.3434

# To do that, we need to create a new dataframe containing the information for Puerto Rico
notrealstate <- data.frame("PR", "Puerto Rico","-66.4314","18.2270")
names(notrealstate) <- c("state.abb", "state.name","state.long","state.lat")

# Append it to the end of the states dataframe
states <- rbind(states,notrealstate)

# Next we need data on countries
# We'll use the available data from map_data
# I found this open dataset from Google.
# https://developers.google.com/public-data/docs/canonical/countries_csv
# Copy it into an excel file and save it as a csv.
countries <- read_csv("E:/2021_UoG/IBIO 6000/src/Data/countries.csv")

# The issue with map.world is that it draws regions rather than geographic centers.
# So let's solve that by 

# Let's work with'what we have so far. To make things simple so that the code below executes, we need data that looks like the existing cables dataframe from the geojson.r code used in TylerMorganWall's example code.
# So we need to create a dataframe from existing data with six columns
# Column 2 - geometry, E.g. "list(c(long, long, long, long, lat, lat, lat, lat))"
# Column 5 - color, E.g. "#939597"

# So, let's make this simple. Let's populate an alternative to cables with a source and destination inside of a list and that's it.
# It'll end up drawing over countries but that should be fine.
# source being country_origin, destination being state_intercept
# From those two, we combine the source to the destination into a list for each record

# We'll need to generate a new dataframe containing the country name, and it's latitude, and longitude
# match() will be used to match country name from country_origin to a set of GPS coordinates
# Example: new_df <- source.item[match(itemsinthislist,tothatlist)]

# oddly enough, I ran into an error. So let's try manually copying vector lists from working_copy
sourcesnotdistinct <- data.frame(working_copy$country_origin)
destinationsnotdistinct <- data.frame(working_copy$intercept_state)

# There are some errors from sources that we need to fix first.
# Some examples:
# "Viet Nam" to Vietnam
# "United King of Great Britain and N. Ireland" to "Great Britain"
# "The United States of America" to "United States"
# "US Virgin Islands" to "U.S. Virgin Islands"
# "Democratic Republic of Congo" to "Congo [DRC]"
names(sourcesnotdistinct) <- c("name")
sourcesnotdistinct$name[sourcesnotdistinct == "The United States of America"] <- "United States"
sourcesnotdistinct$name[sourcesnotdistinct == "US Virgin Islands"] <- "U.S. Virgin Islands"
sourcesnotdistinct$name[sourcesnotdistinct == "Democratic Republic of Congo"] <- "Congo [DRC]"
sourcesnotdistinct$name[sourcesnotdistinct == "United Kingdom of Great Britain and N. Ireland"] <- "Great Britain"

# Before moving on, noticed an issue. The United States cannot be the source to the United States.
# Determine the cause of that.

# Turn dataframe back into a vector

# Next, remove any NA's from sources, x<-x[!is.na(x)]
sourcesnotdistinct <- sourcesnotdistinct[!is.na(sourcesnotdistinct)]

# Remove any NA's from destinations
destinationsnotdistinct <- destinationsnotdistinct[!is.na(destinationsnotdistinct)]

# Next, let's take those and get rid of anything non-distinct
# unique(df[c("x1")])
sources <- unique(sourcesnotdistinct)
destinations <- unique(destinationsnotdistinct)

sourcescheck <- data.frame(sources)
destinationscheck <- data.frame(destinations)

# Next, we need to remove any entries that have no origin or no destination.
# What does this do?
sum_states <- working_copy %>%
  group_by(country_origin, family_name) %>%
  mutate(n_family = n())
# What about this?
working_copy %>% distinct(country_origin, .keep_all =TRUE) %>%
  count(intercept_state)
# What about this?
stripcopy <- working_copy %>% drop_na(country_origin, intercept_state)
# Interesting. stripcopy now has 185 records from 241 originally where there is a country_origin and intercept_state


# The code below is from TylerMorganWall's code is here:
# https://gist.github.com/tylermorganwall/b222fcebcac3de56a6e144d73d166322
cablescene = list()
counter = 1
for(i in 1:length(cables$geometry)) {
  for(j in 1:length(cables$geometry[[i]])) {
    temp = cables$geometry[[i]][[j]]
    cableval = data.frame(x=sinpi(temp[,1]/180)*cospi(temp[,2]/180),
                          y=sinpi(temp[,2]/180),
                          z=cospi(temp[,1]/180)*cospi(temp[,2]/180))
    #Don't lower start of line at the 180/0 longitude border
    if(abs(temp[1,1] - 180) > 0.001 && abs(temp[1,1] + 180) > 0.001) {
      cableval[1,] = cableval[1,] * 1/1.02
    }
    nr = nrow(temp)
    #Don't lower end of line at the 180/0 longitude border
    if(abs(temp[nr,1] - 180) > 0.001 && abs(temp[nr,1] + 180) > 0.001) {
      nr = nrow(cableval)
      cableval[nr,] = cableval[nr,] * 1/1.02
    } 
    cablescene[[counter]] = path(cableval, width = 0.005,material=diffuse(color=cables$color[i]))
    counter = counter + 1
  }
}
fullcablescene = do.call(rbind,cablescene)

for(i in seq(1,720,by=1)) {
  group_objects(fullcablescene,scale=c(1,1,1)*1.02) %>% 
    add_object(sphere(radius=0.99,material=diffuse(image_texture = "E:/2021_UoG/IBIO 6000/src/Data/2k_earth_daymap.jpg"),angle=c(0,-90,0))) %>% 
    group_objects(angle=c(0,-i/2,0)) %>% 
    add_object(sphere(y=5,z=5,x=5,material=light(intensity = 80,color="lightblue"))) %>% 
    add_object(sphere(y=5,z=5,x=-5,material=light(intensity = 10,color="orange"))) %>% 
    add_object(sphere(y=-10,material=light(intensity = 3,color="white"))) %>%
    render_scene(samples=64,width=1200,height=1200,fov=0,aperture=0, ortho_dimensions = c(2.3,2.3),
                 sample_method = "sobol_blue",filename=sprintf("smallcables%d.png",i))
}

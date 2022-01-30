# IBIO*6000 - Ecology & Behaviour
# Bryan Vandenbrink

# Install some packages if you don't already have 'em
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('rayshader')
install.pacakges('rayrender')

# Load libraries
library(rayrender)
library(rayshader)
library(ggplot2)
library(tidyverse)

# Read the csv containing data into working_copy
working_copy <- read_tsv("E:/2021_UoG/IBIO 6000/src/Data/working_copy.csv")


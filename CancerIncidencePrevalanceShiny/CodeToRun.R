library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(quarto)

# Combine result sets
# this will combine the results in the networkResults directory
# saving them in the shiny/data folder
source(here("mergeResults.R"))

# launch shiny
shiny::runApp(here("shiny"))
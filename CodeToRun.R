# Load packages ------
#renv::activate()
#renv::restore()
# to install latest version of IncidencePrevalence
#install.packages("remotes")
#remotes::install_github("darwin-eu-dev/IncidencePrevalence",force = TRUE)

# load r packages
library(CirceR)
library(IncidencePrevalence)
library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(readr)
library(log4r)
library(tidyr)
library(stringr)
library(CDMConnector)
library(ggplot2)
library(broom)
library(survival)
library(bshazard)

# database metadata and connection details -----
# The name/ acronym for the database
#db.name<-"CPRDAurum"
db.name<-"CPRDGold"
#db.name<-"CPRDAurumCovid"

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
# to set the location within the project with folder called "output, we can use: here("output")
# but this file path could be set to somewhere else
output.folder<-here("Results", db.name)

# Specify databaseConnector connection details -----
# database connection details
# connect to database
user<-Sys.getenv("DB_USER")
password<- Sys.getenv("DB_PASSWORD")
port<-Sys.getenv("DB_PORT") 
host<-Sys.getenv("DB_HOST") 
#server_dbi<-Sys.getenv("DB_SERVER_cdm_aurum_202106_dbi") #aurum
server_dbi<-Sys.getenv("DB_SERVER_cdmgold202007_dbi") #gold
#server_dbi<-Sys.getenv("DB_SERVER_p20_059_cdm_aurum_dbi") #aurum covid


# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "public"

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema <- cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema <- "results"

# Name of outcome table in the result table where the outcome cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten 
outcome_table_stem <- "cancerincprev7"

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)

# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()

# Set study details -----
# must be start of year so 1st jan 2007, 1st jan 2013 CANNOT BE MID YEAR
studyStartDate <- "2000-01-01" 
# study end data
studyEndDate <- "2019-12-31"  

# Does a user want to run survival analysis? if a database does not have mortality data change this FALSE
runSurvial <- FALSE

# Run the study ------
source(here("RunStudy.R"))
# after the study is run you should have a zip folder in your output folder to share

# disconnect from the database (only do this after you have run all analysis)
#dbDisconnect(db)
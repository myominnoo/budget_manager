

# required packages -------------------------------------------------------

library(shiny) ## TODO: comment out during deployment
library(shinyFeedback)
library(shinyWidgets)
library(tidyverse)
library(shinycssloaders)


source("R/vars.R")
source("R/utils.R")
source("R/ui_helpers.R")
source("R/server_helpers.R")

# source("R/db_helpers.R") ## no need to source this file as of now

# dbconnection ------------------------------------------------------------

## connect to mongodb server 
## added the following shinyapp.io's ip addresses to connect properly 
# 54.204.34.9
# 54.204.36.75
# 54.204.37.78
# 34.203.76.245
# 3.217.214.132
# 34.197.152.155
con <- "mongodb+srv://myself:letmein2022@budgetapp.whtgpcl.mongodb.net/?retryWrites=true&w=majority"
usersDF <- mongolite::mongo(collection = "users", db = "budgetdb", url = con, verbose = TRUE)
transDF <- mongolite::mongo(collection = "transaction", db = "budgetdb", url = con, verbose = TRUE)




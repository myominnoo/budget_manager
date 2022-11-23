

# required packages -------------------------------------------------------

library(shiny) ## TODO: comment out during deployment
library(shinyFeedback)
library(shinyWidgets)
library(tidyverse)
library(shinycssloaders)
library(lubridate)
library(DT)
library(shinydashboard)
library(formattable)
library(waiter)


source("R/infoUI.R")
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
con <- paste0("mongodb+srv://myself:letmein2022@budgetapp.whtgpcl.mongodb.net", 
              "/?retryWrites=true&w=majority")
dbconnect <- function(collection, db, url) {
  tryCatch({
    mongolite::mongo(collection, db, url, verbose = TRUE)
  }, error = function(cond) TRUE)
}


# loader ------------------------------------------------------------------

loader <- function(color = "#FFFFFF") {
  waiter::waiter_show( # show the waiter
    color = color, 
    html = spin_dots() # use a spinner
  )
  Sys.sleep(1) # do something that takes time
  waiter::waiter_hide() # hide the waiter
}

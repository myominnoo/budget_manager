

library(shiny)
library(mongolite)
library(tidyverse)

library(bslib)
library(lubridate)
library(shinyWidgets)
library(DT)
library(shinydashboard)
library(formattable)


source("utils.R", echo = TRUE)
source("DB.R", echo = TRUE)
source("UI.R", echo = TRUE)
source("SERVER.R", echo = TRUE)


source("budget_process.R", echo = TRUE)
source("ui_info.R", echo = TRUE)


# ui ----------------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(5, "sketchy"), 
  title = "Budget Manager", 
  tabsetPanel(
    type = "hidden",
    id = "tab", 
    tabPanel(
      "tab_login", 
      # h2("login tab")
    ),
    tabPanel(
      "tab_main", 
      # h2("main tab"), 
      uiOutput("main_ui")
    )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  show_login <- function() {
    showModal(modalDialog(loginFormUI, footer = loginFooterUI, size = "m"))
  }
  show_signup <- function() {
    showModal(modalDialog(signupUI, footer = signupFooterUI, size = "m"))
  }
  env <- reactiveValues(auth = NULL)
  
  show_login()
  observeEvent(input$login, {
    err <- FALSE
    user <- users_df() %>% filter(user == input$username)
    err <- validateUsers(input$username, input$password, user)
    if (err) {
      show_login()
    } else {
      env$auth <- input$username
      str(env$auth)
      updateTabsetPanel(session, "tab", "tab_main")
    }
  })
  
  observeEvent(input$exit, stopApp())
  
  observeEvent(input$showSignup, show_signup())
  
  observeEvent(input$signup_cancel, show_login())
  
  observeEvent(input$signup, {
    err <- FALSE
    err <- check_empty(input$signup_un, input$signup_pw)
    if (any(input$signup_un %in% users_df()$user)) {
      showErrorModal("Hmm, Username is not available. Try something else!", 
                     title = "Invalid Username !")
      err <- TRUE
    } else {
      err <- FALSE
    }
    if (err) {
      show_signup()
    } else {
      showErrorModal("Yay, now you can log in !", title = "Come on !")
      write_signup(input$signup_un, input$signup_pw)
      removeModal()
      show_login()
    }
  })
  
  output$main_ui <- renderUI(mainUI)
  observeEvent(input$logout, session$reload())
  
  

# import ------------------------------------------------------------------
  show_import <- function() {
    showModal(modalDialog(importUI, footer = importFooterUI, size = "m"))
  }
  observeEvent(input$showImport, {
    show_import()
  })
  
  observeEvent(input$import, {
    str(input$upload)
    req(is.null(input$upload))
    df <- rio::import(input$upload$datapath) %>% 
      set_df() %>% 
      mutate(created_date = c(Sys.time() - 1:nrow(.)), user = env$auth) %>%
      select(created_date, user, everything())
    df %>% 
      trans$insert()
    showErrorModal(" ", nrow(df), " records have been imported!", "You Rock !")
    trans$find(sprintf('{"user": "%s"}', env$auth)) %>%  
      tibble() %>% 
      str()
  })
    
}




# run ---------------------------------------------------------------------

shinyApp(ui, server)


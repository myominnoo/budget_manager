



# R packages --------------------------------------------------------------

# library(shiny)
library(bslib)
library(lubridate)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(shinydashboard)
library(formattable)
library(mongolite)


source("budget.R")
source("infoUI.R")
source("utils.R")

# dbconnection ------------------------------------------------------------

con <- "mongodb+srv://myself:letmein2022@budgetapp.whtgpcl.mongodb.net/?retryWrites=true&w=majority"
users <- mongolite::mongo(collection = "users", db = "budgetdb", url = con, verbose = TRUE)
users_df <- function() users$find() %>% tibble()

trans <- mongolite::mongo(collection = "transaction", db = "budgetdb", url = con, verbose = TRUE)






# login ui ----------------------------------------------------------------

loginFormUI <- tagList(
  h2(style="text-align:center;", "Myo's Personal Budget Manager"), 
  hr(), 
  p(style = "text-align:center", "LOGIN"), 
  fluidRow(
    column(3), 
    column(6, textInput("username", "Username", width = "100%"))
  ), 
  fluidRow(
    column(3), 
    column(6, passwordInput("password", "Password", width = "100%"))
  )
) 

loginFooterUI <- tagList(actionButton("login", "Login"),
                         actionButton("showSignup", "Sign Up"), 
                         actionButton("exit", "Exit"))

# signup ui ---------------------------------------------------------------

signupUI <- tagList(
  h2(style="text-align:center;", "Myo's Personal Budget Manager"), 
  hr(), 
  p(style = "text-align:center", "JOIN ME !"), 
  fluidRow(
    column(3), 
    column(6, textInput("signup_un", "Username", width = "100%"))
  ), 
  fluidRow(
    column(3), 
    column(6, passwordInput("signup_pw", "Password", width = "100%"))
  )
) 

signupFooterUI <- tagList(actionButton("signup", "SignUp"), 
                          actionButton("signup_cancel", "Cancel"))



# transaction UI ----------------------------------------------------------

newTransUI <- tagList(
  dropdownButton(
    circle = FALSE, 
    icon = icon("square-plus"),
    dateInput("date", "Date", Sys.Date(), width = 100), 
    selectInput("category", "Category", categories_list), 
    textAreaInput("description", "Description"), 
    radioGroupButtons("account", "Account", account_fct, justified = TRUE), 
    radioGroupButtons("type", "Type", type_fct, justified = TRUE), 
    numericInputIcon("amount", "Amount", 0, 0, icon = icon("dollar-sign")), 
    textAreaInput("note", "Note"), 
    hr(), 
    div(style="display:inline-block;float:right", 
        actionBtn("add", "Add", icon = icon("plus")))
  )
)

transUI <- tabPanel(
  "Transaction", 
  icon = icon("cash-register"), 
  fluidRow(
    column(2, newTransUI), 
    column(4), 
    column(3, actionBtn("delete", "Delete", icon = icon("xmark"), width = "100%")), 
    column(3, actionBtn("undo", "Undo", icon = icon("rotate-left"), width = "100%"))
  ), 
  hr(), 
  DT::dataTableOutput("data")
)


# sidebar UI --------------------------------------------------------------

sidebar <- tagList(
  h2(HTML("<b>Myo's Personal Budget Manager</b>")), 
  p("version 1.2.0", style = "font-size:10px;"), 
  hr(),
  numericInput("year", "Year", year(Sys.Date()), width = 75), 
  sliderTextInput("month", "Month", month.abb, 
                  month(Sys.Date(), label = TRUE), 
                  grid = TRUE, hide_min_max = TRUE), 
  hr(), 
  fluidRow(downloadButton("download", "Save Data")), 
  br(), 
  fluidRow(downloadButton("download_template", "Download Template", 
                          icon = icon("file-excel"))), 
  hr(), 
  p(textOutput("currentTime")), 
  p(textOutput("user_name"))
)


# floating widgets --------------------------------------------------------

settingUI <- tagList(
  h2(style="text-align:center;", "Myo's Personal Budget Manager"), 
  hr(), 
  p(style = "text-align:center", "Update Username and Password"), 
  fluidRow(
    column(3), 
    column(6, textInput("update_username", "Username", width = "100%"))
  ), 
  fluidRow(
    column(3), 
    column(6, passwordInput("update_old", "Type your old password", width = "100%"))
  ), 
  fluidRow(
    column(3), 
    column(6, passwordInput("update_new", "Type your new password", width = "100%"))
  )
) 

settingFooterUI <- tagList(actionButton("change", "Change"),
                           modalButton("Cancel"))


importBody <- tagList(
  h2(style="text-align:center;", "Import Your Data"), 
  hr(), 
  fileInput("upload", "Select a file")
) 

importFooter <- tagList(actionBtn("import", "Import", icon("upload")),
                        modalButton("Cancel"))


fabBtn <- shinymanager::fab_button(
  position = "bottom-right", 
  actionBtn("setting", "Setting", icon("gear")),
  actionBtn("sync", "Sync", icon("rotate")),
  actionBtn("logout", "Logout", icon("sign-out")),
  actionBtn("showImport", "Import data", icon("upload"))
)

# summy UI ----------------------------------------------------------------

summaryUI <- tabPanel(
  "Summary", 
  icon = icon("chart-column"), 
  hr(), 
  fluidRow(
    valueBoxOutput("cheque_balance"), 
    valueBoxOutput("savings_balance"), 
    valueBoxOutput("visa_balance"), 
  ), 
  hr(), 
  fluidRow(
    column(8, set_text_tags("balance_this_month_lbl"), 
           textInput("balance_this_month_lbl", label = NULL,
                     value = "Balance", width = "100%") %>% 
             shinyjs::disabled(), 
           formattableOutput("balance_this_month")), 
    valueBoxOutput("savings_this_month")
  ), 
  hr(), 
  fluidRow(
    column(6, set_text_tags("expense_lbl"), 
           textInput("expense_lbl", label = NULL,
                     value = "Expense", width = "100%") %>% 
             shinyjs::disabled(), 
           formattableOutput("expense")), 
    column(6, set_text_tags("income_lbl"), 
           textInput("income_lbl", label = NULL,
                     value = "Income", width = "100%") %>% 
             shinyjs::disabled(), 
           formattableOutput("income"))
  )
)

body <- tagList(
  tabsetPanel(
    type = "pills", 
    summaryUI,
    transUI,
    infoUI
  )
)

mainUI <- tagList(
  br(), 
  fabBtn, 
  sidebarLayout(sidebarPanel(sidebar), mainPanel(body))
)


# main UI -----------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(5, "sketchy"), 
  title = "Budget Manager", 
  tabsetPanel(
    id = "tab", 
    type = "hidden", 
    tabPanel(
      "tab_login"
    ),
    tabPanel(
      "tab_main", 
      uiOutput("mainUI")
    )
  )
)


# main server -------------------------------------------------------------

server <- function(input, output, session) {
  
  ## display time 
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Current time : ", Sys.time())
  })
  ## display user name 
  output$user_name <- renderText(paste0("Username : ", env$auth))
  
  ## create reactive values to track variables
  env <- reactiveValues(
    auth = NULL, user = NULL,  
    data = NULL, undo = NULL, dummy = NULL, 
    balance = NULL, fltr = NULL, this_month = NULL, 
    expense = NULL, income = NULL
  )
  
  # login -------------------------------------------------------------------
  show_login <- function() {
    showModal(modalDialog(loginFormUI, footer = loginFooterUI, size = "m"))
  }
  show_signup <- function() {
    showModal(modalDialog(signupUI, footer = signupFooterUI, size = "m"))
  }  
  
  show_login()
  observeEvent(input$login, {
    err <- FALSE
    env$user <- users_df() %>% filter(user == input$username)
    err <- validateUsers(input$username, input$password, env$user)
    if (err) {
      show_login()
    } else {
      env$auth <- input$username
      updateTabsetPanel(session, "tab", "tab_main")
    }
  })
  
  
  
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
  
  
  # generate main UI --------------------------------------------------------
  
  ## generate app's main UI 
  output$mainUI <- renderUI(mainUI)
  
  df <- eventReactive(env$auth, {
    d <- trans$find(sprintf('{"user":"%s"}', env$auth)) 
    if (nrow(d) > 0) {
      d %>% 
        tibble() %>%
        set_df()
    } else {
      df_na %>% 
        mutate(created_date = Sys.time(), 
               user = env$auth, 
               id = paste(created_date, user), 
               Note = ifelse(is.na(NA), "", Note)) %>%  
        select(Date:Note, everything()) %>% 
        set_df()
    }
  })
  observe({env$undo <- env$data <- df()})
  
  dummy <- reactive({
    generate_dummy(as.Date(paste0("1", input$month, input$year), "%d%B%Y"), env$auth)
  })
  
  
  observe({
    req(env$data)
    # update monthly balance label
    updateTextInput(session = session, "balance_this_month_lbl", value = paste0(
      input$year, " ", input$month, "'s Balance"
    ))
    updateTextInput(session = session, "expense_lbl", value = paste0(
      input$year, " ", input$month, "'s Expense"
    ))
    updateTextInput(session = session, "income_lbl", value = paste0(
      input$year, " ", input$month, "'s Income"
    ))
    
    env$balance <- suppressMessages(suppressWarnings(
        try(get_total_balance(env$data, dummy()), silent = TRUE)
      ))
    env$fltr <- suppressMessages(suppressWarnings(
      try(filter_df(env$data, dummy(), input$month, input$year), silent = TRUE)
    )) 
      
    env$this_month <- suppressMessages(suppressWarnings(try(get_this_month(env$fltr), 
                                                            silent = TRUE)))
    env$expense <- suppressMessages(suppressWarnings(try(get_expense(env$fltr), 
                                                         silent = TRUE)))
    env$income <- suppressMessages(suppressWarnings(try(get_income(env$fltr), 
                                                        silent = TRUE)))
  })
  
  
  # summary -----------------------------------------------------------------
  
  output$cheque_balance <- renderValueBox({
    val <- ifelse(is.null(env$balance), 0, set_balance(env$balance, "Cheque"))
    valueBox(set_color(val), "Cheque", icon = icon("money-check"), color = "yellow")
  })
  output$savings_balance <- renderValueBox({
    val <- ifelse(is.null(env$balance), 0, set_balance(env$balance, "Savings"))
    valueBox(set_color(val), "Savings", icon = icon("piggy-bank"), color = "green")
  })
  output$visa_balance <- renderValueBox({
    val <- ifelse(is.null(env$balance), 0, set_balance(env$balance, "Visa"))
    valueBox(set_color(val), "Visa", icon = icon("cc-visa"), color = "red")
  })
  
  output$balance_this_month <- renderFormattable({show_this_month(env$this_month)})
  output$savings_this_month <- renderValueBox({savings_this_month(env$this_month)})
  output$expense <- renderFormattable({show_expense(env$expense)})
  output$income <- renderFormattable({show_income(env$income)})
  
  
  # Transaction -------------------------------------------------------------
  output$data <- renderDataTable({
    req(env$data)
    env$data <- env$data %>% arrange(desc(Date))
    show_datatable(env$data)
  })
  
  ## edit event 
  observeEvent(input$data_cell_edit, {
    info <- input$data_cell_edit
    cat("edits made in row [", info$row, "] & col [", info$col, 
        "] with value [", info$value, "]\n")
    env$data[info$row, info$col + 1] <- info$value
  })
  
  ## delete event
  observeEvent(input$delete, {
    req(env$data)
    if (is.null(input$data_rows_selected)) {
      show_noti_modal(title = "Error", "Oups ! Select some rows !")
    } else {
      env$undo <- env$data
      env$data <- env$data[-input$data_rows_selected, ] 
      show_noti_modal(title = "Be careful!", "You just deleted ", 
                      length(input$data_rows_selected), " records !")
    }
  })
  
  ## undo event 
  observeEvent(input$undo, {env$data <- env$undo})
  
  ## add new event 
  observeEvent(input$add, {
    req(env$data)
    if (input$amount == 0) {
      show_noti_modal(title = "Invalid", "Hmm, Amount is zero. Try to increase it!")
    } else {
      env$data <- env$data %>% 
        bind_rows(
          data.frame(
            Date = input$date, 
            Category = factor(input$category, categories_fct),
            Description = input$description, 
            Account = factor(input$account, account_fct),
            Type = factor(input$type, type_fct), 
            Amount = as.numeric(input$amount), 
            Note = input$note, 
            created_date = Sys.time(), 
            user = env$auth
          ) %>% 
            mutate(id = paste(created_date, user)) 
        ) %>% 
        set_df()
      show_noti_modal(title = "You rock!", "Whoop-de-doo ! You just added an entry !")
    }
    
    updateDateInput(session = session, "date", value = Sys.Date())
    updateSelectInput(session = session, "category", selected = "Housing")
    updateTextAreaInput(session = session, "description", value = "")
    updateRadioGroupButtons(session = session, "account", selected = "Cheque")
    updateRadioGroupButtons(session = session, "type", selected = "Debit")
    updateNumericInputIcon(session = session, "amount", value = 0)
    updateTextAreaInput(session = session, "note", value = "")
  })
  
  # floating widget servers -------------------------------------------------
  observeEvent(input$logout, {session$reload()})
  observeEvent(input$exit, {stopApp()})
  observeEvent(input$showImport, {
    show_modal(importBody, footer = importFooter)
  })
  observeEvent(input$import, {
    if (is.null(input$upload)) {
      show_noti_modal("Try to select a file!", title = "Oups !")
      show_modal(importBody, footer = importFooter)
    }
    
    req(input$upload)
    d <- import_process(input$upload$datapath, env$auth) 
    show_noti_modal("You just added ", nrow(d), " records", title = "Whoo-de-doo !")
    env$data <- env$data %>% 
      bind_rows(d) 
  })
  
  sync <- function() {
    trans$remove(sprintf('{"user":"%s"}', env$auth))
    env$data %>%
      select(Date:id) %>% 
      trans$insert()
  }
  observeEvent(input$sync, {
    sync()
    show_noti_modal("You just synced your data !", title = "Yike !! ")
  })
  
  ## setting page 
  show_setting <- function() {
    showModal(modalDialog(settingUI, footer = settingFooterUI, size = "m"))
  }
  observeEvent(input$setting, {
    show_setting()
    updateTextInput(session, "update_username", value = env$user$user)
  })
  observeEvent(input$change, {
    err <- validateUsers(env$user$user, input$update_old, env$user)
    if (err) {
      show_setting()
    } else {
      trans$remove(sprintf('{"user":"%s"}', env$auth))
      users$remove(sprintf('{"user":"%s"}', env$user$user))
      env$user$user <- env$user$name <- env$data$user <- env$auth <- input$update_username
      env$user$password <- scrypt::hashPassword(input$update_new)
      env$user %>% users$insert()
      trans$remove(sprintf('{"user":"%s"}', env$auth))
      env$data %>% trans$insert()
      show_noti_modal("Your username and password are successfully updated!", 
                      title = "Good luck remembering them !")
    }
  })
  
  output$settingUI <- renderUI(settingUI)
  
  
  
  # download ----------------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {"budget.xlsx"},
    content = function(file) rio::export(env$data %>% select(Date:Note), file)
  )
  output$download_template <- downloadHandler(
    filename = function() {"budget_template.xlsx"},
    content = function(file) rio::export(df_na, file)
  )
}

shinyApp(ui, server)







library(shiny)
library(bslib)
library(lubridate)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(shinydashboard)
library(formattable)

source("budget_process.R", echo = TRUE)

# functions ---------------------------------------------------------------

set_df <- function(df) {
  df %>% 
    mutate(
      Date = as.Date(Date), 
      Category = factor(Category, categories_fct), 
      Description = as.character(Description), 
      Account = factor(Account, account_fct), 
      Type = factor(Type, type_fct), 
      Amount = as.numeric(Amount), 
      Note = as.character(Note), 
      mnth = month(Date, label = TRUE),
      mnth = as.character(mnth), 
      yr = year(Date)
    )
}

show_datatable <- function(df) {
  df %>% 
    datatable(
      rownames = FALSE, 
      editable = TRUE, 
      selection = "multiple", 
      filter = list(position = "top", clear = FALSE), 
      options = list(scrollX = TRUE)
    )
}

show_modal <- function(title, ... ) {
  showModal(modalDialog(
    title = title, 
    ..., 
    easyClose = TRUE, 
    size = "s"
  ))
}

set_color <- function(val) {
  if (val < 0) 
    p(paste0(sprintf("%.2f", val), " $"), style = "color:red")
  else if (val == 0)
    p(paste0(sprintf("%.2f", val), " $"), style = "color:black")
  else 
    p(paste0("+", sprintf("%.2f", val), " $"), style = "color:green")
}


set_balance <- function(df, account) {
  df %>% 
    filter(.data$Account == account) %>% 
    pull(diff) 
}


sign_formatter <- formatter(
  "span",
  style = x ~ style(color = ifelse(x > 0, "green", ifelse(x < 0, "red", "black"))),
  x ~ currency(x, symbol = "$ ", digits = 2)
)
sign_formatter_red <- formatter(
  "span",
  style = x ~ style(color = ifelse(x > 0, "red", "black")),
  x ~ currency(x, symbol = "$ ", digits = 2)
)
sign_formatter_green <- formatter(
  "span",
  style = x ~ style(color = ifelse(x > 0, "green", "black")),
  x ~ currency(x, symbol = "$ ", digits = 2)
)


df_fltr <- function(df, month, year) {
  df %>% 
    filter(.data$mnth == month, .data$yr == year)
}

set_text_tags <- function(x) {
  tags$style(paste0(
    "#", x, " {border: 0px solid #dd4b39; color: #FFFFFF; font-size: 20px;", 
    " text-align:center; font-weight: bold; background-color: #18191A}"
  ))
}


# ui ----------------------------------------------------------------------

source("ui_info.R", echo = TRUE)

# sidebar -----------------------------------------------------------------

sidebar <- sidebarPanel(
  h2(HTML("<b>Myo's Personal Budget Manager</b>")), 
  p("version 1.0.0", style = "font-size:10px;"), 
  hr(),
  
  fileInput("upload", "Select your budget file", accept = ".xlsx"), 
  hr(), 
  numericInput("year", "Year", year(Sys.Date()), width = 75), 
  sliderTextInput("month", "Month", month.abb, 
                  month(Sys.Date(), label = TRUE), 
                  grid = TRUE, hide_min_max = TRUE), 
  hr(), 
  fluidRow(downloadButton("download", "Save Data")), 
  br(), 
  fluidRow(downloadButton("download_template", "Download Template", 
                          icon = icon("file-excel")))
)

# body --------------------------------------------------------------------

ui_summary <- tabPanel(
  "Summary", 
  icon = icon("chart-column"), 
  
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

ui_add_trans <- dropdownButton(
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
      actionButton("add", "Add", icon = icon("plus")))
)

ui_transaction <- tabPanel(
  "Transaction", 
  icon = icon("cash-register"), 
  fluidRow(
    column(2, ui_add_trans), 
    column(4), 
    column(3, actionButton("delete", "Delete", 
                           icon = icon("xmark"), width = "100%")), 
    column(3, actionButton("undo", "Undo", 
                           icon = icon("rotate-left"), width = "100%"))
  ), 
  hr(), 
  DT::dataTableOutput("data")
)

body <- mainPanel(
  tabsetPanel(
    type = "pills", 
    ui_summary, 
    ui_transaction, 
    ui_info
  )
)


# all ui ------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "sketchy"), 
  br(), 
  sidebarLayout(sidebar, body)
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {

# data import -------------------------------------------------------------
  df <- reactive({
    req(input$upload)
    rio::import(input$upload$datapath)
  })
  
  ## create dummy for categorizations
  dummy <- reactive({
    req(d$data)
    tibble(
      Date = as.Date(paste0("1", input$month, input$year), "%d%B%Y"), 
      Category = categories_fct, 
      Account = c(account_fct, rep(NA, 12)),
      Type = c(type_fct, rep(NA, 13))
    ) %>% 
      expand(Date, Category, Account, Type) %>% 
      mutate(
        Description = NA, 
        Amount = 0, 
        Note = NA
      ) %>% 
      set_df()
  })
  
  ## objects to store reactive values
  d <- reactiveValues(data = NULL, undo = NULL, show = NULL,
                      balance = NULL, fltr = NULL, this_month = NULL, 
                      expense = NULL, income = NULL)
  observe({d$undo <- d$data <- set_df(df())})
  observe({
    req(d$data)
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
    
    d$balance <- d$data %>% 
      bind_rows(dummy()) %>%
      group_by(Account, Type) %>%
      summarize(amount = sum(Amount, na.rm = TRUE)) %>%
      drop_na(Account, Type) %>%
      pivot_wider(names_from = Type, values_from = amount) %>%
      mutate(across(c(Credit, Debit), ~ ifelse(is.na(.x), 0, .x)),
             diff = Credit - Debit)
    d$fltr <- d$data %>%  
      bind_rows(dummy()) %>%
      filter(mnth == input$month, yr == input$year)
  })
  
  observe({
    req(d$fltr)
    d$this_month <- d$fltr %>% 
      group_by(Account, Type) %>%
      summarize(amount = sum(Amount, na.rm = TRUE)) %>%
      drop_na(Account, Type) %>% 
      pivot_wider(names_from = Type, values_from = amount) %>%
      mutate(across(c(Credit, Debit), ~ ifelse(is.na(.x), 0, .x)),
             Surplus = Credit - Debit) %>%
      pivot_longer(-1, names_to = "Type") %>%
      pivot_wider(names_from = Account, values_from = value)
    d$expense <- d$fltr %>% 
      filter(Type == "Debit") %>% 
      filter(Category %in% expense_fct) %>% 
      group_by(Category) %>% 
      summarize(Amount = sum(Amount, na.rm = TRUE)) 
    d$income <- d$fltr %>% 
      filter(Category %in% income_fct) %>% 
      group_by(Type, Category) %>% 
      summarize(Amount = sum(Amount, na.rm = TRUE)) %>% 
      drop_na(Type, Category) %>% 
      pivot_wider(names_from = Type, values_from = Amount) %>% 
      mutate(across(-Category, ~ ifelse(is.na(.x), 0, .x))) %>% 
      mutate(Credit = Credit - Debit) %>% 
      select(Category, Credit) %>% 
      rename(Amount = Credit)
  })
    
  

# summary -----------------------------------------------------------------

  output$cheque_balance <- renderValueBox({
    val <- ifelse(is.null(d$balance), 0, set_balance(d$balance, "Cheque"))
    valueBox(set_color(val), "Cheque", icon = icon("money-check"), color = "yellow")
  })
  output$savings_balance <- renderValueBox({
    val <- ifelse(is.null(d$balance), 0, set_balance(d$balance, "Savings"))
    valueBox(set_color(val), "Savings", icon = icon("piggy-bank"), color = "green")
  })
  output$visa_balance <- renderValueBox({
    val <- ifelse(is.null(d$balance), 0, set_balance(d$balance, "Visa"))
    valueBox(set_color(val), "Visa", icon = icon("cc-visa"), color = "red")
  })

  output$balance_this_month <- renderFormattable({
    if (is.null(d$this_month)) {
      e <- data.frame(
        Type = c("Credit", "Debit", "Surplus"), 
        Cheque = rep(0, 3),
        Savings = rep(0, 3),
        Visa = rep(0, 3)
      ) 
      
    } else {
      e <- d$this_month
    }
    formattable(e, align = c("l", "r", "r", "r"), list(
      area(col = Cheque:Visa, row = 1) ~ sign_formatter_red, 
      area(col = Cheque:Visa, row = 2) ~ sign_formatter_green, 
      area(col = Cheque:Visa, row = 3) ~ sign_formatter
    ))
  })
  
  output$savings_this_month <- renderValueBox({
    if (is.null(d$this_month)) {
      val <- 0
    } else {
      val <- d$this_month %>% 
        mutate(this_month = Cheque + Savings) %>%  
        filter(Type == "Surplus") %>% 
        pull(this_month)
    }
    valueBox(set_color(val), "Saved this month", icon = icon("money-check-dollar"), 
             width = 12, color = "yellow")
  })
  
  output$expense <- renderFormattable({
    if (is.null(d$expense)) {
      e <- data.frame(Category = expense_fct, Amount = 0) %>% 
        mutate(Category = factor(Category, expense_fct)) %>% 
        group_by(Category) %>% 
        summarise(Amount = sum(Amount))   
    } else {
      e <- d$expense
    }
    e <- e %>% 
      add_row(Category = "Total", Amount = sum(.$Amount)) 
    formattable(e, align = c("l", "r"), list(
        area(col = Amount) ~ sign_formatter_red
      ))
  })
  
  output$income <- renderFormattable({
    if (is.null(d$income)) {
      e <- data.frame(Category = account_fct, Amount = 0) %>% 
        mutate(Category = factor(Category, account_fct)) %>% 
        group_by(Category) %>% 
        summarise(Amount = sum(Amount))   
    } else {
      e <- d$income
    }
    e <- e %>% 
      add_row(Category = "Total", Amount = sum(.$Amount)) 
    formattable(e, align = c("l", "r"), list(
      area(col = Amount) ~ sign_formatter_green
    ))
  })
  
  

# Transaction -------------------------------------------------------------
  output$data <- renderDataTable({
    req(d$data)
    d$data <- d$data %>% arrange(desc(Date))
    show_datatable(d$data %>% select(Date:Note))
  })
  
  ## edit event 
  observeEvent(input$data_cell_edit, {
    info <- input$data_cell_edit
    cat("edits made in row [", info$row, "] & col [", info$col, 
        "] with value [", info$value, "]\n")
    d$data[info$row, info$col + 1] <- info$value
  })
  
  ## delete event
  observeEvent(input$delete, {
    req(d$data)
    str(input$data_rows_selected)
    if (is.null(input$data_rows_selected)) {
      show_modal("Error", "Oups ! Select some rows !")
    } else {
      d$undo <- d$data
      d$data <- d$data[-input$data_rows_selected, ] 
      show_modal("Be careful!", "You just deleted ", 
                 length(input$data_rows_selected), " records !")
    }
  })
  
  ## undo event 
  observeEvent(input$undo, {d$data <- d$undo})
  
  ## add new event 
  observeEvent(input$add, {
    req(d$data)
    if (input$amount == 0) {
      show_modal("Invalid", "Hmm, Amount is zero. Try to increase it!")
    } else {
      d$data <- d$data %>% 
        bind_rows(
          data.frame(
            Date = input$date, 
            Category = factor(input$category, categories_fct),
            Description = input$description, 
            Account = factor(input$account, account_fct),
            Type = factor(input$type, type_fct), 
            Amount = as.numeric(input$amount), 
            Note = input$note
          )
        ) %>% 
        set_df()
      show_modal("You rock!", "Whoop-de-doo ! You just added an entry !")
    }
    
    updateDateInput(session = session, "date", value = Sys.Date())
    updateSelectInput(session = session, "category", selected = "Housing")
    updateTextAreaInput(session = session, "description", value = "")
    updateRadioGroupButtons(session = session, "account", selected = "Cheque")
    updateRadioGroupButtons(session = session, "type", selected = "Debit")
    updateNumericInputIcon(session = session, "amount", value = 0)
    updateTextAreaInput(session = session, "note", value = "")
  })

# download ----------------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {input$upload$name},
    content = function(file) rio::export(d$data %>% select(Date:Note), file)
  )
  output$download_template <- downloadHandler(
    filename = function() {"budget_template.xlsx"},
    content = function(file) rio::export(df_na, file)
  )
}

# run ---------------------------------------------------------------------

shinyApp(ui, server)

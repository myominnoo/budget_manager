
# sidebar -----------------------------------------------------------------

sidebar <- sidebarPanel(
  h2(HTML("<b>Myo's Personal Budget Manager</b>")), 
  p("version 1.0.0", style = "font-size:10px;"), 
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



# floating ui -------------------------------------------------------------

fabBtn <- shinymanager::fab_button(
  actionButton(
    inputId = "logout",
    label = "Logout",
    icon = icon("sign-out")
  ),
  actionButton(
    inputId = "showImport",
    label = "Import",
    icon = icon("upload")
  ),
  inputId = "fab"
)


# main ui -----------------------------------------------------------------


mainUI <- tagList(
  br(), 
  fabBtn, 
  sidebarLayout(sidebar, body)
)




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



# import ui ---------------------------------------------------------------


importUI <- tagList(
  h2(style="text-align:center;", "Import Your Data"), 
  hr(), 
  p(style = "text-align:center", "You need an excel template!"), 
  fileInput("upload", "Select your budget file", accept = ".xlsx"), 
) 

importFooterUI <- tagList(actionButton("import", "Import"),
                          modalButton("Cancel"))


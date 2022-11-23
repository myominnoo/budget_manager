
# signup ui ---------------------------------------------------------------

showSignup <- function() {
  showModal(modalDialog(signupUI, footer = signupFooterUI, size = "m"))
}  


loginForm <- function(name, label, n, offset, text, password = FALSE) {
  if (password) {
    column(n, passwordInput(
      name, label, placeholder = text, width = "100%"
    ), offset = offset)
  } else {
    column(n, textInput(
      name, label, placeholder = text, width = "100%"
    ), offset = offset)
  }
}

signupUI <- tagList(
  h2(HTML("<strong>Budget Manager App</strong>"),
     style="text-align:center;"), hr(),
  h4(HTML("<strong>Join me</strong>"), style="text-align:center;"),
  br(),
  fluidRow(
    loginForm("signup_un", "Username", 5, 4, "Enter your username"), 
    loginForm("signup_pw", "Password", 5, 4, "Enter your password", password = TRUE)
  )
)
signupFooterUI <- tagList(
  actionButton("signup", "SignUp"),
  actionButton("cancelSignup", "Cancel")
)



# login ui ----------------------------------------------------------------

showLogin <- function(id) {
  showModal(modalDialog(loginBodyUI, footer = loginFooterUI, size = "m"))
}

loginBodyUI <- tagList(
  h2(HTML("<strong>Budget Manager App</strong>"),
     style="text-align:center;"), hr(),
  h4(HTML("<strong>Log in</strong>"), style="text-align:center;"),
  br(),
  fluidRow(
    loginForm("username", "Username", 5, 4, "Enter your username"), 
    loginForm("password", "Password", 5, 4, "Enter your password", password = TRUE)
  )
) 

loginFooterUI <- tagList(
  actionButton("login", "Login", icon = icon("right-to-bracket")),
  actionButton("showSignup", "Sign Up", icon = icon("user-plus")), 
  actionButton("help", "Help", icon = icon("circle-info")), 
  actionButton("exit", "Exit", icon = icon("door-open"))
)


# transaction ui ----------------------------------------------------------

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
        actionButton("add", "Add", icon = icon("plus")))
  )
)

importUI <- function() {
  showErrorModal(
    fluidRow(
      column(2), 
      column(8, align = "center", fileInput("upload", "Select a file"))
    ), 
    footer = tagList(actionButton("import", "Import", icon("upload")),
                     modalButton("Cancel")), 
    title = "Import Your Data", size = "m", easyClose = FALSE
  )
}

transUI <- tabPanel(
  "Transaction", 
  icon = icon("cash-register"), 
  fluidRow(
    column(3, newTransUI), 
    column(9, align = "right", 
           actionButton("delete", "Delete", icon = icon("xmark"), width = 100), 
           actionButton("undo", "Undo", icon = icon("rotate-left"), width = 100), 
           actionButton("sync", "Sync", icon("rotate"), width = 100))
  ), 
  hr(), 
  DT::dataTableOutput("data")
)


# summy UI ----------------------------------------------------------------

summaryUI <- tabPanel(
  "Summary", 
  icon = icon("chart-column"), hr(), 
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


# body ui - mainPanel ui --------------------------------------------------

bodyUI <- tagList(
  tabsetPanel(
    type = "pills", 
    summaryUI,
    transUI,
    infoUI
  )
)

# sidebar ui --------------------------------------------------------------

sidebarUI <- tagList(
  h2(HTML("<b>Budget Manager App</b>")), 
  p(HTML("version 1.2.0 <br>"), style = "font-size:10px;"), hr(), 
  numericInput("year", "Year", lubridate::year(Sys.Date()), width = 100), 
  sliderTextInput("month", "Month", month.abb, 
                  lubridate::month(Sys.Date(), label = TRUE), 
                  grid = TRUE, hide_min_max = TRUE), hr(),
  fluidRow(downloadButton("download", "Save Data")), 
  br(), 
  fluidRow(downloadButton("download_template", "Download Template", 
                          icon = icon("file-excel"))), 
  hr(), 
  p(textOutput("display_username"), style = "font-size:14px;"), 
  p(textOutput("display_currentTime"), style = "font-size:14px;"), br(), 
  p(HTML("Developed & maintained by Myo Minn Oo <br>", 
         "If you use it, please share an acknowledgement email to ", 
         "dr.myominnoo@gmail.com!"),
    style = "font-size:10px;") 
)

# float ui ----------------------------------------------------------------



settingUI <- tagList(
  h4(HTML("<strong>Update User Info</strong>"), style="text-align:center;"),
  br(), 
  fluidRow(
    loginForm("update_username", "Username", 5, 4, "Enter your username"), 
    loginForm("update_old", "Old Password", 5, 4, "Enter old password", password = TRUE), 
    loginForm("update_new", "New Password", 5, 4, "Enter new password", password = TRUE)
  )
) 

settingFooterUI <- tagList(actionButton("change", "Change"),
                           modalButton("Cancel"))

floatUI <- shinymanager::fab_button(
  position = "bottom-right", 
  actionButton("setting", "Setting", icon("gear")),
  actionButton("showImport", "Import data", icon("upload")),
  actionButton("logout", "Logout", icon("sign-out"))
)

# main ui -----------------------------------------------------------------

mainUI <- tagList(
    br(), # TODO:remove comment when tabsetPanel title is hidden
    floatUI, sidebarLayout(sidebarPanel(sidebarUI), mainPanel(bodyUI))
)

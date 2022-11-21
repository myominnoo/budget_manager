# login ui ----------------------------------------------------------------

getUserInfo <- function(id, name, label, n, offset, password = FALSE) {
  ns <- NS(id)
  if (password) {
    column(n, passwordInput(ns(name), label, width = "100%"), offset = offset)
  } else {
    column(n, textInput(ns(name), label, width = "100%"), offset = offset)
  }
}




loginBodyUI <- tagList(
  h2(HTML("<strong>Budget Manager</strong>"), style="text-align:center;"),
  hr(),
  h3(HTML("<strong>Log in</strong>"), style="text-align:center;"),
  br(),
  fluidRow(
    getUserInfo("login", "username", "Username", 5, 4), 
    getUserInfo("login", "password", "Password", 5, 4, password = TRUE)
  )
)

actionBtn <- function(id, name, label) {
  ns <- NS(id)
  actionButton(ns(name), label)
}

loginFooterUI <- tagList(actionBtn("login", "login", "Login"),
                         actionBtn("signup", "showSignup", "Sign Up"), 
                         actionBtn("login", "exit", "Exit"))

showLogin <- function(id) {
  showModal(modalDialog(loginBodyUI, footer = loginFooterUI, size = "m"))
}
showSignup <- function() {
  showModal(modalDialog(signupUI, footer = signupFooterUI, size = "m"))
}  


# signup ui ---------------------------------------------------------------


# setUserInfoServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     observeEvent(input$showSignup, showSignup())
#     observeEvent(input$cancelSignup, showLogin())
#     
#     
#     observeEvent(input$signup, {
#       
#     })
#   })
# }
# 
# 
# signupUI <- tagList(
#   h2(HTML("<strong>Budget Manager</strong>"), style="text-align:center;"),
#   hr(),
#   h3(HTML("<strong>JOIN ME !</strong>"), style="text-align:center;"),
#   br(),
#   fluidRow(
#     getUserInfo("signup", "signup_un", "Username", 5, 4), 
#     getUserInfo("signup", "signup_pw", "Password", 5, 4, password = TRUE)
#   )
# )
# signupFooterUI <- tagList(actionBtn("signup", "signup", "SignUp"), 
#                           actionBtn("signup", "cancelSignup", "Cancel"))




# body ui - mainPanel ui --------------------------------------------------

bodyUI <- tagList(
  
)

# sidebar ui --------------------------------------------------------------

sidebarUI <- tagList(
  h2(HTML("<b>Budget Manager</b>")), 
  p(HTML("version 1.2.0 <br>"), style = "font-size:10px;"), hr(), 
  numericInput("year", "Year", lubridate::year(Sys.Date()), width = 100), 
  sliderTextInput("month", "Month", month.abb, 
                  lubridate::month(Sys.Date(), label = TRUE), 
                  grid = TRUE, hide_min_max = TRUE), hr(),
  p(textOutput("display_username"), style = "font-size:14px;"), 
  p(textOutput("display_currentTime"), style = "font-size:14px;"), br(), 
  p(HTML("Developed by Myo Minn Oo <br>", "Please contact at dr.myominnoo$gmail.com"),
    style = "font-size:10px;") 
)

# float ui ----------------------------------------------------------------

floatUI <- shinymanager::fab_button(
  position = "bottom-right", 
  actionButton("setting", "Setting", icon("gear")),
  actionButton("sync", "Sync", icon("rotate")),
  actionButton("logout", "Logout", icon("sign-out")),
  actionButton("showImport", "Import data", icon("upload"))
)

# main ui -----------------------------------------------------------------

mainUI <- tagList(
    br(), # TODO:remove comment when tabsetPanel title is hidden
    floatUI, sidebarLayout(sidebarPanel(sidebarUI), mainPanel(bodyUI))
)
# 
# mainUI <- shinycssloaders::withSpinner(
#   mainUI
# )

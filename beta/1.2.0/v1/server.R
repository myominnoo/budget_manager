
getUserInfoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$exit, stopApp())
    observeEvent(input$dismiss_modal, {
      showLogin()
    })
    eventReactive(input$login, {

      user <- getUsersDB(usersDF)
      validateUsers(id, input$username, input$password, user)
      # cbind(user, e = e)
    })
  })
}


# main server -------------------------------------------------------------

server <- function(input, output, session) {
  
  ## display time 
  output$display_currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Current time : ", Sys.time())
  })
  ## display user name 
  output$display_username <- renderText(paste0("Username : ", env$auth))
  ## show login page for the first time
  showLogin()
  
  env <- reactiveValues(auth = NULL)
  ## render main UI
  e <- getUserInfoServer("login")
  
  observeEvent(e(), {
    ## update authenticated user
    env$auth <- e()$user
    if (!e()$e) {
      output$mainUI <- renderUI(mainUI)
      updateTabsetPanel(session, "tab", selected = "mainTab")
    }
  })
  
  # setUserInfoServer("signup")
  # observe(str(e()))
}





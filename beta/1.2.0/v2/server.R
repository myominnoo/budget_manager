

# main server -------------------------------------------------------------

server <- function(input, output, session) {
  
  ## display time 
  output$display_currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Current time : ", Sys.time())
  })
  ## display user name 
  output$display_username <- renderText(paste0("Username : ", env$user$user))
  
  ## show login page for the first time
  showLogin()
  observeEvent(input$showSignup, showSignup())
  observeEvent(input$cancelSignup, showLogin())
  observeEvent(input$exit, stopApp())
  
  ## log user in 
  observeEvent(input$login, {
    env$user <- getUsersDB(usersDF) %>% filter(user == input$username)
    e <- validateUsers(input$username, input$password, env$user)
    if (e) {
      showLogin()
    } else {
      cat(sprintf("login success [user:%s]\n", env$user$user))
      output$mainUI <- renderUI(mainUI)
      env$data <- getTransDB(transDF, env$user$user) 
      updateTabsetPanel(session, "tab", selected = "mainTab")
      removeModal()
    }
  })
  
  ## signup user
  observeEvent(input$signup, {
    err <- checkUserInput(input$signup_un, input$signup_pw, usersDF)
    if (err) {
      showSignup()
    } else {
      showErrorModal("Your name is in there! Try to log in!", 
                     title = "Success", footer = NULL)
      setSignupInfo(input$signup_un, input$signup_pw, usersDF)
      showLogin()
    }
  })
  
  # floating widget servers -------------------------------------------------
  observeEvent(input$logout, session$reload())
  observeEvent(input$showImport, importUI())
  observeEvent(input$import, {
    if (is.null(input$upload)) {
      showErrorModal("Try to select a file!", title = "Oups !", 
                     footer = modalButton("Close"))
      importUI()
    }
    
    req(input$upload)
    df <- importTrans(input$upload$datapath, env$user$user)
    showErrorModal("You just added ", nrow(df), " records", title = "Whoop-de-doo !", 
                   footer = modalButton("Close"))
    env$data <- env$data %>%
      bind_rows(df)
  })
  ## sync data
  observeEvent(input$sync, syncTrans(transDF, env$data, env$user$user))
  ## update setting user info 
  observeEvent(input$setting, {
    # browser()
    showSetting()
    updateTextInput(session, "update_username", value = env$user$user)
  })
  
  observeEvent(input$change, {
    err <- validateUsers(env$user$user, input$update_old, env$user)
    if (err) {
      showSetting()
      updateTextInput(session, "update_username", value = env$user$user)
    } else {
      transDF$remove(sprintf('{"user":"%s"}', env$user$user))
      usersDF$remove(sprintf('{"user":"%s"}', env$user$user))
      env$user$user <- env$user$name <- env$data$user <- input$update_username
      env$user$password <- scrypt::hashPassword(input$update_new)
      env$user %>% usersDF$insert()
      env$data %>% transDF$insert()
      showErrorModal("Your username and password are successfully updated!", 
                      title = "Good luck remembering them !", 
                     footer = modalButton("Close"))
    }
  })
  
  
  # download ----------------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {"budget.xlsx"},
    content = function(file) rio::export(env$data %>% select(Date:Note), file)
  )
  output$download_template <- downloadHandler(
    filename = function() {"budget_template.xlsx"},
    content = function(file) rio::export(createDummy(env$user$user) %>% 
                                           select(Date:Note), file)
  )
  
  
  # Transaction -------------------------------------------------------------
  output$data <- renderDataTable({
    req(env$data)
    env$data <- env$data %>% arrange(desc(Date))
    showTransDF(env$data)
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
      showErrorModal(title = "Error", "Oups ! Select some rows !", 
                     footer = modalButton("Close"))
    } else {
      env$undo <- env$data
      env$data <- env$data[-input$data_rows_selected, ] 
      showErrorModal(title = "Be careful!", "You just deleted ", 
                      length(input$data_rows_selected), " records !", 
                     footer = modalButton("Close"))
    }
  })
  
  ## undo event 
  observeEvent(input$undo, {env$data <- env$undo})
  
  ## add new event 
  observeEvent(input$add, {
    req(env$data)
    if (input$amount == 0) {
      showErrorModal(title = "Invalid", "Hmm, Amount is zero. Try to increase it!", 
                     footer = modalButton("Close"))
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
            user = env$user$user
          ) %>%
            mutate(id = paste(created_date, user))
        ) %>%
        setVars()
      showErrorModal(title = "You rock!", "Whoop-de-doo ! You just added an entry !", 
                     footer = modalButton("Close"))
    }
    
    updateDateInput(session = session, "date", value = Sys.Date())
    updateSelectInput(session = session, "category", selected = "Housing")
    updateTextAreaInput(session = session, "description", value = "")
    updateRadioGroupButtons(session = session, "account", selected = "Cheque")
    updateRadioGroupButtons(session = session, "type", selected = "Debit")
    updateNumericInputIcon(session = session, "amount", value = 0)
    updateTextAreaInput(session = session, "note", value = "")
  })
  

  # summary -----------------------------------------------------------------
  observe({
    req(env$data)
    dummy <- generateDummy(
      as.Date(paste0("1", input$month, input$year), "%d%B%Y"), env$user$user
    )
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
    
    env$balance <- silent(getTotalBalance(env$data, dummy))
    env$fltr <- silent(filterTrans(env$data, dummy, input$month, input$year))
    env$this_month <- silent(getThisMonth(env$fltr))
    env$expense <- silent(getExpense(env$fltr))
    env$income <- silent(getIncome(env$fltr))
  })  
  
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
  
  env <- reactiveValues(user = NULL, data = NULL)
  
}





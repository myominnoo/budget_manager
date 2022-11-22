
## setting page 
showSetting <- function() {
  showErrorModal(settingUI, footer = settingFooterUI, 
                 title = NULL, size = "m",
                 easyClose = FALSE, fade = FALSE)
}



# transaction -------------------------------------------------------------

silent <- function(x) {
  suppressMessages(suppressWarnings(try(x, silent = TRUE)))
}

filterTrans <- function(df, dummy, month, year) {
  df %>%
    bind_rows(dummy) %>%
    filter(mnth %in% month, yr %in% year)
} 


getTotalBalance <- function(df, dummy) {
  df %>%
    bind_rows(dummy) %>%
    group_by(Account, Type) %>%
    summarize(amount = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
    drop_na(Account, Type) %>%
    pivot_wider(names_from = Type, values_from = amount) %>%
    mutate(across(c(Credit, Debit), ~ ifelse(is.na(.x), 0, .x)),
           diff = Credit - Debit)
}
show_this_month <- function(df) {
  if (is.null(df)) {
    e <- data.frame(
      Type = c("Credit", "Debit", "Surplus"), 
      Cheque = rep(0, 3),
      Savings = rep(0, 3),
      Visa = rep(0, 3)
    ) 
    
  } else {
    e <- df
  }
  formattable(e, align = c("l", "r", "r", "r"), list(
    area(col = Cheque:Visa, row = 1) ~ sign_formatter_red, 
    area(col = Cheque:Visa, row = 2) ~ sign_formatter_green, 
    area(col = Cheque:Visa, row = 3) ~ sign_formatter
  ))
} 

savings_this_month <- function(df) {
  if (is.null(df)) {
    val <- 0
  } else {
    val <- df %>% 
      mutate(this_month = Cheque + Savings) %>%  
      filter(Type == "Surplus") %>% 
      pull(this_month)
  }
  valueBox(set_color(val), "Saved this month", icon = icon("money-check-dollar"), 
           width = 12, color = "yellow")
}

show_expense <- function(df) {
  if (is.null(df)) {
    e <- data.frame(Category = expense_fct, Amount = 0) %>% 
      mutate(Category = factor(Category, expense_fct)) %>% 
      group_by(Category) %>% 
      summarise(Amount = sum(Amount))   
  } else {
    e <- df
  }
  e <- e %>% 
    add_row(Category = "Total", Amount = sum(.$Amount)) 
  formattable(e, align = c("l", "r"), list(
    area(col = Amount) ~ sign_formatter_red
  ))
}

show_income <- function(df) {
  if (is.null(df)) {
    e <- data.frame(Category = account_fct, Amount = 0) %>% 
      mutate(Category = factor(Category, account_fct)) %>% 
      group_by(Category) %>% 
      summarise(Amount = sum(Amount))   
  } else {
    e <- df
  }
  e <- e %>% 
    add_row(Category = "Total", Amount = sum(.$Amount)) 
  formattable(e, align = c("l", "r"), list(
    area(col = Amount) ~ sign_formatter_green
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

getExpense <- function(df) {
  df %>% 
    filter(Type == "Debit") %>% 
    filter(Category %in% expense_fct) %>% 
    group_by(Category) %>% 
    summarize(Amount = sum(Amount, na.rm = TRUE)) 
}

getIncome <- function(df) {
  df %>% 
    filter(Category %in% income_fct) %>% 
    group_by(Type, Category) %>% 
    summarize(Amount = sum(Amount, na.rm = TRUE)) %>% 
    drop_na(Type, Category) %>% 
    pivot_wider(names_from = Type, values_from = Amount) %>% 
    mutate(across(-Category, ~ ifelse(is.na(.x), 0, .x))) %>% 
    mutate(Credit = Credit - Debit) %>% 
    select(Category, Credit) %>% 
    rename(Amount = Credit)
}


getThisMonth <- function(df) {
  df %>% 
    group_by(Account, Type) %>%
    summarize(amount = sum(Amount, na.rm = TRUE)) %>%
    drop_na(Account, Type) %>% 
    pivot_wider(names_from = Type, values_from = amount) %>%
    mutate(across(c(Credit, Debit), ~ ifelse(is.na(.x), 0, .x)),
           Surplus = Credit - Debit) %>%
    pivot_longer(-1, names_to = "Type") %>%
    pivot_wider(names_from = Account, values_from = value)
}

## sync data
syncTrans <- function(con, df, auth) {
  con$remove(sprintf('{"user":"%s"}', auth))
  df %>%
    dplyr::select(Date:id) %>%
    con$insert()
  showErrorModal("You just synced your data !", title = "Yike !", 
                 footer = modalButton("Close"))
}

showTransDF <- function(df) {
  df %>% 
    select(Date:Note) %>%
    datatable(
      rownames = FALSE, 
      editable = TRUE, 
      selection = "multiple", 
      filter = list(position = "top", clear = FALSE), 
      options = list(scrollX = TRUE)
    ) 
}



# dbconnection ------------------------------------------------------------

generateDummy <- function(x, auth) {
  tibble(
    Date = x, 
    Category = categories_fct, 
    Account = c(account_fct, rep(NA, 12)),
    Type = c(type_fct, rep(NA, 13))
  ) %>% 
    expand(Date, Category, Account, Type) %>% 
    mutate(
      created_date = Sys.time(), 
      user = auth, 
      id = paste(created_date, user), 
      Description = NA, 
      Amount = 0, 
      Note = NA
    ) %>% 
    setVars()
}

createDummy <- function(auth) {
  data.frame(
    Date = as.Date(character()),
    Category = factor(levels = categories_fct),
    Description = character(),
    Account = factor(levels = account_fct),
    Type = factor(levels = type_fct),
    Amount = numeric(),
    Note = character(),
    stringsAsFactors = FALSE
  ) %>% 
    mutate(
      created_date = Sys.time(), 
      user = auth, 
      id = paste(created_date, user), 
      Note = ifelse(is.na(NA), "", Note)
    ) %>%  
    select(Date:Note, everything())
}

getUsersDB <- function(con) {
  con$find('{}') %>% tibble()
}

getTransDB <- function(con, auth) {
  df <- con$find(sprintf('{"user":"%s"}', auth)) 
  if (nrow(df) > 0) {
    df %>%
      tibble() %>%
      setVars()
  } else {
    createDummy(auth) %>%
      setVars()
  }
}


# html --------------------------------------------------------------------

set_text_tags <- function(x) {
  tags$style(paste0(
    "#", x, " {border: 0px solid #dd4b39; color: #FFFFFF; font-size: 20px;", 
    " text-align:center; font-weight: bold; background-color: #18191A}"
  ))
}

# data process ------------------------------------------------------------

setVars <- function(df) {
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

# import ------------------------------------------------------------------

importTrans <- function(filepath, auth) {
  rio::import(filepath) %>% 
    mutate(created_date = Sys.time(), 
           user = auth, 
           id = paste(created_date, user), 
           Note = ifelse(is.na(NA), "", Note)) %>%  
    select(Date:Note, everything()) %>% 
    setVars()
}


# signup ------------------------------------------------------------------

setSignupInfo <- function(username, password, con) {
  data.frame(
    created_date = Sys.time(), 
    user = username,
    password = scrypt::hashPassword(password),
    is_hashed_password = TRUE, 
    permissions = "standard",
    name = username
  ) %>%  
    con$insert()
  cat(sprintf("user signup success [user:%s]", username))
}

checkUserInput <- function(username, password, con) {
  user <- getUsersDB(con)  
  if (username == "") {
    e <- TRUE
    showErrorModal("Hmm, Username is empty. Try your name!", title = "Empty Username !", 
                   footer = modalButton("Close"))
  } else if (password == "") {
    showErrorModal("Hmm, Password is empty. Try something else!", title = "Empty Password !", 
                   footer = modalButton("Close"))
    e <- TRUE
  } else if (any(username %in% user$user)) {
    showErrorModal("Hmm, Username is not available. Try something else!", 
                   title = "Invalid", footer = modalButton("Close"))
    e <- TRUE
  } else {
    e <- FALSE
  }
  return(e)
}



# login -------------------------------------------------------------------

showErrorModal <- function( ..., title, size = "s", easyClose = TRUE, 
                            footer = actionButton("dismiss_modal", label = "Dismiss")) {
  showModal(modalDialog(..., title = title, size = size, easyClose = easyClose, footer = footer))
}

validateUsers <- function(username, password, df) {
  testPassword <- df %>% pull(password)
  if (username == "") {
    e <- TRUE
    showErrorModal("Hmm, Username is empty. Try your name!", title = "Empty Username !", 
                   footer = modalButton("Close"), easyClose = FALSE)
  } else if (password == "") {
    e <- TRUE
    showErrorModal("Hmm, Password is empty. Try something else!", title = "Empty Password !", 
                   footer = modalButton("Close"), easyClose = FALSE)
  } else if (length(testPassword) == 0) {
    e <- TRUE 
    showErrorModal("Hmm, Username is wrong. Try again!", title = "Invalid", 
                   footer = modalButton("Close"), easyClose = FALSE)
  } else if (!scrypt::verifyPassword(testPassword, password)) {
    e <- TRUE
    showErrorModal("Hmm, Password is wrong. Try again!", title = "Invalid", 
                   footer = modalButton("Close"), easyClose = FALSE)
  } else {
    e <- FALSE
  }
  return(e)
}



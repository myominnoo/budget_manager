
# functions ---------------------------------------------------------------

showErrorModal <- function(..., title, size = "s", easyClose = TRUE) {
  showModal(modalDialog(..., title = title, size = size, easyClose = easyClose))
}



check_empty <- function(username, password) {
  if (username == "") {
    showErrorModal("Hmm, Username is empty. Try your name!", title = "Empty Username !")
    e <- TRUE
  } else if (password == "") {
    showErrorModal("Hmm, Password is empty. Try something else!", title = "Empty Password !")
    e <- TRUE
  }
}

validateUsers <- function(username, password, df) {
  e <- TRUE
  if (username == "") {
    showErrorModal("Hmm, Username is empty. Try your name!", title = "Empty Username !")
    e <- TRUE
  } else if (password == "") {
    showErrorModal("Hmm, Password is empty. Try something else!", title = "Empty Password !")
    e <- TRUE
  } else if (nrow(df) == 0) {
    showErrorModal("Hmm, Username is wrong. Try again!", title = "Invalid")
    e <- TRUE
  } else if (!scrypt::verifyPassword(df$password, password)) {
    showErrorModal("Hmm, Password is wrong. Try again!", title = "Invalid")
    e <- TRUE
  } else {
    removeModal()
    e <- FALSE
  }
}

write_signup <- function(username, password) {
  data.frame(
    created_date = Sys.time(), 
    user = username,
    password = scrypt::hashPassword(password),
    is_hashed_password = TRUE, 
    permissions = "standard",
    name = username
  ) %>%  
    users$insert()
}








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

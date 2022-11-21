
# dbconnection ------------------------------------------------------------


getUsersDB <- function(con) {
  con$find('{}') %>% tibble()
}


# login -------------------------------------------------------------------

showErrorModal <- function(id, ..., title, size = "s", easyClose = TRUE) {
  showModal(modalDialog(..., title = title, size = size, easyClose = easyClose, 
                        footer = actionButton(NS(id, "dismiss_modal"), label = "Dismiss")))
}

validateUsers <- function(id, username, password, df) {
  # browser()
  testPassword <- df %>% filter(user == username) %>% pull(password)
  if (username == "") {
    e <- TRUE
    showErrorModal(id, "Hmm, Username is empty. Try your name!", title = "Empty Username !")
  } else if (password == "") {
    e <- TRUE
    showErrorModal(id, "Hmm, Password is empty. Try something else!", title = "Empty Password !")
  } else if (length(testPassword) == 0) {
    e <- TRUE 
    showErrorModal(id, "Hmm, Username is wrong. Try again!", title = "Invalid")
  } else if (!scrypt::verifyPassword(testPassword, password)) {
    e <- TRUE
    showErrorModal(id, "Hmm, Password is wrong. Try again!", title = "Invalid")
  } else {
    e <- TRUE
    removeModal()
  }
  return(e)
}

# 
# set_vars <- function(df) {
#   df %>%
#     mutate(
#       Date = as.Date(Date),
#       Category = factor(Category, categories_fct),
#       Description = as.character(Description),
#       Account = factor(Account, account_fct),
#       Type = factor(Type, type_fct),
#       Amount = as.numeric(Amount),
#       Note = as.character(Note),
#       mnth = month(Date, label = TRUE),
#       mnth = as.character(mnth),
#       yr = year(Date)
#     )
# }
# 
# 
# 
# 
# 
# create_dummy <- data.frame(Date = as.Date(character()),
#                            Category = factor(levels = categories_fct),
#                            Description = character(),
#                            Account = factor(levels = account_fct),
#                            Type = factor(levels = type_fct),
#                            Amount = numeric(),
#                            Note = character(),
#                            stringsAsFactors = FALSE)


library(mongolite)
library(tidyverse)

# sample logins dataframe with passwords hashed by sodium package
users_base <- data.frame(
  created_date = Sys.time(),
  user = c("admin", "myo", "phyo"),
  password = sapply(c("a", "t", "t"), scrypt::hashPassword) %>% unname(),
  is_hashed_password = TRUE,
  permissions = c("admin", "standard", "standard"),
  name = c("Admin", "Myo", "Phyo")
)


# dbconnection ------------------------------------------------------------

con <- "mongodb+srv://myself:letmein2022@budgetapp.whtgpcl.mongodb.net/?retryWrites=true&w=majority"
users <- mongolite::mongo(collection = "users", db = "budgetdb", url = con, verbose = TRUE)
trans <- mongolite::mongo(collection = "transaction", db = "budgetdb", url = con, verbose = TRUE)


# CRUD --------------------------------------------------------------------

users$find('{}')
users$remove('{}')
users$insert(users_base)
trans$remove('{}')
users$insert(users_base)


users$find('{"user": "admin"}') %>%
  tibble()

users$find('{"user": "admin"}') %>%
  tibble() %>%
  pull(password) %>%
  sodium::password_verify("admin123")







generate_dummy <- function(x, auth) {
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
    set_df()
}

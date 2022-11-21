
# mongodb -----------------------------------------------------------------

con <- "mongodb+srv://myself:letmein2022@budgetapp.whtgpcl.mongodb.net/?retryWrites=true&w=majority"
users <- mongolite::mongo(collection = "users", db = "budgetdb", url = con, verbose = TRUE)
users_df <- function() users$find() %>% tibble()

trans <- mongolite::mongo(collection = "transaction", db = "budgetdb", url = con, verbose = TRUE)
trans_df <- function() trans$find() %>% tibble()


# house keeping -----------------------------------------------------------

# users_df()
# b <- rio::import("")
# b %>%
# mutate(created_date = c(Sys.time() - 1:nrow(.)), user = "myo") %>%
#   mutate(id = paste(created_date, user), 
#          Note = ifelse(is.na(Note), "", Note)) %>% 
#   select(Date:Note, everything()) %>% 
#   trans$insert()
# 
# 
# trans$drop()
# 
# trans$remove(sprintf('{"user":"%s"}', "myo"))

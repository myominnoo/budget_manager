

# https://www.quicken.com/blog/budget-categories/
categories_list <- 
  list(`Essentials` = list(
    # Essentials
    "Housing", 
    "Utilities", 
    "Insurance", 
    "Food", 
    "Healthcare", 
    "Transportation", 
    "Personal"
  ), 
  `Non-essentials` = list(
    # Non-essentials
    "Entertainment", 
    "Travel",
    "Debt",
    "Other"
  ), 
  `Income` = list(
    # Others
    "Savings", 
    "Paycheck", 
    "Interest",
    "Bonus"
  ))

expense_fct <- c(
  "Housing", 
  "Utilities", 
  "Insurance", 
  "Food", 
  "Healthcare", 
  "Transportation", 
  "Personal", 
  "Entertainment", 
  "Travel", 
  "Debt",
  "Other"
)

income_fct <- c(
  "Savings", 
  "Paycheck", 
  "Interest",
  "Bonus"
)

categories_fct <- c(expense_fct, income_fct)

account_fct <- c("Cheque", "Savings", "Visa")

type_fct <- c("Debit", "Credit")


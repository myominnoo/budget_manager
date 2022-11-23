
# info --------------------------------------------------------------------

## Info UI 
infoUI <- tabPanel(
  "Info", 
  icon = icon("circle-info"), 
  
  h2("Categorizing Personal Budget"), 
  hr(), 
  p("This App is developed and maintained by Myo Minn Oo."), 
  div("Souce code is available here: ",
      a("Budget Manager App", href="https://github.com/myominnoo/budget_manager")), 
  br(), 
  p("A well-thought-out budget can help you take control of your finances and use your money with real purpose, so you have enough to pay your bills, grow your savings, and still enjoy life today."), 
  h5("Essentials"), 
  tags$li("Housing : anything you pay toward keeping a roof over your head is considered a housing expense. That includes rent or mortgage payments, property taxes, HOA dues, and home maintenance costs. For most budgeters, this category is by far the biggest."), 
  br(), 
  tags$li("Utilities : Water, electricity, and HVAC (heating, ventilation, and air conditioning) are vital to practically every well-functioning household. Your utilities category should cover all the expenses that keep these services up and running. That includes your gas, electricity, water, and sewage bills. For most families, it will also include your cell phone, cable, and internet expenses."), 
  br(), 
  tags$li("Insurance : to simply put, it should include every single insurance payment, such as health insurance (only what’s not deducted pre-tax by your employer), homeowner’s or renter’s insurance, home warranties or protection plans, auto insurance, life insurance, disability insurance, and so on."), 
  br(), 
  tags$li("Food : Groceries, of course, are an essential expense for every family. Many budgeters include dining out in this category (e.g., restaurant meals, work lunches, food delivery, etc.) However, if you’re someone who tends to spend a significant amount of money on things like gourmet food and wine, you might want to put your non-grocery food expenses into one of the non-essential categories. The basic idea behind your budget categories is to split them up so you can see what you really need to spend separately from what you’re choosing to spend."), 
  br(), 
  tags$li("Healthcare : Maintaining your health and well-being is essential, so it’s critical to include enough in your budget to cover these costs. If you plan for routine medical care, you’ll live a much healthier life over the long run. This budget category includes anything you might spend on healthcare, such as: out-of-pocket costs for primary care, specialty care (dermatologists, psychologists, etc.), dental care, urgent care, prescriptions, medical devices and supplies."), 
  br(), 
  tags$li("Transportation : Regardless of your location or lifestyle, everyone needs to get from point A to point B. Typically, this budget category includes car payments, registration and DMV fees, gas, maintenance, parking, tolls, and public transit."), 
  br(), 
  tags$li("Personal : This category is a catchall for anything that could be considered a personal care or “lifestyle” expense. Personal spending includes things like: gym memberships, clothes and shoes, home decor and furnishings, gifts. Because some personal care products are essential, such as soap and laundry detergent, you might want to include those in your food budget category. After all, you probably buy those with your other groceries."), 
  br(), 
  h5("Non-Essentials"), 
  tags$li("Entertainment : This home budget category consists of your “fun money.” That’s important! For most of us, carving out leisure time (and money) is essential to maintaining a healthy work-life balance. This budget category can include things like: concert tickets, sporting events, family activities & vacations, streaming services and other subscriptions (e.g., Hulu and Netflix), restaurants (if you didn’t include this under “Food”), video games, hobbies."), 
  br(), 
  tags$li("Travel : This category is different from transportation which you have to commute to work or somewhere else. This includes vacation trip, trip to relatives, etc."), 
  br(), 
  tags$li("Debt : Sometimes you have to borrow money."), 
  br(), 
  tags$li("Other : Any categories that do not fit above"), 
  br(),   
  h5("Income"), 
  tags$li("Savings : Money that goes directly to your savings account."), 
  br(), 
  tags$li("Paycheck : Salaries, etc."), 
  br(), 
  tags$li("Interest : Bank interests, etc."), 
  br(), 
  tags$li("Bonus : tip, annual bonus, etc."), 
  br(), 
  hr(), 
  br(), 
  p("Credit: https://www.quicken.com/blog/budget-categories/")
)



helpUI <- tagList(
  br(), 
  column(12, align = "right", 
         actionButton("returnLogin", "Login", icon = icon("right-to-bracket"))), 
  div(
    align = "center", 
    h2(HTML("<strong>How to use Budget Manager App</strong>")), 
    hr(), 
    p(HTML("More to be added later .. "))
  )
)

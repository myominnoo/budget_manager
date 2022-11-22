

ui <- fluidPage(
  theme = bslib::bs_theme(5, "sketchy"),
  title = "Budget Manager App",
  tags$head(tags$link(
    rel = "shortcut icon", 
    href = "https://github.com/myominnoo/budget_manager/raw/main/R/v1.2.0/www/favicon.ico"
  )),
  tabsetPanel(
    id = "tab",
    type = "hidden",
    tabPanel("loginTab"), 
    tabPanel("mainTab", uiOutput("mainUI"))
  )
)


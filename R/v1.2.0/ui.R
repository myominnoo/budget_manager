

ui <- fluidPage(
  theme = bslib::bs_theme(5, "sketchy"),
  tabsetPanel(
    id = "tab",
    type = "hidden",
    tabPanel("loginTab"), 
    tabPanel("mainTab", uiOutput("mainUI"))
  )
)



ui <- fluidPage(
  theme = bslib::bs_theme(5, "sketchy"),
  useShinyFeedback(), # include shinyFeedback
  tabsetPanel(
    id = "tab",
    type = "hidden", 
    selected = "loginTab", 
    tabPanel("loginTab"), 
    tabPanel("mainTab", uiOutput("mainUI"))
  )
)

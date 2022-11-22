

ui <- fluidPage(
  theme = bslib::bs_theme(5, "sketchy"),
  # title = "Budget Manager", 
  titlePanel(
    windowTitle = "Title that appears in the browser bar",
    title = tags$head(tags$link(rel="icon", 
                                href="data:www;base64/favicon.ico", 
                                type="image/x-icon")
    )),
  tabsetPanel(
    id = "tab",
    type = "hidden",
    tabPanel("loginTab"), 
    tabPanel("mainTab", uiOutput("mainUI"))
  )
)

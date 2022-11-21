#Dynamically change theme of shinymaterial

library(shiny)
library(shinymaterial)

ui <- fluidPage(
  uiOutput('material_ui')
)

server <- function(input, output, session) {
  
  
  clrs <- reactiveValues()
  clrs$primary <- 'blue'
  clrs$secondary <- 'red'
  clrs$switch_status <- FALSE
  
  
  
  output$material_ui <- renderUI({
    
    print(clrs$switch_status)
    
    material_page(
      title = "Basic Page", 
      primary_theme_color = clrs$primary, 
      secondary_theme_color = clrs$secondary,
      material_row(
        material_column(
          width = 12,
          material_button("button", "Button"),
          checkboxInput("switch", "Dark mode:",value = clrs$switch_status),
          material_radio_button("radio", "", c("A", "B", "C"))
        )
      )
    )
    
  })
  
  
  
  observeEvent(input$switch, {
    
    if (input$switch) {
      
      clrs$primary <- 'black'
      clrs$secondary <- 'grey'
      clrs$switch_status <- TRUE
      
      
    } else {
      
      clrs$primary <- 'blue'
      clrs$secondary <- 'red'
      clrs$switch_status <- FALSE
      
      
    }
  })
}

shinyApp(ui, server)
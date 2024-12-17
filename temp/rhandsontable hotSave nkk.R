library(shiny)
library(rhandsontable)




ui <- fluidPage(
  rHandsontableOutput("hottable")  
)

server <- function(input, output, session) {
  observe({
    print(hot_to_r(input$hottable))
  })
  
  output$hottable <- renderRHandsontable({
    rhandsontable(readRDS("C:/2024ICBT/temp/hotDataTest.RDS") )
  })
}

shinyApp(ui, server)
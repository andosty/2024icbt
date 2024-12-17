library(shiny)
library(rhandsontable)

DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
                small = letters[1:10],
                dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = F)



ui <- fluidPage(
  rHandsontableOutput('table')
)

server <- function(input, output) {
  
  X = reactiveValues(data = DF)
  
  output$table <- rhandsontable::renderRHandsontable({
    rhandsontable(X$data, rowHeaders = NULL)
  })
  
  observeEvent(input$table$changes$changes,{
    row = input$table$changes$changes[[1]][[1]]
    col = input$table$changes$changes[[1]][[2]]
    value = input$table$changes$changes[[1]][[4]]
    
    X$data[row,col] = value
  })
}

shinyApp(ui, server)
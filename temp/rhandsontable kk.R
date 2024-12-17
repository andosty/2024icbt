library(shiny)
library(rhandsontable)

myDF <- data.frame(x = c(1, 2, 3))

ui <- fluidPage(rHandsontableOutput('hottable'),
                br(),
                actionButton('addCol', 'Add'))

server <- function(input, output, session) {
  EmptyTbl <- reactiveVal(myDF)
  
  observeEvent(input$hottable, {
    EmptyTbl(hot_to_r(input$hottable))
  })
  
  output$hottable <- renderRHandsontable({
    rhandsontable(EmptyTbl())
  })
  
  observeEvent(input$addCol, {
    newCol <- data.frame(c(1, 2, 3))
    names(newCol) <- paste("Col", ncol(hot_to_r(input$hottable)) + 1)
    EmptyTbl(cbind(EmptyTbl(), newCol))
    
  })
  
}

shinyApp(ui, server)
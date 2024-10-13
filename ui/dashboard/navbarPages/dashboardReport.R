tabItem(
  tabName = "dashboard",
  
  fluidRow(
    column(
      width = 12,
      align = 'left',
      #table1st
      # tableOutput(outputId = 'testData'),
      # DT::dataTableOutput('icbtFinalDaset'),
      # DT::dataTableOutput('icbtFinalErrors'),
    )
  ),
  
  fluidRow(
    column(
      width = 3,
      infoBox(
        width = 12,
        title = 'Total Trade',
        value= 500, #compute them in server function
        icon = icon("list"),
        color = "primary"
      )
    ),
    
    column(
      width = 3,
      infoBox(
        width = 12,
        title = 'Total Going Out',
        value= 60, #compute them in server function,
        icon = icon("dove"),
        color = "primary"
      )
    ),
    
    column(
      width = 3,
      infoBox(
        width = 12,
        title = 'Total Coming In',
        # value= unique_locations,
        value= 80, #compute them in server function,
        icon = icon("location-dot"),
        color = "primary"
      )
    ),
    
    column(
      width = 3,
      infoBox(
        width = 12,
        title = 'Net Total',
        value= 84, #compute them in server function,
        # value= unique_locations,
        icon = icon("location-dot"),
        color = "primary"
      )
    ),
  ),
  
  fluidRow(
    sortable(
      width = 6,
      
      # box(
      #   title = "Unique Birds Found at each Location", 
      #   width = 12, 
      #   status = "olive",
      #   collapsible = FALSE, 
      #   ribbon(
      #     text = "NEW",
      #     color = "olive"
      #   ),
      #   
      #   plotlyOutput("plot_unique_birds_site")
      # ),
      
      # box(
      #   width = 12,
      #   title = "Box 2",
      #   closable = TRUE,
      #   collapsible = FALSE,
      # )
    )
    
    # sortable(
    #   width = 6,
    #   box(
    #     width = 12,
    #     title = "Box 3",
    #     collapsible = FALSE,
    #     maximizable = TRUE,
    #   ),
    #   box(
    #     width = 12,
    #     title = "Box 4",
    #     collapsible = TRUE,
    #     label = boxLabel(
    #       text="Lable",
    #       status = "primary",
    #       tooltip = "i am a label"
    #     )
    #     
    #   )
    # ),
    
  )
)
tabItem(
  tabName = "dashboard",
  
  
  
  
  # fluidRow(
  # column(
  #   12,
  #   div(
  #     style = "padding: 50px",
  #     # h1("You have now signed in and your email address has been verified!"),
  #     # br(),
  #     # # h3("This is all the information that comes with the signed in user with 'Email/Password' authentication on Firebase"),
  #     # h3("total obs"),
  #     # textOutput(outputId = "totalData_Obs"),
  #     # h3("total error obs"),
  #     # textOutput(outputId = "totalError_Obs"),
  #     # 
  #     # br(),
  #     # DT::DTOutput("user_access"),
  #     # 
  #     # DT::DTOutput("user_out")
  #   )
  # ),
  
  
  
  #   column(
  #     width = 12,
  #     align = 'left',
  #     #table1st
  #     # tableOutput(outputId = 'testData'),
  #     # DT::dataTableOutput('icbtFinalDaset'),
  #     # DT::dataTableOutput('icbtFinalErrors'),
  #   )
  # ),
  
  
  ## cases and error summaries

   h3("CASES in data and ERROR Summary"),
    div(
  fluidRow(
    column(
      width = 4,
      infoBox(
        width = 12,
        title = 'Total Cases',
        value= textOutput(outputId = "totalData_Cases"), #compute them in server function  
        icon = icon("list"),
        color = "primary"
      )
    ),
    column(
      width = 4,
      infoBox(
        width = 12,
        title = 'Total Cases with Errors',
        value= textOutput(outputId = "totalError_Cases"), #compute them in server function  
        icon = icon("list"),
        color = "primary"
      )
    ),
    column(
      width = 4,
      infoBox(
        width = 12,
        title = 'Total No# of Errors',
        value= textOutput(outputId = "totalError_Obs"), #compute them in server function
        icon = icon("list"),
        color = "primary"
      )
    )
    ),
  ),
  br(),
  ## summary data stats
  # fluidRow(
  #   column(
  #     width = 3,
  #     infoBox(
  #       width = 12,
  #       title = 'Total Trade',
  #       value= 500, #compute them in server function
  #       icon = icon("list"),
  #       color = "primary"
  #     )
  #   ),
  #   
  #   column(
  #     width = 3,
  #     infoBox(
  #       width = 12,
  #       title = 'Total Going Out',
  #       value= 60, #compute them in server function,
  #       icon = icon("dove"),
  #       color = "primary"
  #     )
  #   ),
  #   
  #   column(
  #     width = 3,
  #     infoBox(
  #       width = 12,
  #       title = 'Total Coming In',
  #       # value= unique_locations,
  #       value= 80, #compute them in server function,
  #       icon = icon("location-dot"),
  #       color = "primary"
  #     )
  #   ),
  #   
  #   column(
  #     width = 3,
  #     infoBox(
  #       width = 12,
  #       title = 'Net Total',
  #       value= 84, #compute them in server function,
  #       # value= unique_locations,
  #       icon = icon("location-dot"),
  #       color = "primary"
  #     )
  #   ),
  # ),
  
  fluidRow(
          column(
                  width = 6,
                  box(
                    title = "Total Cases & ErrorCases by RegionName",
                    width = 12,
                    status = "olive",
                    collapsible = FALSE,
                    maximizable = TRUE,
                    # ribbon(
                    #   text = "NEW",
                    #   color = "olive"
                    # ),
                    plotlyOutput("regionalErrorTotal")
                  )
            ),
          column(
              width = 6,
                box(
                  title = "Total Error Frequency by RegionName",
                  width = 12,
                  status = "olive",
                  collapsible = FALSE,
                  maximizable = TRUE,
                  plotlyOutput("regionalErrorCount")
                )
              
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
    
  ),
  br(),
  # h3("TEAM ERROR RANKING"),
     # div(
     #   box(
     #     selectizeInput('foo', choices = NULL, label = "Region Filter")
     #     # varSelectizeInput('v_region_error',label='ok',regionNames)
     #     # varSelectInput(v_region_error, )
     #     # selectInput("v_region_error", label = "Region Filter" , choices = unique("regionNames") , width = 6),
     #     # selectizeInput(
     #     #   inputId = "in_selectize",
     #     #   label = "select variable",
     #     #   choices = NULL,
     #     #   multiple = TRUE
     #     # ),
     #     )
     # ),
#    fluidRow(
#           column(
#                   width = 6,
#                   box(
#                     title = "Total Cases & ErrorCases by RegionName",
#                     width = 12,
#                     status = "olive",
#                     collapsible = FALSE,
#                     maximizable = TRUE
#                     # plotlyOutput("regionalErrorTotal")
#                   )
#             ),
#           column(
#               width = 6,
#                 box(
#                   title = "Total Error Frequency by RegionName",
#                   width = 12,
#                   status = "olive",
#                   collapsible = FALSE,
#                   maximizable = TRUE
#                   # plotlyOutput("regionalErrorCount")
#                 )
#               
#    
#     
#   )
# )
)

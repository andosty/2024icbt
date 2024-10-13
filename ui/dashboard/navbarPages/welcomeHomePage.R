tabItem(
  tabName = "home",
  jumbotron(
    title = "Welcome",  #heading
    status = "info",
    lead = "Informal Cross Border Trade Survey",  #sub heading
    # btnName = "download",
    # href = #
    "Ghana Statistical Services"
  ),
  # Add user box and the box
  fluidRow(
    # Add a quote to the user bio
    box(
      collapsible = FALSE,
      title =       "Data Quality Monitoring Process for ICBT :",
      width = 12,
      blockQuote(
        div(
          htmltools::tags$b('DQM processes are to:'),
          htmltools::tags$li('Validate work of enumerators and provide feedback on data quality'),
          htmltools::tags$li('Ensure that inconsistencies and errors identified are rectified immediately by enumerators on the field'),
          htmltools::tags$li('Facilitate the editing and cleaning of the data collected during field work in real time'),
          htmltools::tags$li('Ensure that all Cross Border zones and cases have been assigned and reflected in database to achieve complete coverage'),
        ),
        color = "info"
      )
    )
  )
)

 dashboardPage(
        dark = NULL, #disable the dark mode switch
        help = NULL, #disable the help tooltip switch
        fullscreen = TRUE , #turn on the fullscreen button
        
        title = "ICBT Dashboard",
        header = dashboardHeader(
          title = dashboardBrand(
            title = "ICBT",
            image = "data:image/png;base64,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"
            
            # img(src = 'gss.png',  height = "40px"),
            # style = "padding-top:5px; padding-bottom:5px;")
            
            # img(src = "gss.png")
          ),
          tags$button(
            id = "submit_sign_out",
            type = "button",
            "Sign Out",
            class = "btn-danger pull-right",
            style = "color: white;"
          )
          # ,
          # 
          # rightUi = 
          #   dropdownMenu(
          #   badgeStatus = "info",
          #   type = "notifications",
          #   
          # ,
          #   
          #   # actionButton("titleBtId", "", icon = icon("refresh"),
          #   #              class = "btn-primary btn-s", title = "Sign Out" , id = "submit_sign_out"),
          #   
          #   # box(
          #   #   title = p("Title 1", 
          #   #             actionButton("titleBtId", "", icon = icon("refresh"),
          #   #                          class = "btn-xs", title = "Update")
          #   #   ), 
          #   #   width = 4, solidHeader = FALSE, status = "warning",
          #   #   # uiOutput("boxContentUI2")
          #   # ),
          #   # 
          #   
          #   notificationItem(
          #     text = "Success",
          #     status = "success",
          #     icon = icon("circle-check")
          #   )
          #   # notificationItem(
          #   #   text = "Warning",
          #   #   status = "warning",
          #   #   icon = icon("circle-exclamation")
          #   # ),
          #   # notificationItem(
          #   #   text = "Error",
          #   #   status = "danger",
          #   #   icon = icon("circle-xmark")
          #   # )
          # )
          # end of right ui
        ),
        
        sidebar = dashboardSidebar(
          sidebarMenu(
            id = "sidebarMenuid",
            menuItem(
              "Home",
              tabName = "home",
              icon = icon("home")
            ),
            menuItem(
              "Dashboard Errors",
              tabName = "dashboardError",
              icon = icon("bar-chart")
            ),
            menuItem(
              "Dashboard Statistics",
              tabName = "dashboardSummary",
              icon = icon("bar-chart")
            ),
            menuItem(
              "Monitor Report",
              tabName = "monitorReport",
              icon = icon("file")
            ),
            menuItem(
              "View Dataset",
              tabName = "icbtData",
              icon = icon("eye")
            ),
            menuItem(
              "View Errors",
              tabName = "errorData",
              icon = icon("stop-circle")
            ),
            menuItem(
              "Action Menu",
              tabName = "actionMenu",
              icon = icon("cloud-download")
            ),
            menuItem(
              "Manual Rejection",
              tabName = "manualRejectionMenu",
              icon = icon(name="filter-circle-xmark", class = NULL, lib = "font-awesome")
              # icon = icon("fa-filter-circle-xmark")
              # icon = icon("table")
            )
          ) 
        ),
        # https://www.rdocumentation.org/packages/shiny/versions/1.9.1/topics/icon
        controlbar = dashboardControlbar(),
        
        #footer
        footer = dashboardFooter(
          left = "Data Quality Monitor - GSS",
          right = "copyright, 2024"
        ),
        
        #body
        body = dashboardBody(
          tabItems(
            
          source("ui/dashboard/navbarPages/welcomeHomePage.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/dashboardErrors.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/dashboardStats.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/monitor_report.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/view_icbtDataSet.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/view_icbtErrors.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/icbt_actionMenus.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/manualRejectionMenu.R", local = TRUE)$value
            
            
          )
        ),
      )
 
 
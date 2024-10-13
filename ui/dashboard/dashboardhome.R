 dashboardPage(
        dark = NULL, #disable the dark mode switch
        help = NULL, #disable the help tooltip switch
        fullscreen = TRUE , #turn on the fullscreen button
        
        title = "Andrew's Dashboard",
        header = dashboardHeader(
          title = dashboardBrand(
            title = "ICBT",
            # image = "https://marketresearch.com.gh/wp-content/uploads/2022/01/msr-new-logo-for-web.png"  #"D:/Developer Room/R-dashboard1/media/logo/logo1.png"
            # image = "D:/Developer Room/R-dashboard1/media/logo/logo1.png"  #"D:/Developer Room/R-dashboard1/media/logo/logo1.png"
            image = "https://images.unsplash.com/photo-1539664030485-a936c7d29c6e?q=80&w=1160&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"
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
              "Dashboard",
              tabName = "dashboard",
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
            )
          ) 
        ),
        
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
          source("ui/dashboard/navbarPages/dashboardReport.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/monitor_report.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/view_icbtDataSet.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/view_icbtErrors.R", local = TRUE)$value,
          source("ui/dashboard/navbarPages/icbt_actionMenus.R", local = TRUE)$value
            
            
          )
        ),
      )
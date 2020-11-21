library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  hr(),
  
  sidebarMenu(
    id="tabs",
    menuItem("READ ME FIRST", tabName = "readme", icon = icon("readme")),
    menuItem("Simulation Tool", tabName = "tool", icon = icon("file-prescription"), selected = TRUE),
    menuItem("About Me & LYF Camp", tabName = "about", icon = icon("address-card")),
    menuItem("Code",  icon = icon("file-text-o"),
             menuSubItem("Functions.R", tabName = "Functions", icon = icon("angle-right")),
             menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
             menuSubItem("server.R", tabName = "server", icon = icon("angle-right")))
  ),
  
  hr()
)


body <- dashboardBody(
  tabItems(
    ###########################
    #### README #### 
    ###########################
    tabItem(tabName = "readme",
            box( width = 12,collapsible = FALSE, solidHeader = TRUE, title="ReadMe", 
                 includeHTML("readme.html")
                 
            )
    ),
    ###########################
    ####  Simulation Tool  #### 
    ###########################
    
    tabItem(tabName = "tool",fluidRow(
      column(width=3,
             box(title="About", width=NULL, solidHeader = T, status="primary", collapsible = T, collapsed=T,
                 "This Shiny App was developed as a simulation tool to understand the COVID risks of hosting summer camp."), 
             box(
               title = "Camp Assumptions", width = NULL, solidHeader = TRUE, collapsible=T, collapsed=T,
               sliderInput(inputId = 'Duration',
                           label = 'How many nights at camp?',
                           min = 1, max = 10,
                           value = 4),
               sliderInput(inputId = 'camp_size',
                           label = 'Camp size? (campers + staff)',
                           min = 20, max = 500,
                           value = 60),
               sliderInput(inputId = 'cabin_size',
                           label = 'How many campers per cabin? (Warning: Must be multiple of camp size!!)',
                           min = 2, max = 50,
                           value = 5),
               sliderInput(inputId = 'day_R0',
                           label = 'Infection rate per active infection during day time activities?',
                           min = 0, max = 1,
                           value = 0.05, step = 0.025),
               sliderInput(inputId = 'night_R0',
                           label = 'Infection rate per active infection in sleeping quarters?',
                           min = 0, max = 1,
                           value = 0.5, step = 0.025)
             ),
             box(
               title = "COVID Assumptions", width = NULL, solidHeader = TRUE, collapsible=T, collapsed=T,
               sliderInput(inputId = 'initial_infected',
                           label = 'Range of initial infections to simulate (Choosing a large range will increase simulation time)',
                           min = 0, max = 100,
                           value = c(1,9)),
               sliderInput(inputId = 'initial_step',
                           label = 'Initial Infection Range Step, (Default values will simulate from 1-9 infections with a step of 2: 1,3,5,7,9)',
                           min = 0, max = 10,
                           value = 2),
               sliderInput(inputId = 'test_sensitivity',
                           label = 'Test Sensitivity (1-false positive rate)',
                           min = 0, max = 1,
                           value = 0.94, step = 0.01),
               sliderInput(inputId = 'test_specificity',
                           label = 'Test Specificity (1-false negative rate)',
                           min = 0, max = 1,
                           value = 0.98, step = 0.01),
               sliderInput(inputId = 'vaccine_rate',
                           label = '% of Camp Vaccinated',
                           min = 0, max = 100,
                           value = 0, step = 1)
             )),
      
      column(width=9,
             box(title = "Simulation Plots", status = "danger",solidHeader = TRUE, width = 12, height = "100%", 
                 fluidRow(column(width = 2, actionButton("Simulate", label = "Simulate")),
                          (column(width = 10, htmlOutput("sim_status")))),
                 plotOutput("sim_plot"),
                 strong("(Left Plot)"), "In blue is the Perfect_Rate, this is the percentage chance in which you will catch all infections at check-in.
          In green is the Import_Rate, which is the percentage chance in which you will import one or more undetected infections to camp.
          In red is the Half_Rate, which is the percentage chance of more than half of camp becoming COVID infected. These three rates are
          plotted versus number of initial infected persons on the bottom or it's equivalent community positivity rate on the top. You can use
          this graph to determine the maximum community positivity rate you would like to see in your local area to minimize risk of importing an infection to camp.",
                 strong("\n(Right Plot)"), "If you import an infection to camp, what is the likely outcome? These are looking only at
          simulations that have imported a case to camp, and plotting a boxplot of the number of additional infections. The
          main conclusion here is that regardless of the community positivity rate, if you import an undetected infection to camp the result is largely the same (the median across
          scenarios is similar). In other words, community positivity rate primarily affects your risk of importing a case, but if
          you do import a case you will likely see similar numbers of infections.")
      ))),
    ###########################
    #### About #### 
    ###########################
    tabItem(tabName = "about",
            box( width = 12,collapsible = FALSE, solidHeader = TRUE, title="About", #"hello"
                 includeHTML("about.html")
                 
            )
    ),
    
    ###########################
    #### Code #### 
    ###########################
    tabItem(tabName = "Functions",
            box( width = NULL, solidHeader = TRUE, title="Functions.R",
                 pre(includeText("Functions.R"))
            )
    ), 
    
    tabItem(tabName = "ui",
            box( width = NULL, solidHeader = TRUE, title="ui.R",
                 pre(includeText("ui.R"))
            )
    ),
    
    tabItem(tabName = "server",
            box( width = NULL, solidHeader = TRUE, title="server.R",
                 pre(includeText("server.R"))
            )
    )
  ))


ui <- dashboardPage(skin="black",
                    dashboardHeader(title = "Summer Camp COVID Simulator"),
                    sidebar,
                    body)

library(shiny)
library(shinythemes)
library(shinydashboard)

sidebar <- dashboardSidebar(
  hr(),
  
  sidebarMenu(
    id="tabs",
    menuItem("ReadMe: Inputs & Assumptions", tabName = "readme", icon = icon("readme")),
    menuItem("Simulation Tool", tabName = "tool", icon = icon("file-prescription"), selected = TRUE),
    menuItem("Outputs & Interpretations", tabName = "readme2", icon = icon("readme")),
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
            box( width = 12,collapsible = FALSE, solidHeader = TRUE, title="Simulation Inputs and Assumptions", 
                 includeHTML("readme.html")
                 
            )
    ),
    ###########################
    ####  Simulation Tool  #### 
    ###########################
    
    tabItem(tabName = "tool",fluidRow(
      column(width=3,
             box(title="Analysis Settings", width=NULL, solidHeader = T, status="primary", collapsible = T, collapsed=T,
                 "Read about each of these in ReadMe before continuing.",
                 radioButtons(inputId = "Analysis",
                              label = "What type of analysis would you like to perform?",
                              choiceNames = list("Single Scenario", "County Positive Test Rate", "Vaccination Rate", 
                                               "Camp Size", "Cabin/Group Size", "Imported Infections"),
                              choiceValues = list(0,1,3,4,5,2)),
                 conditionalPanel(
                   condition = "input.Analysis == '1'",
                   sliderInput(inputId = 'positive_rate',
                               label = 'Range of county positive test rate % to simulate (Choosing a large range will increase simulation time)',
                               min = 0, max = 100,
                               value = c(1,9), step = 0.5),
                   sliderInput(inputId = 's1_step',
                               label = 'County positive test rate Range Step, (Default values will simulate from 1-9% with a step of 2: 1,3,5,7,9)',
                               min = 0, max = 10,
                               value = 2, step = 0.25)),
                 conditionalPanel(
                   condition = "input.Analysis == '2'",
                   sliderInput(inputId = 'initial_infected',
                               label = 'Range of imported infections to simulate (Choosing a large range will increase simulation time)',
                               min = 0, max = 100,
                               value = c(1,9)),
                   sliderInput(inputId = 's2_step',
                               label = 'Range Step, (Default values will simulate from 1-9 infections with a step of 2: 1,3,5,7,9)',
                               min = 0, max = 10,
                               value = 2)),
                 conditionalPanel(
                   condition = "input.Analysis == '3'",
                   sliderInput(inputId = 'vaccine_range',
                               label = 'Range of vaccination rates to simulate (Choosing a large range will increase simulation time)',
                               min = 0, max = 100,
                               value = c(0,50)),
                   sliderInput(inputId = 's3_step',
                               label = 'Vaccination Rate Range Step, (Default values will simulate from 0-50% vaccination rate with step of 10%: 0,10,20,30,40,50%)',
                               min = 0, max = 50,
                               value = 10)),
                 conditionalPanel(
                   condition = "input.Analysis == '4'",
                   sliderInput(inputId = 'camp_size_range',
                               label = 'Range of camp sizes to simulate (Choosing a large range will increase simulation time)',
                               min = 20, max = 500,
                               value = c(50,100)),
                   sliderInput(inputId = 's4_step',
                               label = 'Camp Size Range Step, (Default values will simulate from 50-100 people with step of 25: 50,75,100 people)',
                               min = 0, max = 50,
                               value = 25)),
                 conditionalPanel(
                   condition = "input.Analysis == '5'",
                   sliderInput(inputId = 'cabin_size_range',
                               label = 'Range of cabin/group size to simulate (Choosing a large range will increase simulation time)',
                               min = 2, max = 50,
                               value = c(4,10)),
                   sliderInput(inputId = 's5_step',
                               label = 'Cabin Size Range Step, (Default values will simulate from 4-10 people per cabin with step of 2: 4,6,8,10)',
                               min = 1, max = 10,
                               value = 2))
                 ), 
             box(
               title = "Camp Assumptions", width = NULL, solidHeader = TRUE, collapsible=T, collapsed=T,
               sliderInput(inputId = 'Duration',
                           label = 'How many nights at camp?',
                           min = 1, max = 10,
                           value = 4),
               conditionalPanel(condition = "input.Analysis != '4'",
               sliderInput(inputId = 'camp_size',
                           label = 'Camp size? (campers + staff)',
                           min = 20, max = 500,
                           value = 60)),
               conditionalPanel(condition = "input.Analysis != '5'",
                 sliderInput(inputId = 'cabin_size',
                           label = 'How many campers per cabin?',
                           min = 2, max = 50,
                           value = 5)),
               sliderInput(inputId = 'day_R0',
                           label = 'Infection rate per active infection for all of camp?',
                           min = 0, max = 1,
                           value = 0.05, step = 0.025),
               sliderInput(inputId = 'night_R0',
                           label = 'Infection rate per active infection in cabin/small group?',
                           min = 0, max = 1,
                           value = 0.5, step = 0.025)
             ),
             box(
               title = "COVID Assumptions", width = NULL, solidHeader = TRUE, collapsible=T, collapsed=T,
               conditionalPanel(condition = "input.Analysis != '1'",
                 sliderInput(inputId = 'positive_rate',
                           label = 'County positive test rate %',
                           min = 0, max = 25,
                           value = 2, step = 0.25)),
               sliderInput(inputId = 'test_sensitivity',
                           label = 'Test Sensitivity (1-false positive rate)',
                           min = 0, max = 1,
                           value = 0.94, step = 0.01),
               sliderInput(inputId = 'test_specificity',
                           label = 'Test Specificity (1-false negative rate)',
                           min = 0, max = 1,
                           value = 0.98, step = 0.01),
               conditionalPanel(condition = "input.Analysis != '3'",
               sliderInput(inputId = 'vaccine_rate',
                           label = '% of Camp Vaccinated',
                           min = 0, max = 100,
                           value = 0, step = 1))
             )),
      
      column(width=9,
             box(title = "Simulation Plots", status = "danger",solidHeader = TRUE, width = 12, height = "100%", 
                 fluidRow(column(width = 3, actionButton("Simulate", label = "Simulate")),
                          column(width = 4,
                          radioButtons(inputId = "Plot_settings",
                                       inline = T,
                                       label = NULL,
                                       choiceNames = list("Plots", "Tables"),
                                       choiceValues = list(0,1))),
                          column(width = 3,downloadButton("downloadData", "Download Raw Sim Data"))),
                 fluidRow(column(width = 12, htmlOutput("sim_status"))),
                 fluidRow(column(width = 7,
                        conditionalPanel(condition = "input.Plot_settings == '0'",
                                  plotOutput("sim_plot", height = "800px")),
                        conditionalPanel(condition = "input.Plot_settings == '1'",
                                  strong("Simulation Summary Table:"),
                                  tableOutput("sim_data"),
                                  strong("Assuming 1+ Imported Cases:"),
                                  tableOutput("extra_data"))),
                 column(width = 5, htmlOutput("warning"), conditionalPanel(condition = "input.Plot_settings == '0'",
                        HTML("<br/>"),strong("Simulation Summary Plot:"), "In green is the Perfect_Rate, this is the percentage chance in which all infections will be caught at check-in.
                        In orange is the Import_Rate, which is the percentage chance in which you will import one or more undetected infections to camp.
                        In grey is the Quarter_Rate, which is the percentage chance of 1/4 or more of camp becoming COVID infected.",HTML("<br/><br/>"),
                        conditionalPanel(condition = "input.Analysis != '2'",strong("Imported Infections Plot:"), "This bar graph visualizes the probability of importing X number of undetected cases to camp, and how that
                        changes with your scenarios. The green bar with 0 undetected infections imported is equivalent to the perfect rate from the summary plot."),
                        HTML("<br/>"),strong("Additional Infections Plot:"),"This plot is looking only at simulations that have imported an undetected case, excluding all green scenarios.
                        Boxplots represent the number of additional infections you can expect at camp if you import an undetected case."),
                        conditionalPanel(condition = "input.Plot_settings == '1'", HTML("<br/>"),strong("Simulation Summary Table:"), "This is the table form of the Simulation Summary Plot. 
                        The Perfect_Rate is the percentage chance in which all infections will be caught at check-in.
                        The Import_Rate is the percentage chance in which you will import one or more undetected infections to camp.
                        The Quarter_Rate is the percentage chance of 1/4 or more of camp becoming COVID infected.",
                        HTML("<br/><br/>"),strong("Assuming 1+ Imported Cases Table:"), "This table provides a few more statistics about the simulations that have imported an undetected case, 
                        excluding all perfect rate scenarios.", HTML("<br/><i>Additional Infections</i>"), "is what is plotted as boxplots on the plots page, representing the number of additional
                        infections you can expect at camp.", HTML("<br/><i>Infections Sent Home</i>"), " is the number of undetected infected campers sent home to their families at the end of camp.
                        ", HTML("<br/><i>Initial Infections</i>"), " is the number of infections on Day 0 prior to any testing at check in.", HTML("<br/><i>Mistaken Quarantine</i>"), " is the number of individuals
                         who tested as a false positive and are mistakenly quarantined. ", HTML("<br/><i>Quarantined</i>"), " is the number of individuals in quarantine, to give a sense of the space and resources to plan for. 
                        ", HTML("<br/><i>Total Infections</i>"), " is the total number of infections at camp, initial + additional infections.") )))
      ))),
    ###########################
    #### Outputs and Interpretation #### 
    ###########################
    tabItem(tabName = "readme2",
            box( width = 12,collapsible = FALSE, solidHeader = TRUE, title="Simulation Outputs and Interpretation", #"hello"
                 includeHTML("readme2.html")
                 
            )
    ),
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

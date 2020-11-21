library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(future.apply)

source("Functions.R")


server <- function(input, output) {
  
  STATUS_TRT_DUR_PLOT <- reactiveVal()
  
  observeEvent({list(
    input$Duration,
    input$camp_size,
    input$cabin_size,
    input$day_R0,
    input$night_R0,
    input$initial_infected[1],
    input$initial_infected[2],
    input$test_specificity,
    input$test_sensitivity,
    input$vaccine_rate
  )},
  {STATUS_TRT_DUR_PLOT("<B><font color='red'>*Inputs changed, recalculation required</font></B>")})
  
  SIM_DF = reactive({
    active_data = NULL
    
    for (initial in seq(input$initial_infected[1],input$initial_infected[2],input$initial_step)) {
      
      initials = c(rep(1, times = initial), rep(0,times = (input$camp_size - initial)))
      
      sims <- future_lapply(1:400, future.seed = T, function(sim_n){
        camp_data_holder = data.frame(sim_n = rep(sim_n, each = input$camp_size),
                                      initial_infected = initial,
                                      id =  1:input$camp_size,
                                      status = sample(initials, size = input$camp_size),
                                      vaccine = rbinom(input$camp_size, 1, prob = (input$vaccine_rate/100)),
                                      Quarantine = rep(0,each = input$camp_size))
        active_data_holder = NULL
        
        
        for (day_n in 0:input$Duration){
          test <- testing(camp_data_holder, input, active_data_holder, day_n, initial, sim_n)
          camp_data_holder = test$camp_data_holder
          active_data_holder = test$active_data_holder
          active_infections = test$active_infections
          
          Activities <- Infections(camp_data_holder, input, active_infections)
          camp_data_holder = Activities$camp_data_holder
        }
        return(active_data_holder)
      })
      active_data <- active_data %>%
        bind_rows(do.call(rbind.data.frame, sims))
      incProgress(amount = 1/length(seq(input$initial_infected[1],input$initial_infected[2],input$initial_step)))
    }
    return(active_data)
  })
  
  
  
  PLOT_PLOT = eventReactive(input$Simulate,{
    STATUS_TRT_DUR_PLOT("<i>Up to date</i><B><font color='red'> *Warning: These simulations do not take into account the possibility that individuals could have a low viral burden and thus be difficult to detect but still be infectious. This is a major limitation of this analysis and undetectable transmission should not be taken lightly.</font></B>")
    
    active_data = SIM_DF()
    holder <- summary_stats(active_data, input)
    Rates = holder$Rates
    RATE_PLOT <- ggplot(Rates) +
      geom_line(aes(x=initial_infected, y=100*rate, color=Key)) +
      xlab("Initial Infected (Persons)") + ylab("Percent") + 
      scale_x_continuous(breaks = pretty_breaks(),
                         sec.axis = sec_axis(~100*./input$camp_size, name="Community Positivity Rate (%)", breaks = seq(0,40,2))) + 
      scale_y_continuous(breaks = seq(0,100,10)) +
      theme(legend.position = "bottom")
    
    BOX_PLOT <- ggplot(filter(active_data,day==input$Duration & Infections > initial_infected)) +
      geom_boxplot(aes(x=as.factor(initial_infected), y=Infections-initial_infected)) + 
      xlab("Initial Infections") + ylab("Additional infections")
    
    PLOTS <- grid.arrange(RATE_PLOT,BOX_PLOT, nrow=1)
    
    return(list(PLOTS=PLOTS))
    
    
  })
  
  
  output$sim_plot<-renderPlot({
    withProgress(message = 'Simulating, will take a few minutes.', value = 0, {
      PLOT_PLOT()
      
    })
  })
  
  output$sim_status = renderText({
    STATUS_TRT_DUR_PLOT()
  })
  
}

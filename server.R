library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(future.apply)
library(RColorBrewer)

source("Functions.R")


server <- function(input, output) {
  
  STATUS_TRT_DUR_PLOT <- reactiveVal()
  active_data1 <- NULL
  download_data <- NULL
  
  observeEvent({list(
    input$Duration,
    input$camp_size,
    input$cabin_size,
    input$day_R0,
    input$night_R0,
    input$initial_infected,
    input$test_specificity,
    input$test_sensitivity,
    input$vaccine_rate,
    input$initial_step,
    input$positive_rate,
    input$vaccine_range,
    input$cabin_size_range,
    input$camp_size_range,
    input$Analysis
    
  )},
  {STATUS_TRT_DUR_PLOT("<B><font color='red'>*Inputs changed, recalculation required</font></B>")})
  
  seq_loop <- NULL
  SIM_DF = eventReactive(input$Simulate,{
    STATUS_TRT_DUR_PLOT("<i>Up to date</i>")
    
    active_data = NULL

    
    if (input$Analysis == 0){
      seq_loop <<- seq(1,1,1)
    } else if (input$Analysis == 1){
      seq_loop <<- seq(input$positive_rate[1],input$positive_rate[2],input$s1_step)
    } else if (input$Analysis == 2){
      seq_loop <<- seq(input$initial_infected[1],input$initial_infected[2],input$s2_step)
    } else if (input$Analysis == 3){
      seq_loop <<- seq(input$vaccine_range[1],input$vaccine_range[2],input$s3_step)
    } else if (input$Analysis == 4){
      seq_loop <<- seq(input$camp_size_range[1],input$camp_size_range[2],input$s4_step)
    } else if (input$Analysis == 5){
      seq_loop <<- seq(input$cabin_size_range[1],input$cabin_size_range[2],input$s5_step)
    }
    
    for (initial in seq_loop) {
      
      if (input$Analysis == 1){
        initials = rbinom(input$camp_size, 1, prob = (initial/100))
      } else if (input$Analysis == 2){
        initials = c(rep(1, times = initial), rep(0,times = (input$camp_size - initial)))
      } else if (input$Analysis == 4){
        initials = rbinom(initial, 1, prob = (input$positive_rate/100))
      } else {
        initials = rbinom(input$camp_size, 1, prob = (input$positive_rate/100))
      } 
      sims <- future_lapply(1:50, future.seed = T, function(sim_n){
        if (input$Analysis == 0){
          camp_data_holder = data.frame(sim_n = rep(sim_n, each = input$camp_size),
                                        initial = initial,
                                        id =  1:input$camp_size,
                                        status = rbinom(input$camp_size, 1, prob = (input$positive_rate/100)),
                                        vaccine = rbinom(input$camp_size, 1, prob = (input$vaccine_rate/100)),
                                        Quarantine = rep(0,each = input$camp_size))
        } else if (input$Analysis == 1){
          camp_data_holder = data.frame(sim_n = rep(sim_n, each = input$camp_size),
                                        initial = initial,
                                        id =  1:input$camp_size,
                                        status = rbinom(input$camp_size, 1, prob = (initial/100)),
                                        vaccine = rbinom(input$camp_size, 1, prob = (input$vaccine_rate/100)),
                                        Quarantine = rep(0,each = input$camp_size))
        } else if (input$Analysis == 2){
          camp_data_holder = data.frame(sim_n = rep(sim_n, each = input$camp_size),
                                        initial = initial,
                                        id =  1:input$camp_size,
                                        status = sample(initials, size = input$camp_size),
                                        vaccine = rbinom(input$camp_size, 1, prob = (input$vaccine_rate/100)),
                                        Quarantine = rep(0,each = input$camp_size))
        } else if (input$Analysis == 3){
          camp_data_holder = data.frame(sim_n = rep(sim_n, each = input$camp_size),
                                        initial = initial,
                                        id =  1:input$camp_size,
                                        status = rbinom(input$camp_size, 1, prob = (input$positive_rate/100)),
                                        vaccine = rbinom(input$camp_size, 1, prob = (initial/100)),
                                        Quarantine = rep(0,each = input$camp_size))
        } else if (input$Analysis == 4){
          camp_data_holder = data.frame(sim_n = rep(sim_n, each = initial),
                                        initial = initial,
                                        id =  1:initial,
                                        status = rbinom(initial, 1, prob = (input$positive_rate/100)),
                                        vaccine = rbinom(initial, 1, prob = (input$vaccine_rate/100)),
                                        Quarantine = rep(0,each = initial))
        } else if (input$Analysis == 5){
          camp_data_holder = data.frame(sim_n = rep(sim_n, each = input$camp_size),
                                        initial = initial,
                                        id =  1:input$camp_size,
                                        status = rbinom(input$camp_size, 1, prob = (input$positive_rate/100)),
                                        vaccine = rbinom(input$camp_size, 1, prob = (input$vaccine_rate/100)),
                                        Quarantine = rep(0,each = input$camp_size))
        }
        active_data_holder = NULL
        
        if(input$Analysis != 2){
        for (day_n in 0:input$Duration){
          test <- testing(camp_data_holder, input, active_data_holder, day_n, initial, sim_n)
          camp_data_holder = test$camp_data_holder
          active_data_holder = test$active_data_holder
          active_infections = test$active_infections
          
          Activities <- Infections(camp_data_holder, input, active_infections, initial)
          camp_data_holder = Activities$camp_data_holder
        }
          } else if (input$Analysis == 2){
          active_infections = nrow(filter(camp_data_holder, status == 1 & Quarantine == 0))
          active_data_holder <- data.frame(sim_n = sim_n,
                                           initial = initial,
                                           day = 0,
                                           Infections = sum(camp_data_holder$status),
                                           Active_Infections = active_infections,
                                           Quarantine = sum(camp_data_holder$Quarantine),
                                           initial_infections = sum(camp_data_holder$status))
          Activities <- Infections(camp_data_holder, input, active_infections)
          camp_data_holder = Activities$camp_data_holder
          
          for (day_n in 1:input$Duration){
            test <- testing(camp_data_holder, input, active_data_holder, day_n, initial, sim_n)
            camp_data_holder = test$camp_data_holder
            active_data_holder = test$active_data_holder
            active_infections = test$active_infections
            
            Activities <- Infections(camp_data_holder, input, active_infections, initial)
            camp_data_holder = Activities$camp_data_holder
          }
          
          
          }
        incProgress(amount = 1/(400*length(seq_loop)))
        return(active_data_holder)
      })
      active_data <- active_data %>%
        bind_rows(do.call(rbind.data.frame, sims))
      
    }
    active_data1 <<- active_data
    return(active_data)
  })
  
  Rates_holder <- NULL
  extra_data <- NULL
  PLOT_PLOT = eventReactive(input$Simulate,{
    SIM_DF()
    holder <- summary_stats(active_data1, input)
    Rates = holder$Rates
    Rates_holder <<- spread(Rates, key = "Key", value = "rate") 
    download_data <<- active_data1
    if (input$Analysis == 1){
      label = "County Positive Test Rate (%)"
      colnames(Rates_holder)[1] <<- c("County Positive Test Rate %")
      colnames(download_data)[2] <<- c("County Positive Test Rate %")
    } else if (input$Analysis == 2){
      label = "Number of Imported Undetected Infections (Persons)"
      colnames(Rates_holder)[1] <<- c("# Imported Infections")
      colnames(download_data)[2] <<-c("# Imported Infections")
    } else if (input$Analysis == 3){
      label = "Vaccination Rate (%)"
      colnames(Rates_holder)[1] <<- c("Vaccination Rate %")
      colnames(download_data)[2] <<-c("Vaccination Rate %")
    } else if (input$Analysis == 4){
      label = "Camp Size (Persons)"
      colnames(Rates_holder)[1] <<- c("Camp Size")
      colnames(download_data)[2] <<-c("Camp Size")
    } else if (input$Analysis == 5){
      label = "Cabin Size (Persons)"
      colnames(Rates_holder)[1] <<- c("Cabiin Size")
      colnames(download_data)[2] <<-c("Cabiin Size")
    } else if (input$Analysis == 0){
      label = "Single Scenario"
      colnames(Rates_holder)[1] <<- c("Scenario")
      colnames(download_data)[2] <<-c("Scenario")
    } 
    
    if (input$Analysis == 0){
      RATE_PLOT <- ggplot(Rates) +
        geom_bar(aes(x=0, y=100*rate, fill=Key), stat = "identity") + ylab("Probability (%)") + 
        scale_x_continuous(breaks = seq_loop, limits = c(-1,1)) + 
        scale_y_continuous(breaks = seq(0,100,10)) + ggtitle("Simulation Summary Plot") +
        theme(legend.position = "bottom") + theme(axis.title.x = element_blank(),
                                                  axis.text.x = element_blank(),
                                                  axis.ticks = element_blank() ) +
        scale_fill_manual(values=c("#D95F02","#1B9E77","#666666"))
    } else if (input$Analysis != 0){
      RATE_PLOT <- ggplot(Rates) +
        geom_line(aes(x=initial, y=100*rate, color=Key)) +
        xlab(label) + ylab("Probability (%)") + ggtitle("Simulation Summary Plot") +
        scale_x_continuous(breaks = seq_loop) + 
        scale_y_continuous(breaks = seq(0,100,10)) +
        theme(legend.position = "bottom") +
        scale_color_manual(values=c("#D95F02","#1B9E77","#666666"))

    }
    
    if (input$Analysis != 2){
      import_data <<- active_data1 %>%
        group_by(initial) %>%
        filter(day == 0) %>%
        count(Active_Infections) %>%
        mutate(Imported_Infections = as.factor(Active_Infections))



      IMPORT_PLOT <- ggplot(import_data) +
        geom_bar(aes(x=initial, y=n/4, fill=Imported_Infections), position = position_dodge(preserve = "single"),stat = "identity") + 
        ylab("Probability (%)") + ggtitle("Imported Infections Plot") +
        scale_x_continuous(breaks = seq_loop) + xlab(label) +
        scale_y_continuous(breaks = seq(0,100,10)) +
        theme(legend.position = "bottom")+
        scale_fill_brewer(palette = "Dark2")
    } else { IMPORT_PLOT <- NULL}
    
    
    if (input$Analysis == 2){
    BOX_PLOT <- ggplot(filter(active_data1,day==input$Duration & Infections > initial_infections)) +
      geom_boxplot(aes(x=as.factor(initial_infections), y=Infections-initial_infections, color = as.factor(initial_infections))) + 
      ggtitle("Additional Infections Plot") + theme(legend.position = "none") +
      xlab("Initial Imported Infections") + ylab("Additional infections")+
      scale_fill_brewer(palette = "Dark2")
    } else {
      BOX_PLOT <- ggplot(filter(active_data1,day==input$Duration & Infections > initial_infections)) +
        ggtitle("Additional Infections Plot") +
        geom_boxplot(aes(x=as.factor(initial), y=Infections-initial_infections, color = as.factor(initial))) + 
        xlab(label) + ylab("Additional infections")+ theme(legend.position = "none") +
        scale_fill_brewer(palette = "Dark2")

    }
    if (input$Analysis == 2){
      PLOTS <- grid.arrange(RATE_PLOT,BOX_PLOT, ncol=1, heights = c(2,2))
    } else {    
      PLOTS <- grid.arrange(RATE_PLOT,IMPORT_PLOT,BOX_PLOT, ncol=1, heights = c(3,3,3))
    }
    
    extra_data <<- active_data1 %>%
      filter(day==input$Duration & Infections > initial_infections) %>%
      select(initial,Infections,Quarantine,initial_infections,Active_Infections) %>%
      mutate(Mistaken_Quarantine = Quarantine - Infections,
             Additional_Infections = Infections - initial_infections)
      colnames(extra_data) <<- c("initial","Total Infections","Quarantined","Initial Infections","Infections Sent Home","Mistaken Quarantine","Additional Infections")
    extra_data <<- extra_data %>%
      gather(key = "Parameter", value = "value",2:7) %>%
      group_by(initial, Parameter) %>%
      summarise(lower_quartile = round(quantile(value,prob=0.25), digits=0),
                median = round(median(value), digits=0),
                upper_quartile = round(quantile(value,probs = 0.75), digits=0))
    colnames(extra_data) <<- c(label,"Parameter","Lower Quartile","Median","Upper Quartile")
    return(list(PLOTS=PLOTS))
    
  })
  
  RATESTABLE <- eventReactive(input$Plot_settings == 1,{
    return(Rates_holder %>%
             mutate(Perfect_rate = Perfect_rate*100,
                    Import_rate = Import_rate*100,
                    Quarter_rate = Quarter_rate*100)
                    )
  })
  EXTRATABLE <- eventReactive(input$Plot_settings == 1,{
    return(extra_data)
  })
  
  
  output$warning <- renderText({
    "<B><font color='red'> *Warning: These simulations do not take into account the possibility that 
    individuals could have a low viral burden and thus be difficult to detect but still be infectious. 
    This is a major limitation of this analysis and undetectable transmission should not be taken lightly.</font></B>"
  })
  
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  output$sim_data <- renderTable({RATESTABLE()})
  output$extra_data <- renderTable({EXTRATABLE()})
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "Simulated_dataset.csv"
    },
    content = function(file) {
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  output$sim_plot<-renderPlot({
    withProgress(message = 'Simulating, will take a few minutes.', value = 0, {
      PLOT_PLOT()
    })
  })
  
  output$sim_status = renderText({
    STATUS_TRT_DUR_PLOT()
  })
  
}

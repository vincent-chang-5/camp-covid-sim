testing <- function(camp_data_holder, input, active_data_holder, day_n, initial, sim_n){
  #Testing
  Detected <- rep(0,times=input$camp_size)
  
  for (i in 1:input$camp_size){
    Detected[i] = ifelse(camp_data_holder$status[i] == 0, rbinom(1, 1, prob = (1-input$test_specificity)), rbinom(1, 1, prob = (input$test_sensitivity)))
  }
  
  #Quarantine detected people
  for (i in 1:input$camp_size){
    camp_data_holder$Quarantine[i] = ifelse(camp_data_holder$Quarantine[i] == 1, 1, Detected[i])
  }
  
  #Update counts in the morning after testing
  active_infections = nrow(filter(camp_data_holder, status == 1 & Quarantine == 0))
  
  active_data_holder <- active_data_holder %>%
    bind_rows(data.frame(sim_n = sim_n,
                         initial_infected = initial,
                         day = day_n,
                         Infections = sum(camp_data_holder$status),
                         Active_Infections = active_infections,
                         Quarantine = sum(camp_data_holder$Quarantine)))
  
  return(list(camp_data_holder = camp_data_holder, 
              active_infections = active_infections,
              active_data_holder = active_data_holder))
}

Infections <- function(camp_data_holder, input, active_infections){
  #Day activities
  
  for (i in 1:input$camp_size){
    if(camp_data_holder$Quarantine[i] == 0 & camp_data_holder$status[i] == 0){
      if(camp_data_holder$vaccine[i] == 0){
        camp_data_holder$status[i] = rbinom(1, 1, prob = input$day_R0*active_infections)
      } else {
        camp_data_holder$status[i] = rbinom(1, 1, prob = input$day_R0*active_infections*0.05)
      }
    } else {
      camp_data_holder$status[i] = camp_data_holder$status[i]
    }
    
  }
  
  #Night 
  active_infections = nrow(filter(camp_data_holder, status == 1 & Quarantine == 0))
  for (i in 0:(input$camp_size/input$cabin_size - 1)){
    cabin_infections = sum(camp_data_holder$status[(i*input$cabin_size+1):((i+1)*input$cabin_size)])
    cabin_quarantines = sum(camp_data_holder$Quarantine[(i*input$cabin_size+1):((i+1)*input$cabin_size)])
    active_cabin_infections = nrow(camp_data_holder %>% slice((i*input$cabin_size+1):((i+1)*input$cabin_size)) %>% filter(status == 1 & Quarantine == 0))
    
    for (j in 1:input$cabin_size){
      if(camp_data_holder$Quarantine[i*input$cabin_size+j] == 0 & camp_data_holder$status[i*input$cabin_size+j] == 0){
        if(camp_data_holder$vaccine[i*input$cabin_size+j] == 0){
          camp_data_holder$status[i*input$cabin_size+j] = rbinom(1,1, prob = ifelse(input$night_R0*active_cabin_infections > 1, 1, input$night_R0*active_cabin_infections))
        } else{
          camp_data_holder$status[i*input$cabin_size+j] = rbinom(1,1, prob = ifelse(input$night_R0*active_cabin_infections > 1, 1, input$night_R0*active_cabin_infections*0.05))
        }
      } else {
        camp_data_holder$status[i*input$cabin_size+j] = camp_data_holder$status[i*input$cabin_size+j]
      }
    }
  }
  return(list(camp_data_holder = camp_data_holder))
}

summary_stats <- function(active_data, input){
  Rates <- active_data %>%
    group_by(initial_infected) %>%
    filter(day == 0 & Active_Infections == 0) %>%
    summarise(Perfect_rate = n()/400)%>%
    left_join(active_data %>%
                group_by(initial_infected) %>%
                filter(day == input$Duration & Infections >= input$camp_size/2) %>%
                summarise(Half_rate = n()/400) ) %>%
    left_join(active_data %>%
                group_by(initial_infected) %>%
                filter(day == 0 & Active_Infections >= 1) %>%
                summarise(Import_rate = n()/400)) %>%
    gather(key = "Key", value="rate",2:4)
  
  return(list(Rates=Rates))
}
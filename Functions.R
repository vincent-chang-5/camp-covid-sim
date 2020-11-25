testing <- function(camp_data_holder, input, active_data_holder, day_n, initial, sim_n){
  #Testing

  if(input$Analysis == 4){
    camp_size = initial

  } else {
    camp_size = input$camp_size
  }
  Detected <- rep(0,times=camp_size)
  
  for (i in 1:camp_size){

    Detected[i] = ifelse(camp_data_holder$status[i] == 0, rbinom(1, 1, prob = (1-input$test_specificity)), rbinom(1, 1, prob = (input$test_sensitivity)))
  }
  
  #Quarantine detected people
  for (i in 1:camp_size){
    camp_data_holder$Quarantine[i] = ifelse(camp_data_holder$Quarantine[i] == 1, 1, Detected[i])

  }
  
  #Update counts in the morning after testing
  active_infections = nrow(filter(camp_data_holder, status == 1 & Quarantine == 0))
  if(day_n == 0){
    initial_infections = sum(camp_data_holder$status)
  } else {
    initial_infections = as.numeric(active_data_holder %>%
      filter(day == 0) %>% select(Infections))
  }
  
  active_data_holder <- active_data_holder %>%
    bind_rows(data.frame(sim_n = sim_n,
                         initial = initial,
                         day = day_n,
                         Infections = sum(camp_data_holder$status),
                         Active_Infections = active_infections,
                         Quarantine = sum(camp_data_holder$Quarantine),
                         initial_infections = initial_infections))
  
  return(list(camp_data_holder = camp_data_holder, 
              active_infections = active_infections,
              active_data_holder = active_data_holder))
}

Infections <- function(camp_data_holder, input, active_infections, initial){
  #Day activities

  for (i in 1:nrow(camp_data_holder)){
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
  if (input$Analysis == 5){
    cabin_loop_max = ceiling(nrow(camp_data_holder)/initial)
    c <- split(camp_data_holder, rep(1:ceiling(nrow(camp_data_holder)/initial), each=initial, length.out=nrow(camp_data_holder)))
  } else {
    cabin_loop_max = ceiling(nrow(camp_data_holder)/input$cabin_size)
    c <- split(camp_data_holder, rep(1:ceiling(nrow(camp_data_holder)/input$cabin_size), each=input$cabin_size, length.out=nrow(camp_data_holder)))
  }

  for (i in 1:cabin_loop_max){
    cabin_infections = sum(c[[i]]$status)
    cabin_quarantines = sum(c[[i]]$Quarantine)
    active_cabin_infections = nrow(c[[i]] %>% filter(status == 1 & Quarantine == 0))
    
    for (j in 1:nrow(c[[i]])){
      if(c[[i]]$Quarantine[j] == 0 & c[[i]]$status[j] == 0){
        if(c[[i]]$vaccine[j] == 0){
          c[[i]]$status[j] = rbinom(1,1, prob = ifelse(input$night_R0*active_cabin_infections > 1, 1, input$night_R0*active_cabin_infections))
        } else{
          c[[i]]$status[j] = rbinom(1,1, prob = ifelse(input$night_R0*active_cabin_infections > 1, 1, input$night_R0*active_cabin_infections*0.05))
        }
      } else {
        c[[i]]$status[j] = c[[i]]$status[j]
      }
    }
  }
  camp_data_holder = do.call("rbind", c)
  return(list(camp_data_holder = camp_data_holder))
}

summary_stats <- function(active_data, input){

  if (input$Analysis == 4){
    Rates <- active_data %>%
      group_by(initial) %>%
      filter(day == 0 & Active_Infections == 0) %>%
      summarise(Perfect_rate = n()/400)%>%
      left_join(active_data %>%
                  group_by(initial) %>%
                  filter(day == input$Duration & Infections >= initial/4) %>%
                  summarise(Quarter_rate = n()/400) ) %>%
      left_join(active_data %>%
                  group_by(initial) %>%
                  filter(day == 0 & Active_Infections >= 1) %>%
                  summarise(Import_rate = n()/400)) %>%
      gather(key = "Key", value="rate",2:4)
  } else if (input$Analysis != 4){
  active_data <- active_data %>%
    mutate(day = as.integer(day),
           Infections = as.integer(Infections))
  Rates <- 
    active_data %>%
    group_by(initial) %>%
    filter(day == 0 & Active_Infections >= 1) %>%
    summarise(Import_rate = n()/400) %>%
    full_join(active_data %>%
    group_by(initial) %>%
    filter(day == 0 & Active_Infections == 0) %>%
    summarise(Perfect_rate = n()/400))%>%
    full_join(active_data %>%
                group_by(initial) %>%
                filter(day == input$Duration & Infections >= input$camp_size/4) %>%
                summarise(Quarter_rate = n()/400) ) %>%
    gather(key = "Key", value="rate",2:4)

  }

  Rates[is.na(Rates)] <- 0
  return(list(Rates=Rates))
}
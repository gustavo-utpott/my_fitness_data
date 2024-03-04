# script for the server of the app


server <- function(input, output) {
  
  ###########################
  #        STEPS TAB        #
  ###########################
  
  # defining dataframes
  
  activities <- reactive({
    activities_brute %>%
      filter(date >= as.character(input$date_range[1])) %>%
      filter(date <= as.character(input$date_range[2]))
  }) 

  activities_weekly <- reactive({
    activities_brute_weekly %>%
      filter(date_ref >= as.character(input$date_range[1])) %>%
      filter(date_ref <= as.character(input$date_range[2]))
  }) 
  
  activities_monthly <- reactive({
    activities_brute_monthly %>%
      filter(date_ref >= as.character(input$date_range[1])) %>%
      filter(date_ref <= as.character(input$date_range[2]))
  }) 

  
  # Boxes
  
  output$box_steps_1 <- renderValueBox({
    
    aux <- activities()
    
    number <- sum(aux$step_count)
    
    valueBox(
      number,
      "Total Step Count",
      icon = icon("shoe-prints"),
      color = "lime"
    )
  })
  
  output$box_steps_2 <- renderValueBox({
    
    aux <- activities()
    
    number <- mean(aux$step_count, na.rm = T)
    
    valueBox(
      round(number,0),
      "Mean Step Count",
      icon = icon("shoe-prints"),
      color = "lime"
    )
  })
  
  output$box_steps_3 <- renderValueBox({
    
    aux <- activities()
    
    number <- max(aux$step_count)
    
    valueBox(
      number,
      "Most Steps in a Day",
      icon = icon("shoe-prints"),
      color = "lime"
    )
  })
  
  output$box_active_time <- renderValueBox({
    
    aux <- activities()
    
    number <- mean(aux$active_time)
    
    valueBox(
      round(number,0),
      "Mean active time",
      icon = icon("stopwatch-20"),
      color = "lime"
    )
  })
  
  
  # Charts
  
  output$chart_steps_1 <- renderPlotly({
    
    p <- ggplot(activities()) +
      geom_line(aes(x = date, y = step_count, color = "Daily")) +
      geom_line(aes(x = date, y = ma_3_steps, color = "3 day moving average")) +
      geom_line(aes(x = date, y = ma_7_steps, color = "7 day moving average")) +
      geom_line(aes(x = date, y = ma_31_steps, color = "31 day moving average")) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
      labs(y = "Step Count", color = "Type")
    
    ggplotly(p)
    
  })
  
  output$chart_steps_2 <- renderPlotly({
    
    p <- ggplot(activities_weekly()) +
      geom_line(aes(x = date_ref, y = mean_week_steps, color = "Weekly", group = 1)) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
      labs(y = "Step Count", color = "Type")
    
    ggplotly(p)
    
  })
  
  output$chart_steps_3 <- renderPlotly({
    
    p <- ggplot(activities_monthly()) +
      geom_col(aes(x = date_ref, y = mean_month_steps, fill = "Monthly", group = 1)) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b/%y") +
      labs(y = "Step Count", color = "Type")
    
    ggplotly(p)
    
  })
  
  
  output$chart_act_time_1 <- renderPlotly({
    
    aux <- activities()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date, y = ~aux$active_time, name = "Active time", mode = "lines", type = "scatter")
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Kcal")
    
    fig <- fig %>% add_trace(x = ~aux$date, y = ~aux$calorie, name = "Calories Spent", yaxis = "y2", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      yaxis2 = ay,
      xaxis = list(title="Date"),
      yaxis = list(title="Minutes")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_act_time_2 <- renderPlotly({
    
    aux <- activities_weekly()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_week_act_time, name = "Active time", mode = "lines+markers", type = "scatter")
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Kcal")
    
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_week_calorie_spent, name = "Calories Spent", yaxis = "y2", mode = "lines+markers", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      yaxis2 = ay,
      xaxis = list(title="Date"),
      yaxis = list(title="Minutes")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_act_time_3 <- renderPlotly({
    
    aux <- activities_monthly()
    
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_month_act_time, name = "Active time", mode = "lines+markers", type = "scatter")
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Kcal")
    
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_month_calorie_spent, name = "Calories Spent", yaxis = "y2", mode = "lines+markers", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      yaxis2 = ay,
      xaxis = list(title="Date"),
      yaxis = list(title="Minutes")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  
  ###########################
  #        HEART TAB        #
  ###########################
  
  # filter wearable
  
  intervalo_wear <- reactive({
    wearable <- filter(wearables, wearable %in% input$wearable)
    
    lubridate::interval(wearable$begin_date,wearable$end_date)
  })
    
  # defining dataframes
  
  heartbeats <- reactive({
    heartbeats_brute %>%
      filter(map_lgl(reference_day, ~ any(.x %within% intervalo_wear())))
  }) 
  
  heartbeats_weekly <- reactive({
    heartbeats_brute_weekly %>%
      filter(map_lgl(date_ref, ~ any(.x %within% intervalo_wear())))
  })
  
  heartbeats_monthly <- reactive({
    heartbeats_brute_monthly %>%
      filter(map_lgl(date_ref, ~ any(.x %within% intervalo_wear())))
  }) 
  
  rest_heart_rate <- reactive({
    rest_heart_rate_brute %>%
      filter(map_lgl(reference_day, ~ any(.x %within% intervalo_wear())))
  })  
  
  rest_heart_rate_weekly <-reactive({
    rest_heart_rate_brute_weekly %>%
      filter(map_lgl(date_ref, ~ any(.x %within% intervalo_wear())))
  })  
  
  rest_heart_rate_monthly <- reactive({
    rest_heart_rate_brute_monthly %>%
      filter(map_lgl(date_ref, ~ any(.x %within% intervalo_wear())))
  }) 
    
  
  # Boxes
  
  output$box_heart_1 <- renderValueBox({
    
    aux <- heartbeats()
    
    number <- mean(aux$mean_heart_rate)
    
    diff_mins <- difftime(max(aux$reference_day), min(aux$reference_day), units = "mins")
    
    valueBox(
      round(as.numeric(number*diff_mins),0),
      "Estimated Total Heartbeats",
      icon = icon("heart"),
      color = "lime"
    )
  })
  
  output$box_heart_2 <- renderValueBox({
    
    aux <- rest_heart_rate()
    
    number <- mean(aux$minimo, na.rm = T)
    
    valueBox(
      round(number,0),
      "Mean Resting Heart Rate",
      icon = icon("heart"),
      color = "lime"
    )
  })
  
  output$box_heart_3 <- renderValueBox({
    
    aux <- rest_heart_rate()
    
    number <- max(aux$maximo)
    
    valueBox(
      number,
      "Max Heart Rate",
      icon = icon("heart-circle-bolt"),
      color = "lime"
    )
  })
  
  output$box_heart_4 <- renderValueBox({
    
    aux <- heartbeats()
    
    number <- mean(aux$mean_heart_rate)
    
    valueBox(
      round(as.numeric(number),0),
      "Mean Heartbeat",
      icon = icon("heart"),
      color = "lime"
    )
  })
  
  
  # Charts
  
  output$chart_rhr_1 <- renderPlotly({
    
    aux <- rest_heart_rate()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$reference_day, y = ~aux$minimo, name = "Resting Heart Rate", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Beats per minute")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_rhr_2 <- renderPlotly({
    
    aux <- rest_heart_rate_weekly()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_week_heart_min, name = "Resting Heart Rate", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Beats per minute")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_rhr_3 <- renderPlotly({
    
    aux <- rest_heart_rate_monthly()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_month_heart_min, name = "Resting Heart Rate", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Beats per minute")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_mhr_1 <- renderPlotly({
    
    aux <- rest_heart_rate()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$reference_day, y = ~aux$maximo, name = "Max Heart Rate", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Beats per minute")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_mhr_2 <- renderPlotly({
    
    aux <- rest_heart_rate_weekly()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_week_heart_max, name = "Max Heart Rate", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Beats per minute")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_mhr_3 <- renderPlotly({
    
    aux <- rest_heart_rate_monthly()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_month_heart_max, name = "Max Heart Rate", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Beats per minute")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_mean_hr_1 <- renderPlotly({
    
    aux <- heartbeats()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$reference_day, y = ~aux$mean_heart_rate, name = "Mean Heart Rate", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Beats per minute")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_mean_hr_2 <- renderPlotly({
    
    aux <- heartbeats_weekly()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_week_heartbeats, name = "Mean Heart Rate", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Beats per minute")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_mean_hr_3 <- renderPlotly({
    
    aux <- heartbeats_monthly()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_month_heartbeats, name = "Mean Heart Rate", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Beats per minute")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  ###########################
  #        SLEEP TAB        #
  ###########################
  
  # defining dataframes
  
  sleep_time_daily <- reactive({
    sleep_brute_time_daily %>%
      filter(day_of_sleep >= as.character(input$date_range2[1])) %>%
      filter(day_of_sleep <= as.character(input$date_range2[2]))
  }) 
  
  sleep_time_weekly <- reactive({
    sleep_brute_time_weekly %>%
      filter(date_ref >= as.character(input$date_range2[1])) %>%
      filter(date_ref <= as.character(input$date_range2[2]))
  }) 
  
  sleep_time_monthly <- reactive({
    sleep_brute_time_monthly %>%
      filter(date_ref >= as.character(input$date_range2[1])) %>%
      filter(date_ref <= as.character(input$date_range2[2]))
  }) 
  
  sleep_phases <- reactive({
    sleep_brute_phases %>%
      filter(day_of_sleep >= as.character(input$date_range2[1])) %>%
      filter(day_of_sleep <= as.character(input$date_range2[2]))
  }) 
  
  sleep_phases_weekly <- reactive({
    sleep_brute_phases_weekly %>%
      filter(date_ref >= as.character(input$date_range2[1])) %>%
      filter(date_ref <= as.character(input$date_range2[2]))
  }) 
  
  sleep_phases_monthly <- reactive({
    sleep_brute_phases_monthly %>%
      filter(date_ref >= as.character(input$date_range2[1])) %>%
      filter(date_ref <= as.character(input$date_range2[2]))
  }) 
  
  
  # Boxes
  
  output$box_sleep_1 <- renderValueBox({
    
    aux <- sleep_time_daily()
    
    number <- mean(aux$total_amount_sleep)
    hours <- as.integer(number)
    minutes <- (sign(number) * (abs(number) %% 1))*60
    
    minutes <- ifelse(minutes > 10, as.integer(minutes), paste0("0",as.integer(minutes)))
    
    valueBox(
      paste0(hours, ":", minutes),
      "Mean Time Slept Daily",
      icon = icon("bed"),
      color = "lime"
    )
  })
  
  output$box_sleep_2 <- renderValueBox({ # média tempo q iniciei sono
    
    aux <- sleep_time_daily()
    
    number <- ifelse(mean(aux$bedtime) >= 86400, 
                     round(mean(aux$bedtime), 0) - 86400,
                     round(mean(aux$bedtime), 0))
    hours <- hour(seconds_to_period(number))
    minutes <- minute(seconds_to_period(number))
    
    minutes <- ifelse(minutes > 10, as.integer(minutes), paste0("0",as.integer(minutes)))
    
    
    valueBox(
      paste0(hours, ":", minutes),
      "Mean Bedtime",
      icon = icon("moon"),
      color = "lime"
    )
  })
  
  output$box_sleep_3 <- renderValueBox({ # média tempo q acordei
    
    aux <- sleep_time_daily()
    
    number <- seconds_to_period(round(mean(aux$wake_up_time), 0))
    hours <- hour(number)
    minutes <- minute(number)
    
    minutes <- ifelse(minutes > 10, as.integer(minutes), paste0("0",as.integer(minutes)))
    
    
    valueBox(
      paste0(hours, ":", minutes),
      "Mean Wake Up Time",
      icon = icon("sun"),
      color = "lime"
    )
  })
  
  output$box_sleep_4 <- renderValueBox({
    
    aux <- sleep_phases() %>%
      filter(stage == "40001") %>%
      mutate(real_time_slept = total_amount_sleep - time_in_phase)
    
    number <- mean(aux$real_time_slept, na.rm = T)
    hours <- as.integer(number)
    minutes <- (sign(number) * (abs(number) %% 1))*60
    
    minutes <- ifelse(minutes > 10, as.integer(minutes), paste0("0",as.integer(minutes)))
    
    valueBox(
      paste0(hours, ":", minutes),
      "Mean Real Time Slept",
      icon = icon("bed"),
      color = "lime"
    )
  })
  
  
  # Charts
  # 3 graphs: total amount of sleep, sleep phases, bedtime and wake time.
  
  output$chart_total_sleep_1 <- renderPlotly({
    
    aux <- sleep_time_daily()

    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$day_of_sleep, y = ~aux$total_amount_sleep, name = "Time Slept", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Hours")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
  })
  
  output$chart_total_sleep_2 <- renderPlotly({
    
    aux <- sleep_time_weekly()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_week_total_amount_sleep, name = "Time Slept", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Hours")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  output$chart_total_sleep_3 <- renderPlotly({
    
    aux <- sleep_time_monthly()
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% add_trace(x = ~aux$date_ref, y = ~aux$mean_month_total_amount_sleep, name = "Time Slept", mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Hours")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
    
  })
  
  # phases
  
  output$chart_sleep_phases_1 <- renderPlotly({
    
    aux <- sleep_phases()
    
    fig <- plot_ly() %>% 
      add_trace(x = ~aux$day_of_sleep, y = ~aux$time_in_phase, text = ~aux$time_in_phase_perc, color = ~aux$phase, mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Hours")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
  })
  
  output$chart_sleep_phases_2 <- renderPlotly({
    
    aux <- sleep_phases_weekly()
    
    fig <- plot_ly() %>% 
      add_trace(x = ~aux$date_ref, y = ~aux$mean_week_time_in_phase, text = ~aux$mean_week_time_in_phase_perc, color = ~aux$phase, mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Hours")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
  })
  
  output$chart_sleep_phases_3 <- renderPlotly({
    
    aux <- sleep_phases_monthly()
    
    fig <- plot_ly() %>% 
      add_trace(x = ~aux$date_ref, y = ~aux$mean_month_time_in_phase, text = ~aux$mean_month_time_in_phase_perc, color = ~aux$phase, mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Hours")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    fig
  })
  
  # bedtime and wake up time
  
  
  output$chart_bed_wake_1 <- renderPlotly({
    
    aux <- sleep_time_daily() %>%
      mutate(wake_up_time = wake_up_time + 86400) %>%
      mutate(bedtime_time = paste0(seconds_to_period(bedtime)$hour, ":", seconds_to_period(bedtime)$minute)) %>%
      mutate(wake_up_time_time = paste0(seconds_to_period(wake_up_time)$hour, ":", seconds_to_period(wake_up_time)$minute))
    
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% 
      add_trace(x = ~aux$day_of_sleep, y = ~aux$bedtime, text = ~aux$bedtime_time, name = "Bedtime", mode = "lines", type = "scatter") %>%
      add_trace(x = ~aux$day_of_sleep, y = ~aux$wake_up_time, text = ~aux$wake_up_time_time, name = "Wake Up Time", mode = "lines", type = "scatter")
    
    seq_val <- seq(min(aux$bedtime), max(aux$wake_up_time), length.out = 8)
    seq_time <- seconds_to_period(seq_val)
    seq_ticks <- paste0(seq_time$hour,":",seq_time$minute)
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Time")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff',
               tickmode = "array",
               tickvals = seq_val,
               ticktext = seq_ticks))
    
    fig
  })
  
  output$chart_bed_wake_2 <- renderPlotly({
    
    aux <- sleep_time_weekly() %>%
      mutate(mean_week_wake_up_time = mean_week_wake_up_time + 86400) %>%
      mutate(bedtime_time = paste0(seconds_to_period(mean_week_bedtime)$hour, ":", seconds_to_period(mean_week_bedtime)$minute)) %>%
      mutate(wake_up_time_time = paste0(seconds_to_period(mean_week_wake_up_time)$hour, ":", seconds_to_period(mean_week_wake_up_time)$minute))
    
    
    seq_val <- seq(min(aux$mean_week_bedtime), max(aux$mean_week_wake_up_time), length.out = 8)
    seq_time <- seconds_to_period(seq_val)
    seq_ticks <- paste0(seq_time$hour,":",seq_time$minute)
    
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% 
      add_trace(x = ~aux$date_ref, y = ~aux$mean_week_bedtime, name = "Bedtime", text = ~aux$bedtime_time, mode = "lines", type = "scatter") %>%
      add_trace(x = ~aux$date_ref, y = ~aux$mean_week_wake_up_time, name = "Wake Up Time", text = ~aux$wake_up_time_time, mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Time")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff',
               tickmode = "array",
               tickvals = seq_val,
               ticktext = seq_ticks))
    
    fig
    
  })
  
  output$chart_bed_wake_3 <- renderPlotly({
    
    aux <- sleep_time_monthly() %>%
      mutate(mean_month_wake_up_time = mean_month_wake_up_time + 86400) %>%
      mutate(bedtime_time = paste0(seconds_to_period(mean_month_bedtime)$hour, ":", seconds_to_period(mean_month_bedtime)$minute)) %>%
      mutate(wake_up_time_time = paste0(seconds_to_period(mean_month_wake_up_time)$hour, ":", seconds_to_period(mean_month_wake_up_time)$minute))
    
    
    
    seq_val <- seq(min(aux$mean_month_bedtime), max(aux$mean_month_wake_up_time), length.out = 8)
    seq_time <- seconds_to_period(seq_val)
    seq_ticks <- paste0(seq_time$hour,":",seq_time$minute)
    
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% 
      add_trace(x = ~aux$date_ref, y = ~aux$mean_month_bedtime, name = "Bedtime", text = ~aux$bedtime_time, mode = "lines", type = "scatter") %>%
      add_trace(x = ~aux$date_ref, y = ~aux$mean_month_wake_up_time, name = "Wake Up Time", text = ~aux$wake_up_time_time,mode = "lines", type = "scatter")
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      xaxis = list(title="Date"),
      yaxis = list(title="Hours")
    )%>%
      layout(plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff',
               tickmode = "array",
               tickvals = seq_val,
               ticktext = seq_ticks))
    
    fig
    
  })
  
  
  
  
  
  
}

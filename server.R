function(input, output) {
 
  filtering <- reactive({
    startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
    endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
    years <- endyear - startyear + 1
    solar_europe_de_nuts %>%
      filter(Stadt == input$selected_country) %>%
      filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00"))
  })  

   m2 <- reactive({
     startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     years <- endyear - startyear + 1
     solar_europe_de_nuts %>%
       filter(Stadt == input$selected_country) %>%
       filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>% 
       summarise(
       yieldm2 = sum(solar_watt) / years * input$efficency /1000,
       m = input$kwhy/yieldm2
       #c = "50"
       )
  })
   
   ############# Ursprüngliche Version. Filtering scheint nicht korrekt übernommen zu werden
#m2 <- reactive({
 #    data <- filtering()
  #   data %>% summarise(
   #    yieldm2 = sum(solar_watt) / years * input$efficency /1000,
    #   m = input$kwhy/yieldm2
    # )
   #})
   #############
   
   kwhyield <-reactive({
     startyear <- as.Date(input$date[1]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     endyear <- as.Date(input$date[2]) %>% as.character() %>% substr(1,4) %>% as.numeric()
     years <- endyear - startyear + 1
     solar_europe_de_nuts %>%
       filter(Stadt == input$selected_country) %>%
       filter(utc_timestamp >= paste0(startyear, "-01-01 00:00:00"), utc_timestamp <= paste0(endyear, "-12-31 24:00:00")) %>% 
       summarise(
        ms = sum(solar_watt) / years * input$efficency * input$m2 / 1000 
        
       )    
   })
   
  output$m <- renderInfoBox({
    m2 <- m2()
    valueBox("Benötigte Fläche in m²", prettyNum(m2$m))
  })
  
  output$yieldm2 <- renderInfoBox({
      m2 <- m2()
      valueBox(
          "Erzeugte kWh pro m²", prettyNum(m2$yieldm2))
  })
  
  output$ms <- renderInfoBox({
    kwhyield <- kwhyield()
    valueBox("Produzierte kWh pro m²",prettyNum(kwhyield$ms))
  })
  
  output$radiation_chart <- renderPlot({
    data <- filtering()
    data %>%
      mutate(day = utc_timestamp %>% as.character() %>% substr(6,10)) %>%
      group_by(day) %>%
      summarize(avg = mean(solar_watt, na.rm = TRUE), std = sd(solar_watt, na.rm = TRUE) / sqrt(n())) %>%
      mutate(date = as.Date(paste0("2019-", day))) %>% 
      inner_join(slpc) %>%
      mutate(standardlast = standardlast / m2() %>% pull(m)) %>%
      ggplot() + 
      aes(x = date) +
      geom_ribbon(aes(ymin = avg - 1.96 * std, ymax = avg + 1.96 * std), fill = "green") +
      geom_line(aes(y = avg)) +
      geom_smooth(aes(y = standardlast, color = "red")) +
      xlab("") +
      ylab("") +
      scale_x_date(labels = date_format("%B"))
  })
  
}

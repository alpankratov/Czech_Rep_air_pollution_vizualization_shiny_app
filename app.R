#https://alpankratov.shinyapps.io/Air_Pollution_in_Czech_Republic/ - link to Shiny App on server
library(shiny)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(rmarkdown)
library(leaflet)
library(mapview)
library(webshot)
#webshot::install_phantomjs() needs to be installed in order for interactive map could be pasted into word report as image 

 
load('measurements.RData')
load('stations.RData')
time_aggregation_list = c('Raw hourly data (no aggregation)', 
                          'Daily averages', 
                          'Daily maxima', 
                          'Number of hours per day for which a given threshold is exceeded', 
                          'Number of hours per year for which a given threshold is exceeded',
                          'Number of days per year for which the daily average concentration exceeds a given threshold')
thresholds <- as_tibble(rbind(c('NO2', 'Hourly', 200),
                              c('NO2', 'Yearly', 40),
                              c('PM10', 'Daily', 50),
                              c('PM10', 'Yearly', 40),
                              c('SO2', 'Hourly', 350),
                              c('SO2', 'Daily', 125),
                              c('PM2.5', 'Yearly', 25)))
colnames(thresholds) <- c('AirPollutant', 'Type', 'Threshold')
x_axis_presentation <- c('Calendar time',
                         'Date within the year (going from Jan 1st to Dec 31st)',
                         'Day within the week',
                         'Hour in the day')


ui <- fluidPage(
  titlePanel("Data on air pollution in Czech Republic collected from 2013 to 2019"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "AirPollutant", label = "Select air pollutant",
               choices = unique(measurements$AirPollutant), multiple = FALSE),
      selectInput(inputId = "Aggregation", label = "Select aggregation level",
                  choices = time_aggregation_list, multiple = FALSE),
      uiOutput("ui"),
      selectizeInput(inputId = "selected_stations", label = "Select stations", choices = unique(measurements$StationName), selected = NULL, multiple = TRUE,
                     options = list(maxItems = 4)),
      actionButton("GO","PLOT DATA"),
      downloadButton("downloadData", "Download Table"),
      downloadButton('downloadReport', 'Generate Report')
      ),
    mainPanel(h2("Concentration of selected pollutant with parameters defined by user"),
              h5(textOutput(outputId = 'threshold_info')),
              plotOutput(outputId ="pollutantPlot"),
              splitLayout(leafletOutput(outputId ="stationlocationsPlot"),
                          dataTableOutput(outputId ="DataTable"))
              
              
  )
  )
)

server <- function(input, output) {
  
  output$ui <- renderUI ({
    # Creating dynamic UI where choice of presentation of X-axis (time) depends on laval aggregation set by user 
    switch(input$Aggregation,
           'Raw hourly data (no aggregation)' = radioButtons(inputId = "X_axis", label = "Type of presentation of time axis",
                                                   choices = x_axis_presentation),
           'Daily averages' = radioButtons(inputId = "X_axis", label = "Type of presentation of time axis",
                                                     choices = x_axis_presentation[1:3]),
           'Daily maxima' = radioButtons(inputId = "X_axis", label = "Type of presentation of time axis",
                                           choices = x_axis_presentation[1:3]),
           'Number of hours per day for which a given threshold is exceeded' = radioButtons(inputId = "X_axis", 
                                                                                            label = "Type of presentation of time axis",
                                                                                            choices = x_axis_presentation[1:3]),
           'Number of hours per year for which a given threshold is exceeded' = radioButtons(inputId = "X_axis", 
                                                                                             label = "Type of presentation of time axis",
                                                                                             choices = x_axis_presentation[1]),
           'Number of days per year for which the daily average concentration exceeds a given threshold' = radioButtons(inputId = "X_axis", 
                                                                                             label = "Type of presentation of time axis",
                                                                                             choices = x_axis_presentation[1]),
           
           )
  })
  
  #Creating a dataframe with selected stations that will be used to tag location on map
  stations_to_map <- eventReactive(input$GO, {
    stations_to_map <- stations %>% 
      dplyr::filter(StationName %in% input$selected_stations)
  })
  
  #Creating a map of Czech Republic and plotting there selected station from reactive stations_to_map()
  stations_plot <- eventReactive(input$GO, {
    leaflet() %>% addTiles() %>% setView(15.5,50,7) %>% addTiles() %>% 
      addMarkers(data = stations_to_map(), lat = ~Latitude, lng = ~Longitude,  label = ~StationName)
  })
  
  #Creating the table with data on selected air pollutant registered in selected stations. This dataframe will then be used in
  #two other reactive comonents for (1) creating a table in wide-firmat to display in Shiny App and (2) plotting the data
  measurements_table <- eventReactive(input$GO, {
    measurements_plot <- measurements %>% 
      dplyr::filter(StationName %in% input$selected_stations & AirPollutant == input$AirPollutant)
    if (input$Aggregation == time_aggregation_list[1]) measurements_plot
    else if (input$Aggregation == time_aggregation_list[2]) {
      measurements_plot %>% group_by(Date, Year, Month, Day, StationName, AirPollutant, DateInYear, Weekday) %>% 
        summarize(Concentration = mean(Concentration, na.rm = TRUE)) %>% 
        ungroup() 
    }
    
    else if (input$Aggregation == time_aggregation_list[3]) {
      measurements_plot %>% group_by(Date, Year, Month, Day, StationName, AirPollutant, DateInYear, Weekday) %>% 
        summarize(Concentration = max(Concentration, na.rm = TRUE)) %>% 
        ungroup()
    }
    
    else if (input$Aggregation == time_aggregation_list[4]) {
      if (input$AirPollutant %in% c("PM10", 'PM2.5')) stop('Hourly threshold is not applicable for this pollutant.') else {
        measurements_plot %>% group_by(Date, Year, Month, Day, StationName, AirPollutant, DateInYear, Weekday) %>% 
          summarize(Concentration = sum(Concentration > as.double(thresholds[thresholds$Type == 'Hourly' & thresholds$AirPollutant == input$AirPollutant, 3], na.rm = TRUE))) %>%
          ungroup() 
      }
      
    }
    else if (input$Aggregation == time_aggregation_list[5]) {
      if (input$AirPollutant %in% c("PM10", 'PM2.5')) stop('Hourly threshold is not applicable for this pollutant') else {
        measurements_plot %>% group_by(Year, StationName) %>% 
          summarize(Concentration = sum(Concentration > as.double(thresholds[thresholds$Type == 'Hourly' & thresholds$AirPollutant == input$AirPollutant, 3], na.rm = TRUE))) %>% 
          ungroup() 
      }
    }
    else if (input$Aggregation == time_aggregation_list[6]) {
      if (input$AirPollutant %in% c("NO2", 'PM2.5')) stop('Daily average threshold is not applicable for this pollutant') else {
        measurements_plot %>% 
          group_by(Date, Year, StationName) %>% 
          summarize(Concentration = mean(Concentration, na.rm = TRUE)) %>% 
          group_by(Year, StationName) %>%
          summarize(Concentration = sum(Concentration > as.double(thresholds[thresholds$Type == 'Daily' & thresholds$AirPollutant == input$AirPollutant, 3], na.rm = TRUE))) %>% 
          ungroup()
      }
    }
    
    
  })
  
  #Dataframe created in reactive component measurements_table() is transformed into wide-format.
  #Columns that are duplicated are removed here as athough they are required to for plotting, here they decreasy understandability of the table
  #E.g. in plotting X-axis 'Date within a year' column DateInYear and Date provide the same information but in DateInYear column the year is fixed to "1" for plotting purposes
  measurements_table_to_display <- eventReactive(input$GO, {
    if (input$Aggregation == time_aggregation_list[1])
    measurements_table() %>%  
      select(Date, Hour, StationName, AirPollutant, Concentration) %>% 
      spread(key = StationName, value = Concentration)
    else if (input$Aggregation %in% time_aggregation_list[2:4]) {
      measurements_table() %>%
        select(-DateInYear, -Year, -Month, -Day) %>% 
        spread(key = StationName, value = Concentration)
    }
    else if (input$Aggregation %in% time_aggregation_list[5:6]) {
      measurements_table() %>%
        spread(key = StationName, value = Concentration)
    }
  })

  #Creating a line safety threshold as per EU air quality standard. This line is then added to the plot below.
  threshold_line <- eventReactive(input$GO, {
    if (input$Aggregation == time_aggregation_list[1]) {
      geom_hline(yintercept = as.double(thresholds[thresholds[1] == input$AirPollutant & thresholds[2] == "Hourly", 3]), 
                 color = "blue", lty = 'dashed', size = 0.5)
        
    }
    else if (input$Aggregation == time_aggregation_list[2] | input$Aggregation == time_aggregation_list[3]) {
      geom_hline(yintercept = as.double(thresholds[thresholds[1] == input$AirPollutant & thresholds[2] == "Daily", 3]), 
                 color = "blue", lty = 'dashed', size = 0.5)
    }
    
  })
  
  #Creating a plot of data selected by user.
  measurements_table_to_plot <- eventReactive(input$GO, {
    if (input$X_axis == x_axis_presentation[1]) {
      if (input$Aggregation %in% time_aggregation_list[1:3]) {
        measurements_table() %>% 
          ggplot() +
          geom_line(mapping = aes(x = Date, y = Concentration, color = StationName)) + 
          threshold_line()
      }
      #I deviate here a bit from the project requirements by plotting number of Hours per Day and Days/Hours per Year with X-axis 'Calendar time'
      #not as line but as points. I consider that using points is more appropriate for this plot because there are a lot of 0 values that makes
      #non-zero values on the line plot essentialy verticle lines and removing zero values would give users incorrect impression that there were no Hours and Days
      #when concentration was below the safety threshold.
      else if (input$Aggregation %in% time_aggregation_list[4]) {
        measurements_table() %>% 
          ggplot() +
          geom_point(mapping = aes(x = Date, y = Concentration, color = StationName)) + 
          threshold_line()
      }
      else if (input$Aggregation %in% time_aggregation_list[5:6]) {
        measurements_table() %>% 
          ggplot() +
          geom_point(mapping = aes(x = Year, y = Concentration, color = StationName), size = 3) + 
          threshold_line()
      }
    }
    else if (input$X_axis == x_axis_presentation[2]) {
      measurements_table() %>% 
        ggplot() +
        geom_point(mapping = aes(x = DateInYear, y = Concentration, color = StationName), alpha = 0.3) + 
        scale_x_date(breaks = "month", date_labels = "%B") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        threshold_line()
    }
    else if (input$X_axis == x_axis_presentation[3]) {
      measurements_table() %>% 
        ggplot() +
        geom_jitter(mapping = aes(x = Weekday, y = Concentration, color = StationName), alpha = 0.3) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        threshold_line()
    }
    else if (input$X_axis == x_axis_presentation[4]) {
      measurements_table() %>% 
        ggplot() +
        geom_jitter(mapping = aes(x = Hour, y = Concentration, color = StationName), alpha = 0.3) +
        scale_x_continuous("Hour", labels = as.character(c(0:23)), breaks = c(0:23)) + 
        threshold_line()
    }
    
  })
  
  thresholds_info <- eventReactive(input$GO, {
    if(input$AirPollutant == "NO2") c('The current air quality standards in the EU for Nitrogen dioxide (NO2): hourly concentration exceeding 200 mu*g/m3 for at most 18 hours per year, and average yearly concentration of at most 40 mu*g/m3. Blue dashed line on the chart below (where applicable) shows the threshold')
    else if(input$AirPollutant == "PM10") c('The current air quality standards in the EU for Particulates (PM10): daily average exceeding 50 mu*g/m3 observed on at most 35 days a year, and yearly average of at most 40 mu*g/m3. Blue dashed line on the chart below (where applicable) shows the threshold')
    else if(input$AirPollutant == "PM2.5") c('The current air quality standards in the EU for Fine particulates (PM2.5): yearly average of at most 25 mu*g/m3. Blue dashed line on the chart below (where applicable) shows the threshold')
    else if(input$AirPollutant == "SO2") c('The current air quality standards in the EU for Sulphur dioxide (SO2): hourly concentration exceeding 350 mu*g/m3 for at most 24 hours per year, and average daily concentration exceeding 12 mu*g/m3 on at most 3 days per year. Blue dashed line on the chart below (where applicable) shows the threshold')
  })
  
  


  output$pollutantPlot <- renderPlot({
    measurements_table_to_plot()
    
    })
  output$stationlocationsPlot <- renderLeaflet({
    stations_plot()
    
    })
  output$DataTable <- renderDataTable(
    measurements_table_to_display(),
    options = list(pageLength = 5, lengthMenu = c(5, 10)))

  
  output$downloadData <- downloadHandler(
    filename = "measurements data.csv",
    content = function(file) {
      write.csv(measurements_table_to_display(), file, row.names = FALSE)
    }
  )
  output$threshold_info <- renderText({
    thresholds_info()
  })
  output$downloadReport <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.docx",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(AirPollutant = input$AirPollutant,
                       Aggregation = input$Aggregation,
                       X_axis = input$X_axis,
                       selected_stations = input$selected_stations,
                       thresholds = thresholds,
                       plot = measurements_table_to_plot(),
                       stationmap = stations_plot(),
                       table = measurements_table_to_display())
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  
  
}
shinyApp(ui, server)



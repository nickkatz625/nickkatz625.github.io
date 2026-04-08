###################################################################################
# This R script needs to be submitted.
# It should contain all relevant codes you have used to generate the Shiny app.
# Codes should be well-commented.   
# Do NOT include codes that you have experimented with, or is not connected to the app.
###################################################################################
library(tidyverse)
library(rvest)
library(shiny)
library(readxl)
library(knitr)
library(dplyr)
library(corrplot)
library(ggthemes)
library(janitor)
library(ggplot2)
library(scales)
library(shinydashboard)


################################################################################
#loading dataset
CMJdata <- read_xlsx("forceplate.xlsx")
SJdata <- read_xlsx("SJdata.xlsx")

#selecting for high correlation variables within each dataset

#CMJ: taken 3 highest variable importance values from Random Forest model
#from 208 final project: flight time, avg propuslive Power, propulsive net impulse
#chose later to only display velocity with CMJ


#SJ: taken 4 highest metrics from 2 sources: 1 from driveline baseball's research on force plate
#velocity models, 3 from a simple correlation test ran by the S & C director at KPI
#Peak Power, Jump momentum, propulsive net impulse, flight time.

#cleaning to remove names
CM_data <- CMJdata %>% 
  clean_names() %>%
  select(velocity, peak_propulsive_power, flight_time, propulsive_net_impulse, -name)

SJ_data <- SJdata %>%
  clean_names() %>%
  select(velocity, peak_propulsive_power, jump_momentum, propulsive_net_impulse, flight_time)


#Converting data into long form and formatting columns
SJ_long <- SJ_data %>%
  pivot_longer(cols = -velocity, names_to = "metric", values_to = "value")


CM_long <- CM_data %>%
  pivot_longer(cols = -velocity, names_to = "metric", values_to = "value")


#getting percentile values for both velocity and metric - numeric (0-1), for plots
SJ_percentile <- SJ_long %>%
  group_by(metric) %>%
  mutate(
    metric_percentile = percent_rank(value),
    velocity_percentile = percent_rank(velocity)) %>%
  ungroup()

CM_percentile <- CM_long %>%
  group_by(metric) %>%
  mutate(
    metric_percentile = percent_rank(value),
    velocity_percentile = percent_rank(velocity)) %>%
  ungroup()


#converting percentile values into percentages and scaling within 1 decimal - for tables
SJ_Sum <- SJ_percentile %>%
  mutate(
    metric_percentile = scales::percent(x = metric_percentile, accuracy = 0.1),
    velocity_percentile = scales::percent(x = velocity_percentile, accuracy = 0.1)) %>%
  rename(
    'Metric Percentile' = metric_percentile,
    'Velocity Percentile' = velocity_percentile)


CM_Sum <- CM_percentile %>%
  mutate(
    metric_percentile = scales::percent(x = metric_percentile, accuracy = 0.1),
    velocity_percentile = scales::percent(x = velocity_percentile, accuracy = 0.1)) %>%
  rename(
    'Metric Percentile' = metric_percentile,
    'Velocity Percentile' = velocity_percentile)




################################################################################
# shiny app code

# define UI 
ui <- dashboardPage(
  #title
  dashboardHeader(title = "Force Plate Report"),
  dashboardSidebar(
    sidebarMenu(
      #two different pages; one for each jump
      menuItem("CMJ", tabName = "cmj"),
      menuItem("SJ", tabName = "sj"))),
dashboardBody(
    tabItems(
      
      #first tab: 3 metrics and Velocity
      tabItem(tabName = "cmj",
              fluidRow(
                box(title = "Select Metrics", status = "primary", solidHeader = TRUE,
                    width = 12, 
                           sliderInput("input_cmvelocity", "Velocity (mph)", min = 50, max = 100, value = 85, step = 1),
                           sliderInput("input_cmmetric1", "Flight Time (s)", min = 0.3, max = 0.9, value = 0.6, step = 0.025),
                           sliderInput("input_cmmetric2", "Peak Propulsive Power (W)", min = 2000, max = 8000, value = 6000, step = 100),
                           sliderInput("input_cmmetric3", "Propulsive Net Impulse (Ns)", min = 100, max = 400, value = 250, step = 10))),
    
    #summary table showing percentile information about input metrics
              fluidRow(
                box(title = "Summary", width = 12, status = "success", solidHeader = TRUE,
                    tableOutput("cmj_summary_table"))),
    
     #4 separate plot commands for the 4 different slider selections
              fluidRow(
                box(title = "Peak Propulsive Power", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("cmj_power_plot")),
                box(title = "Propuslive Net Impulse", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("cmj_impulse_plot")),
                box(title = "Flight Time", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("cmj_flight_plot")),
                box(title = "Velocity", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("cmj_velo_plot")))),
              

      
      
      #repeated for other jump type - 4 metrics, no velocity
      tabItem(tabName = "sj",
              fluidRow(
                box(title = "select metrics:", status = "primary", solidHeader = TRUE,
                    width = 12,
                    column(12, 
                           sliderInput("input_sjmetric1", "Flight Time (s)", min = 0.3, max = 0.9, value = 0.6, step = 0.025),
                           sliderInput("input_sjmetric2", "Peak Propulsive Power (W)", min = 2000, max = 8000, value = 6000, step = 100)),
                           sliderInput("input_sjmetric3", "Propulsive Net Impulse (Ns)", min = 100, max = 400, value = 250, step = 10),
                           sliderInput("input_sjmetric4", "Jump Momentum (Kg*m/s)", min = 50, max = 350, value = 250, step = 10))),      
              fluidRow(
                box(title = "Summary", width = 12, status = "success", solidHeader = TRUE,
                    tableOutput("sj_summary_table"))),
              
              fluidRow(
                box(title = "Peak Propulsive Power", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("sj_power_plot")),
                box(title = "Propuslive Net Impulse", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("sj_impulse_plot")),
                box(title = "Flight Time", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("sj_flight_plot")),
                box(title = "Jump Momentum", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("sj_moment_plot")))))))


#This function, with help from chatGPT, that allows for easier plotting of all metrics in the format I wanted
#df for our data
#metric_name to specify metric in selector
#input value of that metric

percentile_band_plot <- function(df, metric_name, input_val, title  = NULL) {
  #this filters the data to only include the chosen metric rows
  df_metric <- df %>%filter(metric == metric_name)
  #using the Empirical Cumulative Distribution Function, found percentiles based on specific metric and input value
  input_percentile <- ecdf(df_metric$value)(input_val)
   #need to return object we are using; in this case, the plots
  plot <- ggplot(df_metric, aes(x = value, y = metric_percentile)) +
   #creates the density plot for ecdf command with stat = identity for the curve
    stat_ecdf(geom = "area", fill = "skyblue", alpha = 0.5) +
    #creates vertical line with the input value to mark where on percentile curve the user is
    geom_vline(xintercept = input_val, color = "darkred", size = 1.5) +
    #this adds the percent value based on the ecdf calculation and changes based on input
    annotate("text", x = input_val, y = 1, label = paste0(round(100 * input_percentile, 1), "%"),
             vjust = -0.5, hjust = 1.2, color = "black", size = 4) +
    labs(x = "Metrc Value", y = "Percentile (0-100)") +
    theme_minimal()
  return(plot)}

#similar function for velocity plots
velocity_percentile_plot <- function(df, input_val, title  = NULL) {
  input_percentile <- ecdf(df$velocity)(input_val)
    plot <- ggplot(df, aes(x = velocity)) +
    stat_ecdf(geom = "area", fill = "salmon", alpha = 0.5) +
    geom_vline(xintercept = input_val, color = "red3", linewidth = 1.5) +
        annotate("text", x = input_val, y = 1, label = paste0(round(100 * input_percentile, 1), "%"),
             vjust = -0.5, hjust = 1.2, color = "darkred", size = 4) +
    labs(x = "Velocity", y = "Percentile (0-100)") +
    theme_minimal()
  return(plot)}



#Table function - for summary with velocity (CMJ) - if else function to handle velocity (column) vs metric pulling 
#within a column. 
sum_table <- function(df_Sum, input_list, metric_lookup) {
  #from tidyverse - purrr package -> map_dfr: loops over each label name to apply row binding tibble output - 
  #it gives us a binded summary of all metrics or labels needed
  purrr::map_dfr(names(input_list), function(label) {
    
    #within the metric lookup, it takes the column name to ensure names allign and get puled properly
    metric_col <- metric_lookup[[label]]
    #takes the input value (selector) for a specific metric or label
    input_val <- input_list[[label]]
    
    #if else function - treat velocity as a column
    if(metric_col == "velocity") {
      percentile_val <- df_Sum %>%
        #had some issues with duplication, ensured only 1 pull 
        distinct(velocity, `Velocity Percentile`) %>%
        #closest value within dataset to the input - gives us the value to present a percentile with
        filter(abs(velocity - input_val) == min(abs(velocity - input_val))) %>%
        #pulls the percentile for that value
        pull(`Velocity Percentile`) 
      #Gets average of the velocity for the table, and assigns the output names and respective input pulls  
      mean_val <- mean(df_Sum$velocity, na.rm = TRUE)
      tibble(
        Metric = label,
        `Input Value` = input_val,
         `Percentile` = percentile_val,
        `Average` = round(mean_val, 1))}
    #when metrics are from the column metric - this was my initial piece, added velocity and if else later
    else{
      #pulls from the metric column to select the metric 
    df_fil <- df_Sum %>%
      filter(metric == metric_col)  
    #similar to above, pulls closest dataset value
    percentile_val <- df_fil %>%
      filter(abs(value - input_val) == min(abs(value - input_val))) %>%
      pull('Metric Percentile') %>%
      #this pulls the first entry only, fixed a duplication issue
      first()
    mean_val <- mean(df_fil$value, na.rm = TRUE)
    
    tibble(
      Metric = label,
      'Input Value' = input_val,
      'Percentile' = percentile_val,
      `Average` = round(mean_val, 1))}})}

#Table function for Sj, without velocity
sj_sum_table <- function(df_Sum, input_list, metric_lookup) {
  purrr::map_dfr(names(input_list), function(label) {
    
    metric_col <- metric_lookup[[label]]
    input_val <- input_list[[label]]
    
    df_fil <- df_Sum %>%
      filter(metric == metric_col)  
  #setup is similar - however, was getting NA values for percentile. tried multiple other tweaks.
  #with advice from chat gpt, the slice_min with the with_ties command could help fix it
  #this finds the difference between the value and the input, selecting one row (n=1)
  #with_ties = FALSE ensures 1 value is chosen, so no duplication or NA errors can occur
    match_row <- df_fil %>%
      slice_min(order_by = abs(value - input_val), n = 1, with_ties = FALSE)
    percentile_val <- match_row %>%
      pull(`Metric Percentile`) 
    
    mean_val <- mean(df_fil$value, na.rm = TRUE)
    
    tibble(
      Metric = label,
      'Input Value' = input_val,
      `Percentile` = percentile_val,
      `Average` = round(mean_val, 1))})}

#these lookup lists helped with the functions. they standarized my naming of variables
cmj_metric_lookup <- list(
  "Velocity (mph)" = "velocity", 
  "Flight Time (s)" = "flight_time",
  "Peak Propulsive Power (W)" = "peak_propulsive_power",
  "Propulsive Net Impulse (Ns)" = "propulsive_net_impulse")

sj_metric_lookup <- list(
  "Flight Time (s)" = "flight_time",
  "Peak Propulsive Power (W)" = "peak_propulsive_power",
  "Propulsive Net Impulse (Ns)" = "propulsive_net_impulse",
  "Jump Momentum (Kg*m/s)" = "jump_momentum")


# define server logic 
server <- function(input, output) {
  
 
  
  #CMJ Plots
  output$cmj_flight_plot <- renderPlot({
    var_name <- cmj_metric_lookup[["Flight Time (s)"]]
    input_val <- input$input_cmmetric1
    percentile_band_plot(CM_percentile, var_name, input_val)
  })
  output$cmj_power_plot <- renderPlot({
    var_name <- cmj_metric_lookup[["Peak Propulsive Power (W)"]]
    input_val <- input$input_cmmetric2
    percentile_band_plot(CM_percentile, var_name, input_val)
  })
    output$cmj_impulse_plot <- renderPlot({
      var_name <- cmj_metric_lookup[["Propulsive Net Impulse (Ns)"]]
      input_val <- input$input_cmmetric3
      percentile_band_plot(CM_percentile, var_name, input_val)
    })
    output$cmj_velo_plot <- renderPlot({
      input_val <- input$input_cmvelocity
      velocity_percentile_plot(CM_data, input_val)
      })
    
  #CMJ table
    output$cmj_summary_table <- renderTable({
      input_values <- list(
        "Flight Time (s)" = input$input_cmmetric1,
        "Peak Propulsive Power (W)" = input$input_cmmetric2,
        "Propulsive Net Impulse (Ns)" = input$input_cmmetric3,
        "Velocity (mph)" = input$input_cmvelocity)
      sum_table(CM_Sum, input_values, cmj_metric_lookup)})

#Squat Jump Output
  output$sj_flight_plot <- renderPlot({
    var_name <- sj_metric_lookup[["Flight Time (s)"]]
    input_val <- input$input_sjmetric1
    percentile_band_plot(SJ_percentile, var_name, input_val)
  })
  output$sj_power_plot <- renderPlot({
    var_name <- sj_metric_lookup[["Peak Propulsive Power (W)"]]
    input_val <- input$input_sjmetric2
    percentile_band_plot(SJ_percentile, var_name, input_val)
  })
    output$sj_impulse_plot <- renderPlot({
      var_name <- sj_metric_lookup[["Propulsive Net Impulse (Ns)"]]
      input_val <- input$input_sjmetric3
      percentile_band_plot(SJ_percentile, var_name, input_val)
    })
  
    output$sj_moment_plot <- renderPlot({
      var_name <- sj_metric_lookup[["Jump Momentum (Kg*m/s)"]]
      input_val <- input$input_sjmetric4
      percentile_band_plot(SJ_percentile, var_name, input_val)
    })
    
    output$sj_summary_table <- renderTable({
      input_values <- list(
        "Flight Time (s)" = input$input_sjmetric1,
        "Peak Propulsive Power (W)" = input$input_sjmetric2,
        "Propulsive Net Impulse (Ns)" = input$input_sjmetric3,
        "Jump Momentum (Kg*m/s)" = input$input_sjmetric4)
      sj_sum_table(SJ_Sum, input_values, sj_metric_lookup)})
    }
    
# run the app
shinyApp(ui = ui, server = server)


################################################################################
# citations/references

#https://www.hawkindynamics.com/hawkin-metric-database

#https://www.drivelinebaseball.com/2021/05/predicted-pitch-velocity/?srsltid=AfmBOopa6wHb2hwisCwM4HqpjrIzkHD8gQNOG3_esGzNBy8Z94HVkyua

#https://rstudio.github.io/shinydashboard/

#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/ecdf


################################################################################

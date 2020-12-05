## load packages
library(tidyverse)
#install.packages("shiny")
library(shiny)
#install.packages("leaflet")
library(leaflet)
#install.packages("geojsonio")
library(geojsonio)

## load data

restrictions_df <- 
  read_csv("data/restrictions.csv")

mhealth_df =
  read_csv("data/Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv") %>%
  janitor::clean_names() %>% 
  rename(mhealth_ind = indicator,
         mhealth_value = value,
         mhealth_lowci = low_ci,
         mhealth_highci =  high_ci,
         mhealth_ci = confidence_interval,
         mhealth_qrange = quartile_range)

access_df =
  read_csv("data/Indicators_of_Reduced_Access_to_Care_Due_to_the_Coronavirus_Pandemic_During_Last_4_Weeks.csv") %>% 
  janitor::clean_names() %>% 
  rename(access_ind = indicator,
         access_value = value,
         access_lowci = low_ci,
         access_highci =  high_ci,
         access_ci = confidence_interval,
         access_qrange = quartile_range,
         time_period = week,
         time_period_label = week_label)

combined_df <- 
  left_join(mhealth_df, access_df, by = c("phase", "group", "state", "subgroup", "time_period", "time_period_label"))  %>% 
  relocate("phase", "group", "state", "subgroup", "time_period", "time_period_label")

## import US states .json file
states <- geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")

## filter MH and access data

combined_df <- combined_df %>%
  filter(group == "By State", access_ind == "Delayed or Did Not Get Care, Last 4 Weeks", mhealth_ind == "Symptoms of Anxiety Disorder or Depressive Disorder")

## make color palettes based on data values
pal <- colorNumeric(
  palette = "Blues",
  domain = combined_df$mhealth_value)

pal2 <- colorNumeric(
  palette = "Greens",
  domain = combined_df$access_value)

## make time period labels for slider
week_labels <- combined_df %>% select(time_period_label) %>% unique

## build Shiny UI
ui <- fluidPage(
  
  tags$h1("Mental Health Symptoms and Access to Care During COVID-19"),
  
  br(),
  
  tags$body("Data obtained from the Household Pulse Survey, which measures frequency of anxiety or depression symptoms (% MH symptoms) and frequency of delayed or reduced access to care (% reduced access) during the coronavirus pandemic"),

  br(),
  br(),
  
  fluidRow(
  column(6, offset = 3, wellPanel(
    sliderInput("week", "Survey time period:", min = 1, max = 17, value = 1, animate=animationOptions(2000, loop = TRUE)
),
    
    tags$h4(textOutput("week_label"))
      )),
  
  br(),
  br(),
  
  fluidRow(
    column(6, leafletOutput("mh_map")),
    
    column(6, leafletOutput("access_map"))
  ),
  
  br(),
  br(),
  
  fluidRow(
    
  column(6, plotOutput("combined_plot")),
  
  column(6, tableOutput("restrictions_table"))
    
  ),
  br(),
  br()

))

## build Shiny server
server <- function(input, output) {
  
  
  combined_updated <- reactive({ combined_df %>% filter(time_period == input$week) })
  
  output$week_label <- renderText({
    paste(week_labels[input$week,])
    
  })
  
  output$mh_map <- renderLeaflet({
    leaflet(combined_updated()) %>% addTiles() %>%
      addPolygons(
        data = states, 
        fillOpacity = 1, 
        color = ~pal(combined_updated()$mhealth_value), 
        stroke = FALSE, 
        smoothFactor = 0.2,
        label = paste(combined_updated()$state, combined_updated()$mhealth_value),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      
      addLegend(
        pal = pal,
        values = ~combined_df$mhealth_value,
        opacity = 0.7,
        title = "% MH symptoms",
        position = "bottomright") 
  })
  
  output$access_map <- renderLeaflet({
    leaflet(combined_updated()) %>% addTiles() %>%
      addPolygons(
        data = states, 
        fillOpacity = 1, 
        color = ~pal2(combined_updated()$access_value), 
        stroke = FALSE, 
        smoothFactor = 0.2,
        label = paste(combined_updated()$state, combined_updated()$access_value),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      
      addLegend(
        pal = pal2,
        values = ~combined_df$access_value,
        opacity = 0.7,
        title = "% reduced access",
        position = "bottomright") 
  })
  

  
  output$combined_plot <- renderPlot({
    
    combined_updated() %>% 
      ggplot(aes(x = mhealth_value, y = access_value)) +
      geom_point() +
      geom_smooth(method= "lm") +
      labs(
        x = "Symptoms of Anxiety or Depressive Disorder (%)",
        y = "Delayed or Did Not Get Care in the Last 4 Weeks (%)",
        title = "Delayed or No Access to Care vs. Symptoms of Anxiety or Depression")
      })
    
  output$restrictions_table <- renderTable({restrictions_df})
    
  
  
}

## run Shiny
shinyApp(ui, server)


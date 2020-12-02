## load packages
library(tidyverse)
#install.packages("shiny")
library(shiny)
#install.packages("leaflet")
library(leaflet)
#install.packages("geojsonio")
library(geojsonio)

## load data
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

## import US states .json file
states <- geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")

## filter MH and access data
mhealth_map <- mhealth_df %>% 
  filter(group == "By State", mhealth_ind == "Symptoms of Anxiety Disorder or Depressive Disorder")

access_map <-  access_df %>% 
  filter(group == "By State", access_ind == "Delayed or Did Not Get Care, Last 4 Weeks")

## make color palettes based on data values
pal <- colorNumeric(
  palette = "Blues",
  domain = mhealth_map$mhealth_value)

pal2 <- colorNumeric(
  palette = "Greens",
  domain = access_map$access_value)

## make time period labels for slider
week_labels <- mhealth_map %>% select(time_period_label) %>% unique

## build Shiny UI
ui <- fluidPage(
  
  
  wellPanel(
    sliderInput("week", "Survey time period:", min = 1, max = 17, value = 1),
    
    textOutput("week_label")
  ),
  
  fluidRow(
    column(6, leafletOutput("mh_map")),
    
    column(6, leafletOutput("access_map"))
  ),
  

  fluidRow(
    column(4, offset = 3, img(src = "coronavirus_cases.png", width = 900))
  )
)

## build Shiny server
server <- function(input, output) {
  
  
  mhealth_updated <- reactive({ mhealth_map %>% filter(time_period == input$week) })
  
  output$week_label <- renderText({
    paste(week_labels[input$week,])
    
  })
  
  output$mh_map <- renderLeaflet({
    leaflet(mhealth_updated()) %>% 
      addPolygons(
        data = states, 
        fillOpacity = 1, 
        color = ~pal(mhealth_updated()$mhealth_value), 
        stroke = FALSE, 
        smoothFactor = 0.2,
        label = paste(mhealth_updated()$state, mhealth_updated()$mhealth_value),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      
      addLegend(
        pal = pal,
        values = ~mhealth_map$mhealth_value,
        opacity = 0.7,
        title = "% MH symptoms",
        position = "bottomright") 
  })
  
  access_updated <- reactive({ access_map %>% filter(time_period == input$week) })
  
  output$access_map <- renderLeaflet({
    leaflet(access_updated()) %>% 
      addPolygons(
        data = states, 
        fillOpacity = 1, 
        color = ~pal2(access_updated()$access_value), 
        stroke = FALSE, 
        smoothFactor = 0.2,
        label = paste(access_updated()$state, access_updated()$access_value),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      
      addLegend(
        pal = pal2,
        values = ~access_map$access_value,
        opacity = 0.7,
        title = "% reduced access",
        position = "bottomright") 
  })
  
}

## run Shiny
shinyApp(ui, server)


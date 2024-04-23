#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(tidyverse)

d = readRDS('EVstations.rds')
colnames(d) = tolower(colnames(d))

d = d %>%
  select(-matches('cng|lng|lpg|hydrogen|e85|bd[.]blends|french|^ng[.]|plus4|level1')) %>%
  filter(fuel.type.code=='ELEC') %>%
  rename(lev2=ev.level2.evse.num, 
         lev3=ev.dc.fast.count, 
         network = ev.network, 
         lat = latitude, 
         lon = longitude) %>%
  mutate(status = case_when(status.code=='E' ~ 'avail', 
                            status.code=='P' ~ 'planned', 
                            status.code=='T' ~ 'temp.unavail', 
                            TRUE ~ ''), 
         lev2 = ifelse(is.na(lev2), 0, lev2), 
         lev3 = ifelse(is.na(lev3), 0, lev3)) %>%
  mutate(network = ifelse(network=='Tesla Destination', 'Tesla', network), 
         network = gsub('Ã©', 'E', network), 
         network = gsub('Ã‰', 'E', network))
d = d %>%
  mutate(network = case_when(
    network %in% c('Tesla', 'Electrify America', 
                   'ChargePoint', 'eVgo', 'Non-Networked') ~ network, 
    TRUE ~ 'Other'))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("EV stations"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("state", 
                  "Select a state", 
                  choices = sort(unique(d$state)), 
                  selected = "CT"), 
      selectInput("level", 
                  "Select a level", 
                  choices = c("Level 2", "Level 3"), 
                  selected = "Level 3"),
      selectInput("network", 
                  "Select a network", 
                  choices = c(unique(d$network), "All Networks"), 
                  selected = "All Networks"), 
      
      # add date range
      # Copy the line below to make a date range selector
      # default: 2015-01-01 to present
      dateRangeInput("dates", label = h3("Date range"), 
                     start = "2015-01-01"),
      
      hr(),
      fluidRow(column(4, verbatimTextOutput("value")))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map = renderLeaflet({
    
    dg = d %>%
      filter(state == input$state) %>% 
      filter(if(input$network == 'All Networks') TRUE 
             else network == input$network) %>%
      filter(if(input$level == 'Level 2') lev2 > 0 else lev3 > 0) %>% 
      filter(open.date >= input$dates[1] & open.date <= input$dates[2])
    
    
    leaflet(dg) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, 
                       radius = ~(lev2 + lev3), 
                       color = ~network, 
                       popup = ~paste(network, "<br>", 
                                      station.name, "<br>", 
                                      street.address, "<br>", 
                                      city, ", ", state, " ", zip, "<br>", 
                                      "Level 2: ", lev2, "<br>", 
                                      "Level 3: ", lev3, "<br>", 
                                      "Open Date: ", open.date))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

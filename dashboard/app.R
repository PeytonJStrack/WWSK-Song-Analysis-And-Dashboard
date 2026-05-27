library(shiny)
library(shinydashboard)
library(tidyverse)

# Load data
github_url <- "https://raw.githubusercontent.com/PeytonJStrack/WWSK-Song-Analysis-And-Dashboard/main/data/Shark_Data.csv"

songs <- read_csv(github_url, show_col_types = FALSE)

ui <- dashboardPage(
  dashboardHeader(title = "94.3 The Shark Radio Analytics"), dashboardSidebar(selectInput("artist", "Select Artist:", choices = c("All", sort(unique(songs$Artist))))),
  dashboardBody(
    fluidRow(valueBoxOutput("total", width = 3), valueBoxOutput("artists", width = 3), valueBoxOutput("songs", width = 3), valueBoxOutput("top_artist", width = 3)),
    fluidRow(box(width = 6, title = "Top Artists", status = "primary", solidHeader = TRUE,  plotOutput("artist_plot", height = "400px")), box(width = 6, title = "Recent Songs", status = "primary", solidHeader = TRUE, tableOutput("recent_songs")))))

total_songs <- nrow(songs)

unique_songs <- n_distinct(songs$Song)

unique_artists <- n_distinct(songs$Artist)

top_artist <- songs %>%
  count(Artist, sort=TRUE) %>%
  slice(1)

server <- function(input, output)
{
  filtered_data <- reactive(
  {
    if(input$artist=="All")
    {
      songs
    } else
    {
      songs %>%
        filter(Artist == input$artist)
    }
  })
  
  output$total <- renderValueBox(
  {
    valueBox(nrow(songs), "Total Songs")
  })
  
  output$artists <- renderValueBox(
  {
    valueBox(n_distinct(songs$Artist), "Unique Artists")
  })
  
  output$songs <- renderValueBox(
  {
    valueBox(n_distinct(songs$Song), "Unique Songs")
  })
  
  output$top_artist <- renderValueBox(
  {
    top_artist <- songs %>%
      count(Artist, sort = TRUE) %>%
      slice(1)
    
    valueBox(value = top_artist$n, subtitle = paste("Top:", stringr::str_trunc(top_artist$Artist, width = 15)))
  })
  
  
  output$artist_plot <- renderPlot({
    
    if(input$artist=="All") 
    {
      
      filtered_data() %>%
        count(Artist, sort=TRUE) %>%
        slice(1:10) %>%
        ggplot(aes(x=reorder(Artist,n), y=n)) +
        geom_col() +
        labs(title="Top Artists", x="Artist", y="Plays")
    } else 
    {
      filtered_data() %>%
        count(Song, sort=TRUE) %>%
        slice(1:10) %>%
        ggplot(aes(
            x=reorder(Song,n), y=n)) +
        geom_col() +
        coord_flip() +
        labs(title=paste("Top Songs by", input$artist), x="Song", y="Plays")
    }
  })
  
  output$recent_songs <- renderTable(
  {
    filtered_data() %>%
      mutate(Time = format(Time,"%I:%M %p"), Date = format(Date,"%m/%d/%Y"))
  })
}

valueBoxOutput("total")
valueBoxOutput("artists")
valueBoxOutput("songs")
valueBoxOutput("top_artist")

shinyApp(ui=ui, server=server)

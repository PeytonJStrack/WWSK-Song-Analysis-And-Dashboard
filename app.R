library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(showtext)
library(plotly)
library(DT)

font_add_google("Oswald", "oswald")
showtext_auto()

# Load data
github_url <- "https://raw.githubusercontent.com/PeytonJStrack/WWSK-Song-Analysis-And-Dashboard/main/data/Shark_Data.csv"

songs <- read_csv(github_url, show_col_types = FALSE) %>%
  mutate(
    Artist = str_remove(Artist, regex("\\s*(\\b(feat\\.?|ft\\.?|featuring)\\b.*|&.*)", ignore_case = TRUE)),
    Artist = str_trim(Artist),
    Date = as.Date(Date),
    Time = parse_date_time(Time, orders = c("I:M p", "H:M:S")),
    SortTime = as.POSIXct(paste(Date, format(Time, "%I:%M %p")), format = "%Y-%m-%d %I:%M %p", tz = "America/New_York"))

theme_shark <- function()
{
  theme_minimal(base_family = "oswald") +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "#0b0f14", color = NA),
      panel.background = element_rect(fill = "#111111", color = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#2d3748"),
      text = element_text(color = "white"),
      axis.text = element_text(color = "#e5e7eb", size = 12),
      axis.title = element_text(color = "#e5e7eb", size = 14),
      plot.title = element_text(color = "#ff4d4d", face = "bold", size = 18))
}

ui <- dashboardPage(
  dashboardHeader(
    title = tags$a(href = "https://www.943theshark.com/",target = "_blank", tags$img(src = "shark_logo.png", style = "height: 50px; margin-top: -6px;")), titleWidth = 150),
  dashboardSidebar(
    tags$style(HTML(
      "body 
      {
        background-color: #0b0f14;
        color: #e5e7eb;
      }
    
      .small-box 
      {
        background-color: #171b22 !important;
        border-left: 4px solid #d71920;
        border-top: 2px solid #d71920;
        border-radius: 8px;
        box-shadow: 0 3px 8px rgba(0,0,0,.35);
      }
      
      .small-box h3 
      {
        font-size: 34px;
        font-weight: 700;
        color: white;
      }
      
      .small-box p 
      {
        color: #cbd5e1;
        font-size: 14px;
      }
      
      .box 
      {
        border: none;
        border-radius: 8px;
        box-shadow: 0 3px 8px rgba(0,0,0,.35);
      }
      
      .box.box-primary 
      {
        border-top: none !important;
        background-color: #171b22;
      }
      
      .box.box-primary > .box-header 
      {
        background-color: #171b22 !important;
        color: white;
        border-bottom: 2px solid #d71920;
      }
      
      .box.box-solid.box-primary 
      {
        border: none !important;
        background-color: #171b22 !important;
      }
      
      .box-body 
      {
        background-color: #171b22;
        color: #e5e7eb;
      }

      .box.box-warning .box-body 
      {
        min-height: 160px;
        font-size: 24px;
      }
      
      .box.box-warning
      {
          border-top: 3px solid #d71920 !important;
          background-color: #171b22 !important;
          border-radius: 10px;
      }
      
      .box.box-warning > .box-header
      {
          background-color: #171b22 !important;
          color: white !important;
          border-bottom: 2px solid #d71920 !important;
      }
      
      .box.box-solid.box-warning
      {
          border: none !important;
          background-color: #171b22 !important;
      }
      
      .table 
      {
        color: #e5e7eb;
        background-color: #171b22;
        margin-left: auto;
        margin-right: auto;
      }
      
      .table th 
      {
        color: #d71920;
        border-bottom: 1px solid #2d3748;
      }
      
      .table td 
      {
        border-color: #2d3748;
      }
      
      table.dataTable
      {
          color: #e5e7eb !important;
          background-color: #171b22 !important;
      }
      
      table.dataTable tbody td
      {
          color: #e5e7eb !important;
          background-color: #171b22 !important;
      }
      
      table.dataTable thead th
      {
          color: #d71920 !important;
          background-color: #171b22 !important;
          border-bottom: 2px solid #d71920 !important;
      }
      
      .dataTables_info,
      .dataTables_paginate,
      .dataTables_paginate a
      {
          color: #e5e7eb !important;
      }
      
      .skin-blue .main-sidebar 
      {
        background-color: #111820;
      }
      
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li:hover > a 
      {
          border-left: 4px solid #d71920 !important;
      }
      
      .main-header 
      {
        position: fixed;
        width: 100%;
        z-index: 1000;
      }
      
      .skin-blue .main-header .navbar 
      {
        background-color: #420D09 !important;
        min-height: 70px;
      }
      
      .skin-blue .main-header .logo 
      {
        background-color: #420D09 !important;
        border-bottom: 2px solid #420D09 !important;
        width: 150px;
        height: 70px;
        line-height: 70px;
      }
      
      .sidebar-toggle 
      {
        color: #d71920;
        height: 70px;
        padding-top: 0px;
        margin-left: -12px;
      }
      
      .sidebar-toggle:before 
      {
        position: relative;
        top: 10px;
      }
      
      .main-header .sidebar-toggle:hover
      {
        background-color: #d71920 !important;
        color: white !important;
      }
      
      .main-sidebar 
      {
        position: fixed;
        height: 100%;
        overflow-y: auto;
        padding-top: 70px;
        width: 180px;
      }
      
      .content-wrapper,
      .right-side,
      .main-footer 
      {
        margin-left: 180px;
        margin-top: 65px;
        background-color: #0b0f14;
      }
      
      .hof-plaque
      {
          background: linear-gradient(180deg, #171b22 0%, #111111 100%);
          border: 1px solid rgba(215,25,32,.5);
          border-radius: 12px;
          box-shadow: 0 0 20px rgba(215, 25, 32, 0.15);
      }
      
      .box.box-primary.hof-plaque > .box-header
      {
          border-bottom: none !important;
      }
      
      .js-plotly-plot text
      {
          font-family: 'Oswald', sans-serif !important;
      }

      ::-webkit-scrollbar 
      {
        width: 8px;
      }
      
      ::-webkit-scrollbar-track 
      {
        background: #111820;
      }
      
      ::-webkit-scrollbar-thumb 
      {
        background: #333;
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb:hover 
      {
        background: #555;
      }")),
    
    sidebarMenu(id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Analytics", icon = icon("chart-bar"),
        menuSubItem("Top Artists", tabName = "artists"),
        menuSubItem("Song Repeats", tabName = "repeats"),
        menuSubItem("Weekly Trends", tabName = "weekly")),
      menuItem("Artist Explorer", tabName = "artist_explorer", icon = icon("microphone")),
      menuItem("Hall of Fame", tabName = "hall_of_fame", icon = icon("trophy")))), 
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Oswald:wght@400&display=swap")),
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(valueBoxOutput("total", width = 3), valueBoxOutput("artists", width = 3), valueBoxOutput("songs", width = 3), valueBoxOutput("top_artist", width = 3)),
        fluidRow(
          box(width = 4, title = paste("Recently Played Songs • Last Updated: ", format(max(songs$SortTime), "%B %d, %Y at %I:%M %p")), status = "primary", solidHeader = TRUE, tableOutput("recent_songs")),
          column(width = 8,
            box(width = 12, title = "Top Artists This Week", status = "primary", solidHeader = TRUE, plotlyOutput("dash_weekly_artist_plot", height = "294.75px")),
            box(width = 12, title = "Top Songs This Week", status = "primary", solidHeader = TRUE, plotlyOutput("dash_weekly_song_plot", height = "294.75px"))))),
      tabItem(tabName = "artist_explorer",
                fluidRow(
                  box(width = 12, title = "Artist Insights", status = "primary", solidHeader = TRUE,
                      selectInput("artist", "Select Artist:", choices = c("All", sort(unique(songs$Artist))), selected = "All"))),
                fluidRow(valueBoxOutput("artist_total_plays", width = 3), valueBoxOutput("artist_unique_songs", width = 3), valueBoxOutput("artist_top_song", width = 3), valueBoxOutput("artist_last_seen", width = 3)),
                fluidRow(
                  box(width = 8, title = textOutput("artist_plot_title"), status = "primary", solidHeader = TRUE, plotlyOutput("filtered_artist_plot", height = "671px")),
                  box(width = 4, title = textOutput("artist_recent_title"), status = "primary", solidHeader = TRUE, tableOutput("artist_recent_songs"))),
                fluidRow(
                  box(width = 12, title = "Artist Insights", status = "primary", solidHeader = TRUE, htmlOutput("artist_insights"))),
              fluidRow(
                box(width = 12, title = "Complete Song Catalog", status = "primary", solidHeader = TRUE, DT::dataTableOutput("artist_catalog")))),
      tabItem(tabName = "artists",
        fluidRow(valueBoxOutput("artist_rank1", width = 4), valueBoxOutput("artist_rank1_plays", width = 4), valueBoxOutput("artist_rank1_songs", width = 4)),
        fluidRow(
          box(width = 12, title = "Top Artists", status = "primary", solidHeader = TRUE, plotlyOutput("artist_plot", height = "690px")))),
      tabItem(tabName = "repeats",
        fluidRow(valueBoxOutput("top_song_name", width = 4), valueBoxOutput("top_song_plays", width = 4), valueBoxOutput("top_song_artist", width = 4)),
        fluidRow(
          box(width = 12, title = "Most Replayed Songs", status = "primary", solidHeader = TRUE,plotlyOutput("repeat_plot", height = "690px")))),
      tabItem(tabName = "weekly",
        fluidRow(valueBoxOutput("weekly_top_artist", width = 4), valueBoxOutput("weekly_artist_plays", width = 4), valueBoxOutput("weekly_top_song", width = 4)),
        fluidRow(
          box(width = 6, title = "Top Artists This Week", status = "primary", solidHeader = TRUE, plotlyOutput("weekly_artist_plot", height = "690px")),
          box(width = 6, title = "Top Songs This Week", status = "primary", solidHeader = TRUE, plotlyOutput("weekly_song_plot", height = "690px")))),    
      tabItem(tabName = "hall_of_fame",
        fluidRow(
          box(width = 4,title = "Most Played Artists", status = "warning", solidHeader = TRUE, htmlOutput("hof_artists")),
          box(width = 4, title = "Most Played Songs", status = "warning", solidHeader = TRUE, htmlOutput("hof_songs")),
          box(width = 4, title = "Largest Catalogs", status = "warning", solidHeader = TRUE, htmlOutput("hof_catalogs"))),
        fluidRow(
          box(width = 4, title = "Fastest Rising Artists", status = "warning", solidHeader = TRUE, htmlOutput("hof_rising")),
          box(width = 4, title = "Top Played One-Hit Wonders", status = "warning", solidHeader = TRUE, htmlOutput("hof_one_hit")),
          box(width = 4, title = "Average Plays Per Song", status = "warning", solidHeader = TRUE, htmlOutput("hof_efficiency"))),
        fluidRow(
          column(width = 8, offset = 2,
            box(width = 12, status = "primary", solidHeader = TRUE, title = "94.3 THE SHARK HALL OF FAME", class = "hof-plaque",
              tags$div(
                style = "
                  text-align:center;
                  padding:25px;
                  line-height:1.8;",
                tags$h2(
                  style = "
                    color:#d71920;
                    letter-spacing:3px;
                    margin-bottom:20px;",
                  "94.3 THE SHARK"),
                tags$h3(
                  style = "
                    color:white;
                    margin-bottom:25px;",
                  "HALL OF FAME"),
                tags$p(
                  style = "
                    color:#cbd5e1;
                    font-size:18px;",
                  "Recognizing the most dominant artists, songs, and achievements in station airplay history."),
                tags$hr(),
                tags$p(
                  style = "
                    color:#9ca3af;
                    font-size:16px;",
                  paste(
                    "Tracking",
                    nrow(songs),
                    "plays across",
                    n_distinct(songs$Artist),
                    "artists and",
                    n_distinct(songs$Song),
                    "songs."))))))))),
      tags$div(style = "
        text-align:center;
        color:#6b7280;
        padding:15px;
        font-size:12px;
        letter-spacing:1px;",
      HTML(
        paste0(
          "94.3 THE SHARK ANALYTICS DASHBOARD",
          "<br>",
          "LAST UPDATED ",
          format(max(songs$SortTime), "%B %d, %Y %I:%M %p")))))

server <- function(input, output, session)
{
  current_time <- reactive(
  {
      now(tzone = "America/New_York")
  })
  
  filtered_data <- reactive(
  {
    songs %>%
      filter(SortTime <= current_time())
  })
  
  artist_data <- reactive(
  {
    if(input$artist == "All")
    {
      return(tibble())
    }
    songs %>%
      filter(SortTime <= current_time(), Artist == input$artist)
  })
  
  weekly_data <- reactive(
  {
    filtered_data() %>%
      filter(SortTime >= current_time() - days(7))
  })
  
  podium_html <- function(df, name_col, value_col, suffix = "")
  {
    if(nrow(df) == 0)
    {
      return("<div>No data available.</div>")
    }
    
    while(nrow(df) < 3)
    {
      df <- bind_rows(df,tibble(!!name_col := "-", !!value_col := 0))
    }
    paste0(
      "<div style='font-size:18px; line-height:2;'>",
      "🥇 <b>", df[[name_col]][1], "</b> — ", round(df[[value_col]][1], 1), suffix, "<br>",
      "🥈 <b>", df[[name_col]][2], "</b> — ", round(df[[value_col]][2], 1), suffix, "<br>",
      "🥉 <b>", df[[name_col]][3], "</b> — ", round(df[[value_col]][3], 1), suffix,
      "</div>")
  }
  
  output$total <- renderValueBox(
  {
    valueBox(nrow(filtered_data()), "Total Songs")
  })
  
  output$artists <- renderValueBox(
  {
    valueBox(n_distinct(filtered_data()$Artist), "Unique Artists")
  })
  
  output$songs <- renderValueBox(
  {
    valueBox(n_distinct(filtered_data()$Song), "Unique Songs")
  })
  
  output$top_artist <- renderValueBox(
  {
    top_artist <- filtered_data() %>%
      count(Artist, sort = TRUE)
    if(nrow(top_artist) == 0)
    {
      return(valueBox("0", "Top: None"))
    }
    valueBox(value = top_artist$n[[1]], subtitle = paste("Top:", str_trunc(top_artist$Artist[[1]], width = 15)))
  })
  
  output$artist_total_plays <- renderValueBox(
  {
    if(input$artist == "All")
    {
      return(valueBox("-", "Total Plays"))
    }
    valueBox(nrow(artist_data()), "Total Plays")
  })
  
  output$artist_unique_songs <- renderValueBox(
  {
    if(input$artist == "All")
    {
      return(valueBox("-", "Unique Songs"))
    }
    valueBox(n_distinct(artist_data()$Song), "Unique Songs")
  })
  
  output$artist_top_song <- renderValueBox(
  {
    if(input$artist == "All")
    {
      return(valueBox("-", "Top Song Plays"))
    }
    top_song <- artist_data() %>%
      count(Song, Artist, sort = TRUE) %>%
      slice(1)
    
    if(nrow(top_song) == 0)
    {
      return(valueBox("-", "Top Song Plays"))
    }
    
    valueBox(top_song$n[[1]], "Top Song Plays")
  })
  
  output$artist_last_seen <- renderValueBox(
  {
    if(input$artist == "All")
    {
      return(valueBox("-", "Last Played"))
    }
    if(nrow(artist_data()) == 0)
    {
      return(valueBox("-", "Last Played"))
    }
    valueBox(format(max(artist_data()$SortTime), "%b %d"), "Last Played")
  })
  
  output$artist_recent_songs <- renderTable(
  {
    if(input$artist == "All")
    {
      return(NULL)
    }
    artist_data() %>%
      arrange(desc(SortTime)) %>%
      mutate(Time = format(Time, "%I:%M %p"), Date = format(Date, "%m/%d/%Y")) %>%
      select(Artist, Song, Time, Date) %>%
      head(20)
  })
  
  output$artist_insights <- renderUI(
  {
    if(input$artist == "All")
    {
      return(h4("Select an artist to view insights."))
    }
    top_song <- artist_data() %>%
      count(Song, Artist, sort = TRUE) %>%
      slice(1)
    if(nrow(top_song) == 0)
    {
      top_song <- tibble(Song = "None", n = 0)
    }
    artist_rank <- filtered_data() %>%
      count(Artist, sort = TRUE) %>%
      mutate(Rank = row_number()) %>%
      filter(Artist == input$artist) %>%
      pull(Rank)
    if(length(artist_rank) == 0)
    {
      artist_rank <- "-"
    }
    artist_dates <- artist_data() %>%
      distinct(Date) %>%
      arrange(Date)
    if(nrow(artist_dates) == 0)
    {
      longest_streak <- 0
    } else
    {
      artist_dates <- artist_dates %>%
        mutate(gap = as.numeric(Date - lag(Date, default = first(Date))), streak_group = cumsum(gap > 1)) 
      longest_streak <- artist_dates %>%
        count(streak_group) %>%
        summarise(max_streak = max(n)) %>%
        pull(max_streak)
    }
    current_streak <- 0
    if(nrow(artist_dates) > 0)
    {
      streak_dates <- artist_dates$Date
      latest_date <- max(streak_dates)
      if(latest_date >= Sys.Date() - 1)
      {
        current_streak <- 1
        if(length(streak_dates) > 1)
        {
          for(i in length(streak_dates):2)
          {
            if(as.numeric(streak_dates[i] - streak_dates[i - 1]) == 1)
            {
              current_streak <- current_streak + 1
            }
            else
            {
              break
            }
          }
        }
      }
    }
    this_week_plays <- filtered_data() %>%
      filter(Artist == input$artist, SortTime >= current_time() - days(7)) %>%
      nrow()
      
    previous_week_plays <- filtered_data() %>%
      filter(Artist == input$artist, SortTime >= current_time() - days(14), SortTime < current_time() - days(7)) %>%
      nrow()
      
    if(previous_week_plays == 0)
    {
      trend_text <- "New This Week"
    }
    else
    {
      pct_change <- round(((this_week_plays - previous_week_plays) / previous_week_plays) * 100, 1)
      if(pct_change > 0)
      {
        trend_text <- paste0("↑ ", pct_change, "% (", this_week_plays, " vs ", previous_week_plays, " plays)")
      }
      else if(pct_change < 0)
      {
        trend_text <- paste0("↓ ", pct_change, "% (", this_week_plays, " vs ", previous_week_plays, " plays)")
      }
      else
      {
        trend_text <- "No Change"
      }
    }
    HTML(
      paste0(
        "<h3 style='margin-bottom: 20px;'>", input$artist, "</h3>",
        "<div style='font-size: 18px;'>",
        "<b style='color: #d71920;'>PERFORMANCE</b>",
        "<hr style='margin-top: 5px;'>",
        "<p><b>Overall Rank:</b> #", artist_rank, "<br>",
        "<b>Total Plays: </b> ", nrow(artist_data()), "<br>",
        "<b>Unique Songs: </b> ", n_distinct(artist_data()$Song), "</p>",
        "<b style='color: #d71920;'>STREAKS</b>",
        "<hr style='margin-top: 5px;'>",
        "<p><b>Current Daily Streak: </b> ", current_streak, " days<br>",
        "<b>Longest Daily Streak: </b> ", longest_streak, " days</p>",
        "<b style='color: #d71920;'>TRENDING</b>",
        "<hr style='margin-top: 5px;'>",
        "<p><b>Weekly Trend: </b> ", trend_text, "</p>",
        "<b style='color: #d71920;'>SONG DATA</b>",
        "<hr style='margin-top: 5px;'>",
        "<p><b>Most Played Song:</b> ", top_song$Song, "<br>",
        "<b>Times Played: </b> ", top_song$n, "</p>",
        "<b style='color: #d71920;'>HISTORY</b>",
        "<hr style='margin-top: 5px;'>",
        "<p><b>First Dashboard Appearance: </b> ",
        format(min(artist_data()$SortTime), "%b %d, %Y"), "<br>",
        "<b>Latest Dashboard Appearance: </b> ",
        format(max(artist_data()$SortTime), "%b %d, %Y"), "</p>","</div>"))
  })
  
  output$artist_plot <- renderPlotly(
  {
    filtered_data() %>%
      count(Artist, sort = TRUE) %>%
      slice(1:10) %>%
      ggplot(aes(x = reorder(Artist, n), y = n, fill = n, text = paste("Artist:", Artist, "<br>Plays:", n))) +
        geom_col() +
        scale_fill_gradientn(colors = c("#ff4d4d", "#d71920", "#A61E1E")) +
        coord_flip() +
        labs(x = NULL, y = NULL) +
        theme_shark() -> p
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Oswald", size = 12, color = "#e5e7eb"))
  })
  
  output$artist_rank1 <- renderValueBox(
  {
    leader <- filtered_data() %>%
      count(Artist, sort = TRUE) %>%
      slice(1)
    if(nrow(leader) == 0)
    {
      return(valueBox("-", "#1 Artist"))
    }
    valueBox(value = leader$Artist, subtitle = "#1 Artist")
  })
  
  output$artist_rank1_plays <- renderValueBox(
  {
    leader <- filtered_data() %>%
      count(Artist, sort = TRUE) %>%
      slice(1)
    valueBox(value = leader$n,subtitle = "Total Plays")
  })
  
  output$artist_rank1_songs <- renderValueBox(
  {
    leader_artist <- filtered_data() %>%
      count(Artist, sort = TRUE) %>%
      slice(1) %>%
      pull(Artist)
    unique_songs <- filtered_data() %>%
      filter(Artist == leader_artist) %>%
      summarise(n = n_distinct(Song)) %>%
      pull(n)
    valueBox(value = unique_songs, subtitle = "Unique Songs"
    )
  })
  
  output$top_song_name <- renderValueBox(
  {
    top_song <- filtered_data() %>%
      count(Song, Artist, sort = TRUE) %>%
      slice(1)
    if(nrow(top_song) == 0)
    {
      return(valueBox("-", "#1 Song"))
    }
    valueBox(value = str_trunc(top_song$Song, 20), subtitle = "#1 Song")
  })
  
  output$top_song_plays <- renderValueBox(
  {
    top_song <- filtered_data() %>%
      count(Song, Artist, sort = TRUE) %>%
      slice(1)
    if(nrow(top_song) == 0)
    {
      return(valueBox("-", "Total Plays"))
    }
    valueBox(value = top_song$n, subtitle = "Total Plays")
  })
  
  output$top_song_artist <- renderValueBox(
  {
    song_name <- filtered_data() %>%
      count(Song, Artist, sort = TRUE) %>%
      slice(1) %>%
      pull(Song)
    if(length(song_name) == 0)
    {
      return(valueBox("-", "Artist"))
    }
    artist_name <- filtered_data() %>%
      filter(Song == song_name) %>%
      count(Artist, sort = TRUE) %>%
      slice(1) %>%
      pull(Artist)
    valueBox(value = str_trunc(artist_name, 20), subtitle = "Artist")
  })
  
  output$weekly_top_artist <- renderValueBox(
  {
    leader <- weekly_data() %>%
      count(Artist, sort = TRUE) %>%
      slice(1)
    if(nrow(leader) == 0)
    {
      return(valueBox("-", "#1 Artist This Week"))
    }
    valueBox(value = str_trunc(leader$Artist, 20), subtitle = "#1 Artist This Week")
  })
  
  output$weekly_artist_plays <- renderValueBox(
  {
    leader <- weekly_data() %>%
      count(Artist, sort = TRUE) %>%
      slice(1)
    valueBox(value = leader$n, subtitle = "Weekly Plays")
  })
  
  output$weekly_top_song <- renderValueBox(
  {
    leader <- weekly_data() %>%
      count(Song, Artist, sort = TRUE) %>%
      slice(1)
    if(nrow(leader) == 0)
    {
      return(valueBox("-", "#1 Song This Week"))
    }
    valueBox(value = str_trunc(leader$Song, 20), subtitle = "#1 Song This Week")
  })
  
  output$artist_plot_title <- renderText(
  {
    if (input$artist == "All") 
    {
      "Top Songs By Artist"
    } else 
    {
      paste("Top Songs By", input$artist)
    }
  })
  
  output$artist_recent_title <- renderText(
  {
    if (input$artist == "All") 
    {
      "Recently Played Songs"
    } else 
    {
      paste("Recently Played Songs By", input$artist)
    }
  })
  
  output$repeat_plot <- renderPlotly(
  {
    filtered_data() %>%
      count(Song, Artist, sort = TRUE) %>%
      slice(1:10) %>%
      ggplot(aes(x = reorder(Song, n), y = n, fill = n, text = paste("Song:", Song, "<br>Artist:", Artist, "<br>Plays:", n))) +
        geom_col() +
        scale_fill_gradientn(colors = c("#ff4d4d", "#d71920", "#A61E1E")) +
        coord_flip() +
        labs(x = NULL, y = NULL) +
        theme_shark() -> p
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Oswald", size = 12, color = "#e5e7eb"))
  })
  
  output$recent_songs <- renderTable(
  {
    filtered_data() %>%
      arrange(desc(SortTime)) %>%
      mutate(Time = format(Time, "%I:%M %p"), Date = format(Date, "%m/%d/%Y")) %>%
      select(Artist, Song, Time, Date) %>%
      head(20)
  })
  
  output$filtered_artist_plot <- renderPlotly(
  {
    if(input$artist == "All")
    {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Please Select An Artist", size = 8) +
        theme_void()
    } else
    {
      artist_data() %>%
        count(Song, Artist, sort = TRUE) %>%
        slice(1:10) %>%
        ggplot(aes(x = reorder(Song, n), y = n, fill = n, text = paste("Song:", Song, "<br>Artist:", Artist, "<br>Times Played:", n))) +
          geom_col() +
          scale_fill_gradientn(colors = c("#ff4d4d", "#d71920", "#A61E1E")) +
          coord_flip() + 
          labs(x = NULL, y = NULL) +
          theme_shark() -> p
      ggplotly(p, tooltip = "text") %>%
        layout(font = list(family = "Oswald", size = 12, color = "#e5e7eb"))
    }
  })
  
  output$artist_catalog <- DT::renderDataTable(
  {
    if(input$artist == "All")
    {
      return(NULL)
    }
    artist_data() %>%
      count(Song, sort = TRUE, name = "Plays") %>%
      arrange(desc(Plays), Song)
  }, options = list(pageLength = 10, lengthChange = FALSE, searching = FALSE, ordering = TRUE, dom = "tip"), rownames = FALSE)
  
  output$weekly_artist_plot <- renderPlotly(
  {
    weekly_data() %>%
      count(Artist, sort = TRUE) %>%
      slice(1:10) %>%
      ggplot(aes(x = reorder(Artist, n), y = n, fill = n, text = paste("Artist:", Artist, "<br>Plays:", n))) +
        geom_col() +
        scale_fill_gradientn(colors = c("#ff4d4d", "#d71920", "#A61E1E")) +
        coord_flip() +
        labs(x = NULL, y = NULL) +
        theme_shark() -> p
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Oswald", size = 12, color = "#e5e7eb"))
  })
  
  output$weekly_song_plot <- renderPlotly(
  {
      weekly_data() %>%
      count(Song, Artist, sort = TRUE) %>%
      slice(1:10) %>%
      ggplot(aes(x = reorder(Song, n), y = n, fill = n, text = paste("Song:", Song, "<br>Artist:", Artist, "<br>Plays:", n))) +
        geom_col() +
        scale_fill_gradientn(colors = c("#ff4d4d", "#d71920", "#A61E1E")) +
        coord_flip() +
        labs(x = NULL, y = NULL) +
        theme_shark() -> p
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Oswald", size = 12, color = "#e5e7eb"))
  })
  
  output$dash_weekly_artist_plot <- renderPlotly(
  {
    weekly_data() %>%
      count(Artist, sort = TRUE) %>%
      slice(1:10) %>%
      ggplot(aes(x = reorder(Artist, n), y = n, fill = n, text = paste("Artist:", Artist, "<br>Plays:", n))) +
        geom_col() +
        scale_fill_gradientn(colors = c("#ff4d4d", "#d71920", "#A61E1E")) +
        coord_flip() +
        labs(x = NULL, y = NULL) +
        theme_shark() -> p
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Oswald", size = 12, color = "#e5e7eb"))
  })
  
  output$dash_weekly_song_plot <- renderPlotly(
  {
      weekly_data() %>%
        count(Song, Artist, sort = TRUE) %>%
        slice(1:10) %>%
        ggplot(aes(x = reorder(Song, n), y = n, fill = n, text = paste("Song:", Song, "<br>Artist:", Artist, "<br>Plays:", n))) +
          geom_col() +
          scale_fill_gradientn(colors = c("#ff4d4d", "#d71920", "#A61E1E")) +
          coord_flip() +
          labs(x = NULL, y = NULL) +
          theme_shark() -> p
    ggplotly(p, tooltip = "text") %>%
      layout(font = list(family = "Oswald", size = 12, color = "#e5e7eb"))
  })
  
  output$hof_artists <- renderUI(
  {
    leaders <- filtered_data() %>%
      count(Artist, sort = TRUE) %>%
      slice(1:3)
    HTML(podium_html(leaders, "Artist", "n"))
  })
  
  output$hof_songs <- renderUI(
  {
    leaders <- filtered_data() %>%
      count(Song, Artist, sort = TRUE) %>%
      slice(1:3)
    HTML(podium_html(leaders, "Song", "n"))
  })
  
  output$hof_catalogs <- renderUI(
  {
    leaders <- filtered_data() %>%
      group_by(Artist) %>%
      summarise(Catalog = n_distinct(Song)) %>%
      arrange(desc(Catalog)) %>%
      slice(1:3)
    
    HTML(podium_html(leaders, "Artist", "Catalog"))
  })
  
  output$hof_rising <- renderUI(
  {
    current_week <- filtered_data() %>%
      filter(SortTime >= current_time() - days(7)) %>%
      count(Artist, name = "Current")
    previous_week <- filtered_data() %>%
      filter(SortTime >= current_time() - days(14), SortTime < current_time() - days(7)) %>%
      count(Artist, name = "Previous")
    leaders <- current_week %>%
      inner_join(previous_week, by = "Artist") %>%
      mutate(Growth = if_else(Previous > 0, ((Current - Previous) / Previous) * 100, NA_real_)) %>%
      filter(!is.na(Growth)) %>%
      arrange(desc(Growth)) %>%
      slice(1:3)
    HTML(podium_html(leaders, "Artist", "Growth", "%"))
  })
  
  output$hof_one_hit <- renderUI(
  {
    leaders <- filtered_data() %>%
      group_by(Artist) %>%
      summarise(Songs = n_distinct(Song), Plays = n()) %>%
      filter(Songs == 1) %>%
      arrange(desc(Plays)) %>%
      slice(1:3)
    HTML(podium_html(leaders, "Artist", "Plays"))
  })
  
  output$hof_efficiency <- renderUI(
  {
    leaders <- filtered_data() %>%
      group_by(Artist) %>%
      summarise(Plays = n(), Songs = n_distinct(Song), Avg = Plays / Songs) %>%
      filter(Songs >= 5) %>%
      arrange(desc(Avg)) %>%
      slice(1:3)
    HTML(podium_html(leaders, "Artist", "Avg"))
    
  })
}

shinyApp(ui = ui, server = server)
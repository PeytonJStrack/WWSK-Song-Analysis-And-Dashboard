library(tidyverse)
library(lubridate)
library(jsonlite)

url <- "https://us.production.audio.one/api/v1.3/app/20008/playlist/6/live-stream-history/?page_size=100"

#Access the JSON Data
radio_data <- fromJSON(url)

#Filter the Data and Alter the Format
radio_data$results %>%
  select(Artist = current_artist_name, Song = current_title, Time = streamed_time) %>%
  mutate(TempTime = ymd_hms(Time, tz = "UTC"), TempTime = with_tz(Time, tzone = "America/New_York")) %>%
  filter(TempTime <= now(tzone = "America/New_York")) %>%
  mutate(Date = as.Date(TempTime), Time = format(TempTime, "%I:%M %p")) %>%
  select(Artist, Song, Time, Date) -> songs

#Ensure the Shark Data exists in the Data file
if(file.exists("data/Shark_Data.csv")){
  
  read_csv("data/Shark_Data.csv", show_col_types = FALSE) %>%
    mutate(Time = as.character(Time), Date = as.Date(Date)) -> old_songs
  
} else 
{
  old_songs <- tibble()
}

# Combine old and new data and remove exact duplicates
bind_rows(old_songs, songs) %>%
  distinct(Song, Artist, Time, Date, .keep_all = TRUE) %>%
  arrange(Date, parse_date_time(Time,"%I:%M %p")) -> updated_songs

# Save updated data
write_csv(updated_songs, "data/Shark_Data.csv")


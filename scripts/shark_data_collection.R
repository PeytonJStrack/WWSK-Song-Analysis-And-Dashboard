library(tidyverse)
library(lubridate)
library(jsonlite)

url <- "https://us.production.audio.one/api/v1.3/app/20008/playlist/6/live-stream-history/?page_size=100"

plr_url <- "https://us.production.audio.one/api/v1.3/app/20009/playlist/12/live-stream-history/?page_size=100"

#Access the JSON Data
shark_data <- fromJSON(url)

plr_data <- fromJSON(plr_url)

artist_fixes <- c("Blink 182" = "Blink-182", "Guns 'n Roses" = "Guns N' Roses", "Ac/dc" = "AC/DC")

#Filter the Data and Alter the Format
shark_data$results %>%
  select(Artist = current_artist_name, Song = current_title, Time = streamed_time) %>%
  mutate(Artist = recode(Artist, !!!artist_fixes), Time = ymd_hms(Time, tz = "UTC"), Time = with_tz(Time, tzone = "America/New_York"), Date = as.Date(format(Time, tz = "America/New_York", "%Y-%m-%d"))) %>%
  filter(!(hour(Time) >= 6 & hour(Time) < 9)) -> shark_songs

plr_data$results %>%
  select(Artist = current_artist_name, Song = current_title, Time = streamed_time) %>%
  mutate(Artist = recode(Artist, !!!artist_fixes), Time = ymd_hms(Time, tz = "UTC"), Time = with_tz(Time, tzone = "America/New_York"), Date = as.Date(format(Time, tz = "America/New_York", "%Y-%m-%d"))) %>%
  filter(hour(Time) >= 6 & hour(Time) < 9) -> plr_songs

bind_rows(shark_songs, plr_songs) %>%
  mutate(Time = format(Time,"%I:%M %p")) -> songs

#Ensure the Shark Data exists in the Data file
if(file.exists("data/Shark_Data.csv"))
{
  read_csv("data/Shark_Data.csv", show_col_types = FALSE) %>%
  mutate(Time = format(parse_date_time(Time, orders = c("I:M p", "H:M:S")), "%I:%M %p"), Date = as.Date(Date)) -> old_songs
} else 
{
  old_songs <- tibble()
}

# Combine old and new data and remove exact duplicates
bind_rows(old_songs, songs) %>%
  distinct(Song, Artist, Time, Date, .keep_all = TRUE) %>%
  arrange(Date, parse_date_time(Time, orders = "I:M p")) -> updated_songs

# Save updated data
write_csv(updated_songs, "data/Shark_Data.csv")


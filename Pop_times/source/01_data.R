# load data
df_raw <- read.csv("./data/pop_times.csv")

df_wide <- read.csv("./data/pop_times_pivot.csv") %>%
  mutate(location = as.factor(location))

# prepare data

df_raw <- df_raw %>%
  mutate(time_spendt = if_else(is.na(time_spendt), "", time_spendt)) %>%
  mutate(Zeit = lubridate::as_datetime(Zeit)) %>%
  mutate(lat = lat + 0.0021)




df_long <- df_raw %>%
  select(location, day_of_week , hour, 
         popularity, current_pop,time_spendt) %>%
  mutate(location = as.factor(location),
         day_of_week  = as.factor(day_of_week))

locations <- unique(df_long$location)
Uhrzeiten <- unique(df_long$hour)
Tage <- unique(df_long$day_of_week)

heute <- unique(df_raw$Zeit)
Tag <- lubridate::wday(heute, label = TRUE, abbr = FALSE)
Uhrzeit <- lubridate::hour(heute)
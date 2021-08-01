
map_current_popularity <- function(dt=dt, Uhrzeit = Uhrzeit, Tag = Tag){
  dt %>%
    filter(hour == Uhrzeit & day_of_week == Tag) %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(lat = ~lon, lng = ~lat,
                     radius = ~(current_pop/10),
                     opacity = ~current_pop,
                     popup = ~paste(location, "current popularity is:", current_pop),
    ) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479") %>%
    return()
}

map_popularity <- function(dt, Uhrzeit, Tag){

  m <- leaflet() %>%
    addTiles() 
  
  for(x in locations){
    # get coords
    longitude <- dt %>%
      filter(location  == x & day_of_week == Tag) %>%
      pull(lat) %>%
      unique()
    latitude <- dt %>%
      filter(location  == x & day_of_week == Tag) %>%
      pull(lon) %>%
      unique()
    
    # get popularity
    popularity <- dt %>%
      filter(location  == x) %>%
      filter(hour == Uhrzeit & day_of_week == Tag) %>%
      pull(popularity)
    
    # extract content for markers
    content <- dt %>% 
      filter(location  == x) %>%
      filter(hour == Uhrzeit & day_of_week == Tag) %>%
      mutate(pop = paste("popularity is", popularity, "on", 
                         day_of_week, "at", hour)) %>%
      select(address, time_spendt, pop) %>%
      paste0(collapse = ",")
    
    # add markers to map
    m <- m %>%
      addMarkers(lng = longitude,
                 lat = latitude,
                 label = paste(x, content),
                 group = Tag
      )
  }
  m
}

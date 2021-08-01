#
#
#

library(dplyr)
library(ggplot2)
library(leaflet)
library(shiny)
library(shinydashboard)
library(DT)

# load and prepare data
source("./source/01_data.R")
# load functions
source("./source/02_helper.R")

# define UI
header <- dashboardHeader(title = "Pankow's popular places")

sidebar <- dashboardSidebar(
    sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("Map current popularity", icon = icon("binoculars"),
                 tabName = "current_map"),
        
        menuItem("Maps for popular places", icon = icon("binoculars"), 
                 tabName = "pop_places_map"),
        
        menuItem("Charts", icon = icon("chart-bar"), tabName = "charts"),
        
        menuItem("Long Table popular times", icon = icon("border-all"),
                 tabName = "pop_times_long"),
        
        menuItem("Wide Table popular times", icon = icon("border-all"), 
                 tabName = "pop_times_wide"),
        
        menuItem("top 5 times per place", icon = icon("border-all"), 
                 tabName = "top5_times")
        
    )
)

body <- dashboardBody(
    tabItems(
        tabItem("current_map",
        div(p("Zeigt die derzeitige Popularität der Plätze, wobei der Radius
                eines Kreises entsprechend der Popularität des Ortes ist, d.h.
                ein großer Kreis entspricht einer hohen Popularität.")),
                div(p("Map Feature: Mit dem Maßband (icon unten links) können
                die Wegstrecke abgeschätzt werden.")),
                div(p("N.B.: Im Moment greift das Dashboard auf einen
                gespeicherten Datensatz zurück.In einer Live-Version würde
                      es Ist-Daten abfragen.")),
                leafletOutput("current_popularity_map")
                
        ),
        
        tabItem("pop_places_map",
                div(p("Bitte bewegen Sie die maus über einen Ort der Sie interessiert.")),
                div(p("Wir zeigen für eine gewählte Uhrzeit und einen gewählten
                      Tag die Plätze auf der Karte")),
                inputPanel(
                    selectInput(inputId = "Uhrzeit", 
                                label = "Uhrzeit:",
                                selected = lubridate::hour(lubridate::now()),
                                choices = Uhrzeiten),
                    selectInput(inputId = "Tag", 
                                label = "Tag:",
                                selected = lubridate::wday(lubridate::now(),
                                                           label = TRUE,
                                                           abbr = FALSE),
                                choices = Tage)
                    ),
                mainPanel(leafletOutput(outputId = "pop_map"))
                ),
        
        tabItem("charts",
                div(p("Die untere Graphik zeigt wie sich die Popularität am 
                      gewählten Ort über die Wochentage und Uhrzeiten verteilt.")),
                inputPanel(
                    selectInput(inputId = "location", 
                                label = "Location:", 
                                choices = locations)
                    ),
                mainPanel(plotOutput(outputId = "locationPlot"))
                ),
        
        tabItem("pop_times_long",
                div(p("Aufgabe: Ihr habt am Montag und Dienstag von 10-15 Uhr
                Zeit und wollt den besten Platz finden.")),
                div(p("*Lösung:* Wähle bei `day_of_week` Montag und Dienstag 
                      aus und stelle bei `hour` den Slider auf 10-14. 
                      Anschließend sortiere `popularity` absteigend durch 
                      klicken auf den Pfeil hinter `popularity`.")),
                DTOutput("table_long")
                ),
        
        
        
        tabItem("pop_times_wide",
                div(p("Anwendungsbeispiel: Durch filtern von `hour` und `location` 
                lassen sich Tage identifizieren an denen viel los ist.")),
                div(p("Zur Bedienung:")),
                div(p("spalten können gefiltert
                      werden, indem man auf die Box unter dem Spaltennamen 
                      klickt.")),
                div(p("Die Tabelle kann bzgl. einer Spalte sortiert werden, 
                      indem man auf einen Pfeil hinter dem Spaltennamen klickt.")),
                DTOutput("table_wide")
                ),
        
        tabItem("top5_times",
                "Für jede `Location` werden die 5 Datenpunkte mit der 
                höchsten `popularity` ausgegeben.Auf diese Weise kann 
                man Bspw. schnell sehen, wann sich welcher Ort am 
                meisten lohnt.",
                DTOutput("top5")
        )
        )
    )
    


# Define server logic for dashboard
server <- function(input, output) {

    output$current_popularity_map <- renderLeaflet(map_current_popularity(df_raw, Uhrzeit, Tag))

    output$pop_map <- renderLeaflet({
        Uhrzeit <- input$Uhrzeit
        Tag <- input$Tag
        
        m <- leaflet() %>%
            addTiles() 
        
        for(x in locations){
            # get coords
            longitude <- df_raw %>%
                filter(location  == x & day_of_week == Tag) %>%
                pull(lat) %>%
                unique()
            latitude <- df_raw %>%
                filter(location  == x & day_of_week == Tag) %>%
                pull(lon) %>%
                unique()
            
            # get popularity
            popularity <- df_raw %>%
                filter(location  == x) %>%
                filter(hour == Uhrzeit & day_of_week == Tag) %>%
                pull(popularity)
            
            # extract content for markers
            content <- df_raw %>% 
                filter(location  == x) %>%
                filter(hour == Uhrzeit & day_of_week == Tag) %>%
                mutate(pop = paste("popularity is", popularity, "on", day_of_week, "at", hour)) %>%
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
    })
    
    output$locationPlot <- renderPlot({
        x <- input$location

        p <- df_long %>%
            filter(location == x) %>%
            ggplot(aes(x = hour, y = popularity,
                       colour = day_of_week)) +
            geom_point() +
            labs(title = paste("Popularität in der Woche am Ort:", x))

        p
    })
    
    output$table_long <- renderDT({
        df_long %>%
            DT::datatable(
                filter = 'top',
                rownames = FALSE)
    })
    
    output$table_wide <- renderDT({
        df_wide %>%
            DT::datatable(
                filter = 'top',
                rownames = FALSE)
    })
    
    output$top5 <- renderDT({
        df_top_5 <- df_long %>%
            select(-current_pop) %>%
            arrange(desc(popularity)) %>%
            group_by(location) %>%
            slice(1:5) %>%
            DT::datatable(filter = 'top',
                          rownames = FALSE)
    })
        }

shinyApp(
    ui = dashboardPage(header, sidebar, body),
    server = server
)
#
# shiny app for popular times for politcs
#

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(
        title = 'DataTable Options',
        tabPanel('Display popular times long',  
                 DT::dataTableOutput('ex1')),
        
        tabPanel('Display popular times wide',
                 DT::dataTableOutput('ex2')),
        
        tabPanel('Display top 5 popular times per location',    
                 DT::dataTableOutput('ex3')),
        
        tabPanel('Plots popular',
                 sidebarPanel(
                         # Input: dropdown for location
                     selectInput("select_location", 
                                 label = h3("Select location"), 
                                 choices = list("Brennerberg",
                                                "Kissingen-Stadion",
                                                "Diseño",
                                                "U Vinetastr.",
                                                "S+U Pankow",
                                                "Wochenmarkt Pankow",
                                                "REWE",
                                                "Neumann Forum",
                                                "S Pankow-Heinersdorf",
                                                "PENNY"), 
                                 selected = "Brennerberg")
                         
                     ),
                     
                     mainPanel(
                         textInput("txt", "Page Under Construction"),
                         textOutput("text"),
                         plotOutput("plots")
                     )
                 )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # load data
    df_long <- read.csv("./data/pop_times.csv") %>%
        select(location, day_of_week , hour, 
               popularity, current_pop,time_spendt) %>%
        mutate(location = as.factor(location),
               day_of_week  = as.factor(day_of_week))
    
    df_wide <- read.csv("./data/pop_times_pivot.csv") %>%
        mutate(location = as.factor(location))
    
    # prepare data
    
    locations <- unique(df_long$location)
    
    df_top_5 <- df_long %>%
        arrange(desc(popularity)) %>%
        group_by(location) %>%
        slice(1:5) 
    
    # define tabs and tables
    output$ex1 <- DT::renderDataTable(
        DT::datatable(df_long, 
                      filter = 'top',
                      rownames = FALSE,
                      options = list(
                          list(pageLength = 25),
                          rowCallback = JS(
                              'function(row, data) {',
                              '$("td", row).each(function(i) {',
                              'if (i == 0) return; // first column is row names',
                              'if (parseFloat(data[i]) >= 50.0)',
                              '$(this).css("color", "red");',
                              '});',
                              '}'))
                      )
        )
    
    output$ex2 <- DT::renderDataTable(
        DT::datatable(df_wide, 
                      filter = 'top',
                      rownames = FALSE,
                      options = list(pageLength = 25))
    )
    
    output$ex3 <- DT::renderDataTable(
        DT::datatable(df_top_5, 
                      filter = 'top',
                      rownames = FALSE,
                      options = list(pageLength = 25))
    )
    
    output$text <- renderText({ input$txt })
    
    output$plots <- renderPlot(
        {x <- "Brennerberg"
        df_long %>%
                filter(location == x) %>%
                ggplot(aes(x = hour, y = popularity,
                           colour = day_of_week)) + 
                geom_point() + labs(title = paste("location:", x))
        }
        )
}

# Run the application 
shinyApp(ui = ui, server = server)

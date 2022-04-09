#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
source('themes.R')

# Load files
merged_table <- read_tsv('data/merged_table.tsv')
long_table_absolute <- read_tsv('data/long_table_absolute.tsv')
long_table_percent <- read_tsv('data/long_table_percent.tsv')
specialties <- merged_table$Specialty %>% unique()
years <- unique(merged_table$Year)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NRMP Match Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = 'specialty',
                        label = 'Specialty',
                        choices = specialties, 
                        multiple = TRUE, 
                        selected = 'Dermatology'),
            selectInput(inputId = 'pgy',
                        label = 'PGY Class',
                        choices = c('PGY1', 'PGY2'), 
                        selected = 'PGY2',
                        multiple = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("plot_match_abs")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  output$plot_match_abs <- renderPlotly({
    
    p <- long_table_absolute %>% 
      filter(Specialty %in% input$specialty,
             Class == input$pgy) %>% 
      ggplot(aes(x = Year,
                 y = Value,
                 color = Name)) +
      geom_point(size = 4, alpha = 1) +
      geom_line(size = 2, alpha = 0.8) + 
      scale_x_continuous(breaks = years) +
      scale_color_carto_d(palette = 'Bold') +
      labs(y = 'Number') +
      theme_custom()
    
    if(length(input$specialty) > 1) {
      p <- p + facet_wrap(~Specialty, scales = 'free_y', ncol = 1 )
    }
    
    ggplotly(p)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

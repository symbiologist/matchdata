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
library(ggiraph)
source('themes.R')

# Load files
full_table <- read_tsv('data/full_table.tsv')
long_table_absolute <- read_tsv('data/long_table_absolute.tsv')
long_table_percent <- read_tsv('data/long_table_percent.tsv')
specialties <- full_table$Specialty %>% unique()
years <- unique(full_table$Year)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NRMP Match Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2,
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
        mainPanel(width = 8,
           girafeOutput("plot_match_abs")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  absolute_color_scale <- c('dodgerblue4',  'dodgerblue3', 'darkorchid4','darkorchid3', 'orange')
  
  output$plot_match_abs <- renderGirafe({
    
    p <- long_table_absolute %>%
      filter(Specialty %in% input$specialty,
             Class == input$pgy,
             !str_detect(Name, 'Change|Ranked|Programs')) %>% 
      mutate(Name = factor(Name, levels = c('U.S. Senior Applicants',
                                            'U.S. Senior Matches',
                                            'Total Applicants',
                                            'Total Matches',
                                            'Positions Offered'))) %>% 
      ggplot(aes(x = Year,
                 y = Value,
                 color = Name,
                 data_id = Name,
                 tooltip = Value)) +
      geom_point_interactive(size = 4, alpha = 1) +
      geom_line(size = 2, alpha = 0.8) + 
      scale_x_continuous(breaks = years) +
      scale_color_manual(values = absolute_color_scale) +
      labs(y = 'Number') +
      theme_custom(legend_position = 'top') +
      guides(color=guide_legend(nrow = 2, byrow = FALSE))
    
    if(length(input$specialty) > 1) {
      p <- p + facet_wrap(~Specialty, scales = 'free_y', ncol = 1 )
    }
    
    girafe(ggobj = p,
           width_svg = 8,
           height_svg = 5)
  })
  
  output$plot_competitive <- renderGirafe({
    
    p <- long_table_percent
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

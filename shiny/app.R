library(shiny)
library(bslib)
library(tidyverse)
library(ggiraph)
library(shinythemes)
library(RColorBrewer)
source('themes.R')

# Load files
full_table <- read_tsv('data/full_table.tsv')
match_rate_table <- read_tsv('data/match_rate_table.tsv')
combined_delta_table <- read_tsv('data/combined_delta_table.tsv')
long_table_absolute <- read_tsv('data/long_table_absolute.tsv')
long_table_percent <- read_tsv('data/long_table_percent.tsv')
specialties <- full_table$Specialty %>% unique()
years <- unique(full_table$Year)
timeframes <- combined_delta_table$Timeframe %>% unique()

sidebar_width <- 2
main_width <- 8

colors_two <- c('dodgerblue4', 'orangered')

# Define UI for application that draws a histogram
ui <- fixedPage(
  # theme = bs_theme(font_scale = 1, `enable-gradients` = TRUE, 
  #                 `enable-shadows` = TRUE, `enable-rounded` = TRUE, bootswatch = "lux"),
  theme = shinytheme('cerulean'),
  
    # Application title
    titlePanel(
      h1('NRMP Match Data', align = 'center')),
    
    navlistPanel(
      fluid = FALSE,
      widths = c(2, 10),
      
      "Over Time",
      
      tabPanel("By Specialty",
               h2('Applicants Matched Over Time', align = 'center'),
               
               fixedRow(
                 column(
                   width = 3,
                   selectInput(inputId = 'specialty',
                               label = 'Specialty',
                               choices = specialties, 
                               multiple = FALSE, 
                               selected = 'Dermatology')
                 )
                 
               ),
               fixedRow(
                 column(width = main_width,
                        girafeOutput("plot_match_abs_pgy1", height = '400px'),
                        girafeOutput('plot_match_abs_pgy2')
                        )
      )
    ),
      tabPanel("Component 2"),
      "By Year",
      tabPanel("Match Rate",
               
               h2('Match Rates by Program'),

               fixedRow(
                 column(width = sidebar_width,
                        selectInput(inputId = 'year',
                                    label = 'Year',
                                    choices = years,
                                    selected = '2022',
                                    multiple = FALSE)
                 ),
               ),
               fixedRow(
                 
                 column(width = main_width - 1,
                        h3('PGY1 Programs'),
                        girafeOutput('plot_match_rate_pgy1', 
                                     width = '500px',
                                     height = '1200px')
                 ),
                 column(width = main_width/2,
                        h3('PGY2 Programs', align = 'center'),
                        girafeOutput('plot_match_rate_pgy2',
                                     width = '500px',
                                     height = '500px')
                 )
               )
               ),
      tabPanel("Greatest Change",
               
               fixedRow(
                 column(width = 2,
                        selectInput(inputId = 'timeframe',
                                    label = 'Timeframe',
                                    choices = timeframes,
                                    selected = '1 Year')),
                 column(width = 4,
                        sliderInput(inputId = 'positions',
                                     label = 'Minimum Positions',
                                     min = 0,
                                     max = 1000, 
                                     value = 10)),
                 column(width = 4,
                        sliderInput(inputId = 'programs',
                                    label = 'Minimum No. of Programs',
                                    min = 0,
                                    max = 100, 
                                    value = 10))
                 ),
                 fixedRow(
                   column(width = 4,
                          girafeOutput('plot_delta_pgy1', width = '600px', height = '800px')),
                   column(width = main_width/2,
                          girafeOutput('plot_delta_pgy2', width = '600px', height = '300px'))
                 )
               
               )
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # bs_themer()
  
  absolute_color_scale <- c('dodgerblue4',  'dodgerblue3', 'darkorchid4','darkorchid3', 'orange')
  
  output$plot_match_abs_pgy1 <- renderGirafe({
    
    p <- long_table_absolute %>%
      filter(Specialty %in% input$specialty,
             Class == 'PGY1',
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
      labs(y = 'Number',
           title = 'PGY1 Programs') +
      theme_custom(legend_position = 'top') +
      guides(color=guide_legend(nrow = 2, byrow = FALSE))
    
    girafe(ggobj = p,
           width_svg = 8,
           height_svg = 5,
           options = list(opts_selection(type = "none"),
                          opts_toolbar(saveaspng = FALSE)))
  })
  
  output$plot_match_abs_pgy2 <- renderGirafe({
    
    p <- long_table_absolute %>%
      filter(Specialty %in% input$specialty,
             Class == 'PGY2',
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
      labs(y = 'Number',
           title = 'PGY2 Programs') +
      theme_custom(legend_position = 'none') +
      guides(color=guide_legend(nrow = 2, byrow = FALSE))
    
    girafe(ggobj = p,
           width_svg = 8,
           height_svg = 4,
           options = list(opts_selection(type = "none"),
                          opts_toolbar(saveaspng = FALSE)))
  })
  
  match_rate_table_plot <- match_rate_table %>% 
    filter(Name %in% c('U.S. Senior Match Rate %', 'Total Match Rate %')) %>% 
    mutate(Name = str_remove(Name, ' Match Rate %')) %>% 
    drop_na()
  
  output$plot_match_rate_pgy1 <- renderGirafe({
    
    p <- match_rate_table_plot %>% 
      filter(Year == input$year,
             Class == 'PGY1') %>% 
      ggplot(aes(x = reorder(Specialty, Value),
                 y = Value,
                 fill = Name,
                 tooltip = Value)) +
      geom_bar_interactive(stat = 'identity', position = 'dodge') +
      labs(x = 'Specialty') +
      theme_custom(legend_position = 'none') +
      scale_fill_manual(values = alpha(colors_two, 0.5)) +
      coord_flip() +
      labs(x = '',
           y = 'Match Rate (%)')
    
    ggiraph(ggobj = p,
            width_svg = 6,
            height_svg = 12,
            options = list(opts_selection(type = "none"),
                           opts_toolbar(saveaspng = FALSE)))
      
  })
  
  output$plot_match_rate_pgy2 <- renderGirafe({
    
    p <- match_rate_table_plot %>% 
      filter(Year == input$year,
             Class == 'PGY2') %>% 
      ggplot(aes(x = reorder(Specialty, Value),
                 y = Value,
                 fill = Name,
                 tooltip = Value)) +
      geom_bar_interactive(stat = 'identity', position = 'dodge') +
      labs(x = 'Specialty') +
      theme_custom(legend_position = 'bottom') +
      scale_fill_manual(values = alpha(colors_two, 0.5)) +
      coord_flip() +
      labs(x = '',
           y = 'Match Rate (%)')
    
    ggiraph(ggobj = p, 
            width_svg = 6,
            height_svg = 6,
            options = list(opts_selection(type = "none"),
                           opts_toolbar(saveaspng = FALSE)))
    
  })
  
  delta_table_plot <- combined_delta_table %>% 
    left_join(full_table %>%  filter(Year == 2022) %>% select(Specialty, Class, `Positions Offered`, `No. of Programs`))
  
  delta_plot <- function(input_table = delta_table_plot,
                         timeframe = input$timeframe,
                         positions = input$positions,
                         programs = input$programs,
                         class = c('PGY1', 'PGY2'),
                         group = c('Both', 'Top', 'Bottom'),
                         color_scale = colorRampPalette(rev(brewer.pal(11, 'Spectral')))(3),
                         n = 3) {
    
    input_table <- input_table %>% 
      filter(Timeframe == timeframe,
             `Positions Offered` > positions,
             `No. of Programs` > programs,
             Class == class)
    
    if(group == 'Top') {
      input_table <- input_table %>% 
        top_n(n = n, wt = `% Change in U.S. Senior Match Rate %`)
    } else if (group == 'Bottom') {
      input_table <- input_table %>% 
        top_n(n = n, wt = -`% Change in U.S. Senior Match Rate %`)
    }
    
    input_table %>% 
      ggplot(aes(x = reorder(Specialty, -`% Change in U.S. Senior Match Rate %`),
                 y = `% Change in U.S. Senior Match Rate %`,
                 fill = `% Change in U.S. Senior Match Rate %`,
                 tooltip = `% Change in U.S. Senior Match Rate %`)) +
      geom_bar_interactive(stat = 'identity', color = 'black') +
      labs(x = 'Specialty') +
      theme_custom(legend_position = 'none') +
      scale_fill_gradient2(low = color_scale[1],
                           mid = color_scale[2],
                           high = color_scale[3]) +
      coord_flip() +
      labs(x = '',
           y = 'Change in U.S. Senior Match Rate (%)')
  }
  
  delta_plot_width <- 6
  delta_plot_height <- 10
  output$plot_delta_pgy1 <- renderGirafe({
    
    p <- delta_plot(class = 'PGY1') +
      ggtitle('PGY1 Programs')
    
    ggiraph(ggobj = p, 
            width_svg = 6,
            height_svg = 8,
            options = list(opts_selection(type = "none"),
                           opts_toolbar(saveaspng = FALSE)))
    
  })
  
  output$plot_delta_pgy2 <- renderGirafe({
    
    p <- delta_plot(class = 'PGY2') +
      ggtitle('PGY2 Programs')
    
    ggiraph(ggobj = p, 
            width_svg = 6,
            height_svg = 3,
            options = list(opts_selection(type = "none"),
                           opts_toolbar(saveaspng = FALSE)))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

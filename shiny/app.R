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

prelims <- specialties %>% str_subset('Only|Prelim')

sidebar_width <- 2
main_width <- 8

colors_two <- brewer.pal(3, 'RdBu')[c(1,3)]

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = bs_theme(font_scale = 1, `enable-gradients` = TRUE,
                   `enable-shadows` = TRUE, `enable-rounded` = TRUE, bootswatch = 'litera'),
  title = "NRMP Match Data",
  fluid = FALSE,
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  #   ),
  
  tabPanel("By Specialty",
           
           fixedRow(
             column(width = 12,
                    h2('Applicants Matched Over Time', align = 'center')
             )
           ),
           
           fixedRow(
             column(width = 12,
                    'Here you can view the number of positions, applicants, and the match rate over time for a specialty of interest.
                    Select a specialty to start!'
                    ),
           ),

           fixedRow(
             column(
               width = 4,
               selectInput(inputId = 'specialty',
                           label = h5('Select a Specialty'),
                           choices = specialties, 
                           multiple = FALSE, 
                           selected = 'Dermatology')
             )
           ),
           fixedRow(
             column(width = 12,
                    h3('Absolute Number', align = 'center'))
             
           ),
           fixedRow(''),
           fixedRow(
             column(width = 6,
                    girafeOutput("plot_match_abs_pgy1", height = '350px', width = '600px')
             ),
             column(width = 6,
                    girafeOutput('plot_match_abs_pgy2', height = '350px', width = '600px')),
           ),
           fixedRow(
             column(width = 12,
                    plotOutput('plot_match_abs_legend', height = '30px')
             )
           ),
           fixedRow(
             column(width = 12,
                    h3('Match Rate %', align = 'center')),
             
           ),
           fixedRow(
             column(width = 6,
                    girafeOutput('plot_match_ratetime_pgy1', height = '350px', width = '600px')
             ),
             column(width = 6,
                    girafeOutput('plot_match_ratetime_pgy2', height = '350px', width = '600px')
             )
           ),
           fixedRow(
             column(width = 12,
                    plotOutput('plot_match_ratetime_legend', height = '30px')
             )
           ),
           fixedRow(
             br()
           )
  ),
  tabPanel("By Year",
           
           h2('Match Rates Across Programs', align = 'center'),
           
           fixedRow(
             column(width = 12,
                    'Here you can view the match rate in a given year across all specialties. Select a year and use the filters to hide smaller programs if desired.'
             ),
           ),
           
           fixedRow(
             column(width = 4,
                    h5('Filter Programs')
             )
           ),
           
           fixedRow(
             column(width = 2,
                    selectInput(inputId = 'year',
                                label = 'Select a Year',
                                choices = years,
                                selected = '2022',
                                multiple = FALSE),
             ),
             column(width = 3,
                    sliderInput(inputId = 'matchrate_positions',
                                label = 'Minimum Positions Offered',
                                min = 0,
                                max = 1000, 
                                value = 10)
             ),
             column(width = 3,
                    sliderInput(inputId = 'matchrate_programs',
                                label = 'Minimum No. of Programs',
                                min = 0,
                                max = 100, 
                                value = 10)
             ),
             column(width = 3,
                    checkboxInput(inputId = 'matchrate_checkbox',
                                       label = 'Exclude Prelim Programs',
                                       value = TRUE)
             )
           ),
           fixedRow(
             column(width = 6,
                    h3('PGY1 Programs'),
                    girafeOutput('plot_match_rate_pgy1', 
                                 width = '550px',
                                 height = '1200px')
             ),
             column(width = 6,
                    h3('PGY2 Programs', align = 'center'),
                    girafeOutput('plot_match_rate_pgy2',
                                 width = '550px',
                                 height = '500px')
             )
           ),
           fixedRow(
             column(width = 6,
                    )
           ),
           # fixedRow(
           #   column(
           #     width = 4,
           #     selectInput(inputId = 'ratetime_specialty',
           #                 label = 'Select a Specialty',
           #                 choices = specialties, 
           #                 multiple = FALSE, 
           #                 selected = 'Dermatology')
           #   )
           # )
  ),
  tabPanel("Greatest Change",
           h2('Changes in Match Rate', align = 'center'),
           fixedRow(
             column(width = 12,
                    'Which specialties saw the largest changes in match rate? Select a timeframe and use the filters to see which specialties had the largest changes.'
             ),
           ),
           fixedRow(
             column(width = 4,
                    h5('Filter Programs'))
           ),
           fixedRow(
             column(width = 2,
                    selectInput(inputId = 'timeframe',
                                label = 'Timeframe',
                                choices = timeframes,
                                selected = '1 Year')),
             column(width = 3,
                    sliderInput(inputId = 'positions',
                                label = 'Minimum Positions',
                                min = 0,
                                max = 1000, 
                                value = 10)),
             column(width = 3,
                    sliderInput(inputId = 'programs',
                                label = 'Minimum No. of Programs',
                                min = 0,
                                max = 100, 
                                value = 10)),
             column(width = 3,
                    checkboxInput(inputId = 'delta_checkbox',
                                  label = 'Exclude Prelim Programs',
                                  value = TRUE)
             )
           ),
           fixedRow(
             column(width = 6,
                    girafeOutput('plot_delta_pgy1', width = '550px', height = '800px')),
             column(width = 6,
                    girafeOutput('plot_delta_pgy2', width = '550px', height = '300px'))
           )
           
  ),
  tabPanel("Top Charts",
           fixedRow(
             column(width = 3,
                    h5('Most Competitive Specialties')),
             column(width = 3, 
                    h5('Least Competitive Specialties')),
             column(width = 3,
                    h5('Fastest Growing Specialties')),
             column(width = 3,
                    h5('Slowest Growing Specialties'))
           ),
           fixedRow(
             column(width = 3,
                    'Coming soon!')
           )
           
  ),
  tabPanel("About",
           fixedRow(
             column(width = 12,
                    h4('What happens to medical school graduates applying to residencies every year?'))
           ),
           fixedRow(column(width = 12,
                           "I'm a 4th year medical student at UCSF interested in applying genomics and informatics to medicine. 
                           I love analyzing data and wanted an interactive way to explore the numbers behind medical graduates and their intended specialties."),
                    br(),
                    column(width = 12,
                           ''),
                    br(),
                    column(width = 12,
                           "The National Residenct Matching Program (NRMP) tracks these numbers each year, which they publicly share on their website.
                           I extracted and processed the last 10 years of data to create this interactive tool so medical students can easily visualize data about their intended specialties. 
                           For example, 2022 witnessed a striking decrease in students applying in Emergency Medicine, as well as a record drop in students who successfully matched in dermatology. 
                           Hopefully by making these data more accessible, I can help medical students can arm themselves with greater knowledge about the residency match process."),
                    br(),
                    column(width = 12,
                           ''),
                    br(),
                    column(width = 12,
                           
                           "All of the original data came from public documents published by the NRMP (www.nrmp.org). These visualizations are not officially endorsed by their organization.
                           I work on this in my free time and pay to host it online, so this page remains in active (but slow) development! Feel free to contact me (dwu@ucsf.edu) if you have any suggestions.")
                           
           )
           
           )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    #bs_themer()
    
    absolute_color_scale <- c('dodgerblue4',  'dodgerblue3', 'darkorchid4','darkorchid3', 'orange')
    
    g_legend <- function(a.gplot){ 
      tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
      legend <- tmp$grobs[[leg]] 
      legend
    } 
    
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
        labs(y = '# of Applicants',
             title = 'PGY1 Programs') +
        theme_custom(legend_position = 'none')# +
      #guides(color=guide_legend(nrow = 2, byrow = FALSE))
      
      girafe(ggobj = p,
             width_svg = 6,
             height_svg = 4,
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
        labs(y = '# of Applicants',
             title = 'PGY2 Programs') +
        theme_custom(legend_position = 'none') #+
      #guides(color=guide_legend(nrow = 2, byrow = FALSE))
      
      girafe(ggobj = p,
             width_svg = 6,
             height_svg = 4,
             options = list(opts_selection(type = "none"),
                            opts_toolbar(saveaspng = FALSE)))
    })
    
    output$plot_match_abs_legend <- renderPlot({
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
        geom_point(size = 4, alpha = 1) +
        scale_color_manual(values = absolute_color_scale) +
        theme_custom(legend_position = 'bottom') +
        theme(legend.text = element_text(size = 14))
      guides(color=guide_legend(nrow = 1, byrow = TRUE))
      
      legend <- g_legend(p) 
      
      grid::grid.newpage()
      grid::grid.draw(legend) 
      
    })
    
    ##### MATCH RATE % OVER TIME
    output$plot_match_ratetime_pgy1 <- renderGirafe({
      
      p <- long_table_percent %>%
        filter(Specialty %in% input$specialty,
               Class == 'PGY1',
               !str_detect(Name, 'Change|Ranked|Programs|Filled|Predominance')) %>% 
        mutate(Name = factor(Name, levels = c('U.S. Senior Match Rate %',
                                              'Total Match Rate %'))) %>% 
        ggplot(aes(x = Year,
                   y = Value,
                   color = Name,
                   data_id = Name,
                   tooltip = Value)) +
        geom_point_interactive(size = 4, alpha = 1) +
        geom_line(size = 2, alpha = 0.8) + 
        scale_x_continuous(breaks = years) +
        scale_color_manual(values = absolute_color_scale[c(1,3)]) +
        labs(y = 'Match Rate %',
             title = 'PGY1 Programs') +
        theme_custom(legend_position = 'none') #+
      #guides(color=guide_legend(nrow = 2, byrow = FALSE))
      
      girafe(ggobj = p,
             width_svg = 6,
             height_svg = 4,
             options = list(opts_selection(type = "none"),
                            opts_toolbar(saveaspng = FALSE)))
    })
    
    output$plot_match_ratetime_pgy2 <- renderGirafe({
      
      p <- long_table_percent %>%
        filter(Specialty %in% input$specialty,
               Class == 'PGY2',
               !str_detect(Name, 'Change|Ranked|Programs|Filled|Predominance')) %>% 
        mutate(Name = factor(Name, levels = c('U.S. Senior Match Rate %',
                                              'Total Match Rate %'))) %>% 
        ggplot(aes(x = Year,
                   y = Value,
                   color = Name,
                   data_id = Name,
                   tooltip = Value)) +
        geom_point_interactive(size = 4, alpha = 1) +
        geom_line(size = 2, alpha = 0.8) + 
        scale_x_continuous(breaks = years) +
        scale_color_manual(values = absolute_color_scale[c(1,3)]) +
        labs(y = 'Match Rate %',
             title = 'PGY2 Programs') +
        theme_custom(legend_position = 'none') #+
      #guides(color=guide_legend(nrow = 2, byrow = FALSE))
      
      girafe(ggobj = p,
             width_svg = 6,
             height_svg = 4,
             options = list(opts_selection(type = "none"),
                            opts_toolbar(saveaspng = FALSE)))
    })
    
    output$plot_match_ratetime_legend <- renderPlot({
      p <- long_table_percent %>%
        filter(Specialty %in% input$specialty,
               !str_detect(Name, 'Change|Ranked|Programs|Filled|Predominance')) %>% 
        mutate(Name = factor(Name, levels = c('U.S. Senior Match Rate %',
                                              'Total Match Rate %'))) %>% 
        ggplot(aes(x = Year,
                   y = Value,
                   color = Name,
                   data_id = Name,
                   tooltip = Value)) +
        geom_point(size = 4, alpha = 1) +
        scale_color_manual(values = absolute_color_scale[c(1,3)]) +
        theme_custom(legend_position = 'bottom') +
        theme(legend.text = element_text(size = 14)) +
      guides(color=guide_legend(nrow = 1, byrow = TRUE))
      
      legend <- g_legend(p) 
      
      grid::grid.newpage()
      grid::grid.draw(legend) 
      
    })
    
    ### MATCH RATE IN A YEAR
    match_rate_table_plot <- match_rate_table %>% 
      filter(Name %in% c('U.S. Senior Match Rate %', 'Total Match Rate %')) %>% 
      mutate(Name = str_remove(Name, ' Match Rate %')) %>% 
      drop_na()
    
    output$plot_match_rate_pgy1 <- renderGirafe({
      
      filtered_programs <- full_table %>% 
        filter(`Positions Offered` > input$matchrate_positions,
               `No. of Programs` > input$matchrate_programs,
               Year == input$year) %>% 
        select(Specialty, Class, Year)
      
      if(input$matchrate_checkbox) {
        filtered_programs <- filtered_programs %>% 
          filter(!(Specialty %in% prelims))
      }
      
      p <- filtered_programs %>% 
        left_join(match_rate_table_plot) %>% 
        filter(Class == 'PGY1') %>% 
        ggplot(aes(x = reorder(Specialty, Value),
                   y = Value,
                   fill = Name,
                   tooltip = Value)) +
        geom_bar_interactive(stat = 'identity', position = 'dodge', color = 'black') +
        labs(x = 'Specialty') +
        theme_custom(legend_position = 'none') +
        scale_fill_manual(values = colors_two) +
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
      
      filtered_programs <- full_table %>% 
        filter(`Positions Offered` > input$matchrate_positions,
               `No. of Programs` > input$matchrate_programs,
               Year == input$year) %>% 
        select(Specialty, Class, Year)
      
      p <- filtered_programs %>% 
        left_join(match_rate_table_plot) %>% 
        filter(Class == 'PGY2') %>% 
        ggplot(aes(x = reorder(Specialty, Value),
                   y = Value,
                   fill = Name,
                   tooltip = Value)) +
        geom_bar_interactive(stat = 'identity', position = 'dodge', color = 'black') +
        labs(x = 'Specialty') +
        theme_custom(legend_position = 'bottom') +
        scale_fill_manual(values = colors_two) +
        coord_flip() +
        labs(x = '',
             y = 'Match Rate (%)')
      
      ggiraph(ggobj = p, 
              width_svg = 6,
              height_svg = 5,
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
      
      if(input$delta_checkbox) {
        input_table <- input_table %>% filter(!(Specialty %in% prelims))
      }
      
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
             y = 'Change in Match Rate %')
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
  
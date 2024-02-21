library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggalt)
library(forcats)
library(R.utils)
library(png)
library(grid)
library(ggpubr)
library(scales)
library(bbplot)
library(ggthemes)
library(colourpicker)
library(DT)
library(shinythemes)

crime <- read.csv('C:/Users/DELL/Downloads/crime_2016_tidy.csv')

# List of crimes
crime_list <- unique(crime$crime)

# List of states
state_list <- unique(crime$state)

# Overview data
overview_data <- crime %>% group_by(state) %>% filter(crime == 'Total Cognizable IPC Crime')

# IPC Table
ipc_main <- as.data.frame(list(unique(crime$crime)), col.names = c('main')) %>% 
  mutate(Section = main %>% str_extract('(Sec.*)') %>% str_replace('(IPC.*)', ''), Description = main %>% str_replace('(Sec.*IPC)', '') %>% str_remove_all('-')) %>%
  select(-main) %>% 
  drop_na()

# Plot Crime Data
plot_crime <- function(crime_type, color){
  plot <- ggplot(crime %>% filter(crime == crime_type), aes(x=reorder(state, incidents), y=incidents)) + 
    geom_col(fill=color) + 
    geom_hline(yintercept = 0, size = 1, colour="#333333") +
    guides(fill=F) +
    bbc_style() +
    labs(title=paste('Incidents of', crime_type), subtitle = 'Crime Statistics, 2016', y='Number of incidents') +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(margin=margin(t = 13, b = 10)), panel.grid.major.x = element_line(color="#cbcbcb"), 
          panel.grid.major.y=element_blank())
  
  finalise_plot(plot, source = 'Source: National Crime Record Bureau', save_filepath = paste(crime_type, '.jpg', sep = ''), 
                width_pixels = 1000, height_pixels = 1000)
}

# Plot State Data
plot_state <- function(state_name, color, numberOfCrimes){
  # Filtering top 10 crimes by incidents in selected state
  state_crime_data <- crime %>% filter(state == state_name & !crime %in% c('Total Cognizable IPC Crime', 'Other IPC Crimes')) %>% 
    top_n(numberOfCrimes, wt = incidents) %>% mutate(crime = str_extract(crime, '(Sec.*)'))
  
  plot <- ggplot(state_crime_data, aes(x=reorder(crime, incidents), y=incidents)) + 
    geom_col(fill=color) + 
    geom_hline(yintercept = 0, size = 1, colour="#333333") +
    guides(fill=F) +
    bbc_style() +
    labs(title=paste('Incidents in', state_name), subtitle = 'Crime Statistics, 2016', y='Number of incidents') +
    scale_y_continuous(labels = scales::comma) +
    coord_flip() +
    theme(axis.text.x = element_text(margin=margin(t = 13, b = 10)), panel.grid.major.x = element_line(color="#cbcbcb"), 
          panel.grid.major.y=element_blank())
  
  finalise_plot(plot, source = 'Source: National Crime Record Bureau', save_filepath = paste(state_name, '.jpg', sep = ''),
                width_pixels = 1000, height_pixels = 1000)
}

# App begins here
library(shiny)

# Define UI for application
ui <- fluidPage(theme = shinytheme('flatly'),
                
                # Application title
                titlePanel("Crime In India"),
                
                
                tabsetPanel(
                  tabPanel('Overview',
                           sidebarLayout(
                             sidebarPanel(
                               h2('Overview of Crimes'),
                               p('This chart shows the total number of cognizable IPC crimes in every state, sorted by number of incidents'),
                               DT::dataTableOutput('stateOverviewTable')
                               
                               
                             ),
                             # Overivew
                             mainPanel(
                               plotOutput("overviewPlot", height = '750px')
                             )
                           )
                  ),
                  
                  tabPanel('Crime-wise', 
                           # Crime-Wise
                           sidebarLayout(
                             sidebarPanel(
                               selectizeInput('typeCrime', 'Type of crime', crime_list, multiple = F),
                               colourInput('colorPlotCrime', 'Select color', value='#BA1A52')
                             ),
                             
                             # Show a plot
                             mainPanel(
                               plotOutput("crimePlot", height = '750px')
                             )
                           )
                  ),
                  tabPanel('State-Wise',
                           # State-Wise
                           sidebarLayout(
                             sidebarPanel(
                               selectizeInput('whichState', 'Select State/UT', state_list, multiple = F),
                               sliderInput('numberOfCrimes', 'Select maximum number of crimes', min=5, max=20, value = 10, step=1),
                               colourInput('colorPlotState', 'Select color', value='#BA1A52')
                             ),
                             
                             # Show a plot
                             mainPanel(
                               plotOutput("statePlot", height = '750px')
                             )
                           )
                  ),
                  tabPanel('Indian Penal Code - Section Descriptions',
                           # State-Wise
                           sidebarLayout(
                             sidebarPanel(
                               h3('The table on the right gives a description of the sections of the Indian Penal Code'),
                               p('Use the search box to filter a particular section of the IPC')
                             ),
                             
                             # Show a plot
                             mainPanel(
                               DT::dataTableOutput("IPCTable")
                             )
                           )
                  )
                )
)

# Define server logic
server <- function(input, output) {
  
  output$crimePlot <- renderPlot({
    plot_crime(input$typeCrime, input$colorPlotCrime)
  })
  output$statePlot <- renderPlot({
    plot_state(input$whichState, input$colorPlotState, input$numberOfCrimes)
  })
  output$overviewPlot <- renderPlot({
    overview_plot <- ggplot(overview_data, 
                            aes(x=reorder(state, -incidents), y=incidents)) + 
      geom_col(fill='#F63231') + 
      geom_hline(yintercept = 0, size = 1, colour="#333333") +
      guides(fill=F) +
      bbc_style() +
      labs(title='Overview of crimes', subtitle = 'Crime Statistics, 2016', y='Number of incidents') +
      scale_y_continuous(labels = scales::comma, breaks = c(0, 50000, 100000, 150000, 200000, 250000)) +
      scale_x_discrete(labels=abbreviate) +
      theme(axis.text.x = element_text(angle = 90))
    
    finalise_plot(overview_plot, source = 'Source: National Crime Record Bureau', save_filepath = 'overview_plot.jpg', 
                  width_pixels = 1500, height_pixels = 1000)
  })
  
  output$stateOverviewTable <- DT::renderDataTable({overview_data %>% select(state, incidents) %>% 
      rename('State' = state, 'No. of incidents' = incidents)},
      selection = 'none', 
      options = list(order = list(list(1, 'desc')), dom='pt', pagingType = 'numbers'), 
      rownames = F)
  
  output$IPCTable <- DT::renderDataTable({ipc_main},
                                         selection = 'none', options = list(dom='ptf', pagingType='numbers'), 
                                         rownames = F)
}

# Run the application 
shinyApp(ui = ui, server = server)


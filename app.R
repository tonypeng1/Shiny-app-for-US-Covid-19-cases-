library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(lubridate)
library(plotly)
library(zoo)

X <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")

# set length of dates
len <- 150
cols <- names(X)
date_range <- tail(cols, len + 1)

# Select interested counties and dates
Z <- X %>% 
    filter((County.Name == 'Harris County' & State == 'TX') | 
               (County.Name == 'Dallas County' & State == 'TX') | 
               (County.Name == 'Travis County' & State == 'TX') |
               (County.Name == 'Hillsborough County' & State == 'FL') | 
               (County.Name == 'Pinellas County' & State == 'FL') |
               (County.Name == 'Orange County' & State == 'FL') | 
               (County.Name == 'Miami-Dade County' & State == 'FL') | 
               (County.Name == 'Los Angeles County' & State == 'CA') | 
               (County.Name == 'Santa Clara County' & State == 'CA') | 
               (County.Name == 'San Francisco County' & State == 'CA') |
               (County.Name == 'San Diego County' & State == 'CA') |
               (County.Name == 'Mecklenburg County' & State == 'NC') |
               (County.Name == 'Washington' & State == 'DC') |
               (County.Name == "Prince George's County" & State == 'MD') |
               (County.Name == "Montgomery County" & State == 'MD') |
               (County.Name == "Fairfax County" & State == 'VA') |
               (County.Name == "Fulton County" & State == 'GA') |
               (County.Name == "Suffolk County" & State == 'MA') |
               (County.Name == "Cook County" & State == 'IL') |
               (County.Name == "New York County" & State == 'NY') |
               (County.Name == "Maui County" & State == 'HI') |
               (County.Name == "Maricopa County" & State == 'AZ') |
               (County.Name == "Denver County" & State == 'CO') |
               (County.Name == "Clark County" & State == 'NV') |
               (County.Name == "Marion County" & State == 'IN') |
               (County.Name == "Davidson County" & State == 'TN') |
               (County.Name == "Philadelphia County" & State == 'PA') |
               (County.Name == "Hennepin County" & State == 'MN') |
               (County.Name == 'King County' & State == 'WA')
    ) %>% 
    select(countyFIPS:State, date_range[1]:date_range[len + 1])

city_table <- list(
    'Harris County' = 'Houstin Region',
    'Dallas County' = 'Dallas Region',
    'Travis County' = 'Austin Region',
    'Hillsborough County' = 'Tampa Region',
    'Orange County' = 'Orlando Region',
    'Pinellas County' = 'Clearwater/St. Petersburg Region',
    'Miami-Dade County' = 'Miami Region',
    'Los Angeles County' = 'Los Angeles Region',
    'Santa Clara County' = 'San Jose Region',
    'San Francisco County' = 'San Francisco Region',
    'San Diego County' = 'San Diego Region',
    'Mecklenburg County' = 'Charlotte Region',
    'Washington' = 'Washington DC',
    "Prince George's County" = 'Washington Metro - East',
    "Montgomery County" = 'Washington Metro - North',
    "Fairfax County" = 'Washington Metro - West',
    "Fulton County" = 'Atlanta Region',
    "Suffolk County" = 'Boston Region',
    "Cook County" = 'Chicago Region',
    "New York County" = 'New York Region',
    "Maui County" = 'Maui Island Region',
    "Maricopa County" = 'phoenix Region',
    "Denver County" = 'Denver Region',
    "Clark County" = 'Las Vegas Region',
    "Marion County" = 'Indianapolis Region',
    "Davidson County" = 'Nashville Region',
    "Philadelphia County" = 'Philadelphia Region',
    "Hennepin County" = 'Minneapolis Region',
    "King County" = 'Seattle Region'
)

ZZ <- Z

ui <- dashboardPage(
        dashboardHeader(title = 'Covid-19 Cases in US Cities',
                        titleWidth = 300),
        dashboardSidebar(
                sidebarMenu(
                    selectInput(inputId = 'county_of_city', label = "City Region",
                                c('Atlanta' = 'Fulton County',
                                  'Austin' = 'Travis County',
                                  'Boston' = 'Suffolk County',
                                  'Charlotte' = 'Mecklenburg County',
                                  'Chicago' = 'Cook County',
                                  'Clearwater/St. Petersburg' = 'Pinellas County',
                                  'Dallas' = 'Dallas County',
                                  'Denver' = 'Denver County',
                                  'Houston' = 'Harris County',
                                  'Indianapolis' = 'Marion County',
                                  'Las Vegas' = 'Clark County',
                                  'Los Angeles' = 'Los Angeles County',
                                  'Maui Island' = 'Maui County',
                                  'Miami' = 'Miami-Dade County',
                                  'Minneapolis' = 'Hennepin County',
                                  'Nashville' = 'Davidson County',
                                  'New York' = 'New York County',
                                  'Orlando' = 'Orange County',
                                  'Philadelphia' = 'Philadelphia County',
                                  'Phoenix' = 'Maricopa County',
                                  'San Diego' = 'San Diego County',
                                  'San Francisco' = 'San Francisco County',
                                  'San Jose' = 'Santa Clara County',
                                  'Seattle' = 'King County',
                                  'Tampa' = 'Hillsborough County',
                                  'Washington DC' = 'Washington',
                                  'Washington Metro - East' = "Prince George's County",
                                  'Washington Metro - North' = "Montgomery County",
                                  'Washington Metro - West' = "Fairfax County"
                                  ),
                                selected = 'Travis County'),
                    menuItem('Data Source', icon = icon('database'),
                             href = 'https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/',
                             newtab = FALSE),
                    menuItem('Source code', icon = icon('file-code-o'),
                             href = 'https://github.com/tonypeng1/Shiny-app-for-US-Covid-19-cases-/', newtab = FALSE)
                        )
                    ),
        dashboardBody(
                fluidRow(
                        infoBoxOutput('city', width = 3),
                        infoBoxOutput('county', width = 3),
                        infoBoxOutput('date', width = 3),
                        infoBoxOutput('value', width = 3)
                ),
                fluidRow(
                    box(plotlyOutput("plot", height = 500), 
                        title = textOutput('title'), status = 'primary',
                        solidHeader = TRUE, width = 9),
                    box(title = 'Plot Type', status = 'primary', solidHeader = TRUE,
                        selectInput(inputId = 'type', label = 'Total or New Cases:',
                                    c('New Cases' = 'New Cases', 
                                      "Total Cases" = 'Total Cases'),
                                      selected = 'New Case'), width = 3
                        )
                    )
            )
        )

server <- function(input, output) {
    
        # Plot
        output$plot <- renderPlotly({
            
            # Run this code block if new cases. Change total cases to new cases & remove 
            # the last column
            if (input$type == 'New Cases') {
                for (i in 2:(len + 1)) {
                    ZZ <- ZZ %>% mutate(!!date_range[i - 1] := 
                                            !!as.name(date_range[i]) - 
                                            !!as.name(date_range[i - 1]))
                }
                ZZ <- select(ZZ, -date_range[len + 1])
            }
            
            # Select the county to plot and leave only the date columns
            ZZZ <- ZZ %>% 
                filter(County.Name == input$county_of_city) %>% 
                select(-(countyFIPS:State))
            
            # Change table from wide to long format
            ZZZ_long <- pivot_longer(ZZZ, col = date_range[1]:date_range[len], names_to = 'date')
            
            # Remove the 'X' in front of the string and change date to 'date' format
            ZZZ_long$date <- ZZZ_long$date %>%
                substring(2) %>% 
                mdy()
            
            output$value <- renderInfoBox({
                infoBox('Cases', ZZZ_long$value[len], 
                        icon = icon('sad-tear'), color = 'orange')
            })
            
            output$date <- renderInfoBox({
                infoBox('Date', ZZZ_long$date[len], 
                        icon = icon('calendar-alt'), color = 'orange')
            })
            
            ZZZ_long <- ZZZ_long %>% 
                mutate(Rolling_Mean_14_days = round(rollmean(value, 14, na.pad=TRUE),
                                              digits = 0))
            
            p <- ggplot(data = ZZZ_long) + 
                geom_col(aes(x = date, y = value), fill = 'darkslateblue') +
                theme(plot.title = element_text(hjust = 0.5)) +
                theme(axis.title.x = element_blank(), 
                      axis.title.y = element_blank()) +
                geom_line(aes(x = date, y = Rolling_Mean_14_days), 
                          color = 'orange')

            xlab("Date")
            ggplotly(p)
        })
        
        output$city <- renderInfoBox({
            name <- input$county_of_city
            infoBox('City', city_table[[name]], icon = icon('archway'), 
                    color = 'orange')
        })
        
        output$county <- renderInfoBox({
            infoBox('County', input$county_of_city, 
                    icon = icon('city'), color = 'orange')
        })
        
        output$title <- renderText(paste(input$county_of_city, input$type, 
                                         sep = ' '))
}

shinyApp(ui, server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse,warn.conflicts = FALSE)
library(httr)
covid_cases <- read_csv('data/covid_cases.csv')


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("COVID plots for Upper Tier Local Authorities"),
    
    # Sidebar with a button to get the latest document
    # And a drop down box to choose the UTLA from the list in areaName
    sidebarLayout(
        sidebarPanel(
            actionButton("refresh","Get latest data"),
            selectInput("utla",
                        "Choose UTLA:",
                        choices=levels(as.factor(covid_cases$areaName)),
                        selected="Islington"),
            dateRangeInput("date_range","Range for plots:",
                           min="2020-01-05",
                           max=Sys.Date(),
                           start="2020-11-01",
                           end=Sys.Date()),
            radioButtons("date_type","Choose Date Type:",
                         choices=c("Publish Date" = "pub", 
                                   "Specimen Date" = "spec"))
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            textOutput("today_cases"),
            textOutput("today_rate"),
            textOutput("change_yesterday"),
            textOutput('change_lastweek'),
            plotOutput("case_plot"),
            tableOutput("utla_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    utla_cases <- reactive({filter(covid_cases,areaName==input$utla) %>% 
            select(-(areaType:areaName)) %>% 
            arrange(desc(date)) })
    #TODO: Switch to most recent date depending on specimen or pub date
    today_cases <- reactive({head(utla_cases(),1)})
    # TODO: Switch plot based on specimen date or pub date
    # TODO: Add London and National Trend lines
    # TODO: Make interactive
    output$case_plot <- renderPlot({
        ggplot(filter(utla_cases(), 
                      date >= input$date_range[1],
                      date <= input$date_range[2]),aes(x=date)) + 
            geom_col(aes(y=newCasesByPublishDatePer100),color="purple") + 
            geom_line(aes(y=newCasesByPublishDateRollingRateDay)) +   
            geom_point(aes(y=newCasesByPublishDateRollingRateDay)) + 
            scale_x_date("Date",date_breaks="1 week") + 
            theme(axis.text.x=element_text(angle=90)) 
    })
    output$today_cases <- renderText({
        sprintf("Cases: %d",today_cases()$newCasesByPublishDate)
    })
    output$today_rate <- renderText({
        sprintf("Rate: %6.1f",today_cases()$newCasesByPublishDateRollingRateDay)
    })
    output$change_yesterday <- renderText({
        sprintf("Rate change from yesterday: %5.1f %%",
                100*today_cases()$newCasesByPublishDatePercentChangeDay)
    })
    output$change_lastweek <- renderText({
        sprintf("Rate change from last week: %5.1f %%",
                100*today_cases()$newCasesByPublishDatePercentChangeWeek)
    })
    #TODO: Clean up table
    output$utla_table <-renderTable({mutate(utla_cases(),
                                            date=as.character(date))})
    
    # BUTTON to LOAD IN NEW DATA - then process accordingly
    # TODO: Grab national and London data 
    observeEvent(input$refresh, {
        r <- GET('https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=newCasesByPublishDate&metric=newCasesByPublishDateRollingRate&metric=newCasesByPublishDateRollingSum&metric=newCasesBySpecimenDate&metric=newCasesBySpecimenDateRollingRate&format=csv')
        covid_cases <<- content(r) %>% 
            group_by(areaName) %>% 
            arrange(areaName,date) %>% 
            mutate(newCasesByPublishDateRollingRateDay=newCasesByPublishDateRollingRate / 7,
                   newCasesBySpecimenDateRollingRateDay=newCasesBySpecimenDateRollingRate / 7,
                   utlaPop = newCasesByPublishDateRollingSum / newCasesByPublishDateRollingRate,
                   newCasesByPublishDatePer100=newCasesByPublishDate/utlaPop,
                   newCasesBySpecimenDatePer100=newCasesBySpecimenDate/utlaPop,
                   newCasesByPublishDatePercentChangeDay=(newCasesByPublishDateRollingRate-lag(newCasesByPublishDateRollingRate,1))/lag(newCasesByPublishDateRollingRate,1),
                   newCasesByPublishDatePercentChangeWeek=(newCasesByPublishDateRollingRate-lag(newCasesByPublishDateRollingRate,7))/lag(newCasesByPublishDateRollingRate,7))
        write_csv(covid_cases,"data/covid_cases.csv")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

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
                           start="2020-10-01",
                           end=Sys.Date()),
            radioButtons("date_type","Choose Date Type:",
                         choices=c("Publish Date" = "pub", 
                                   "Specimen Date" = "spec"))
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            textOutput("summary_stats"),
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
    today_cases <- reactive({head(utla_cases(),1)})
    output$case_plot <- renderPlot({
        ggplot(filter(utla_cases(), 
                      date >= input$date_range[1],
                      date <= input$date_range[2]),aes(x=date)) + 
            geom_col(aes(y=newCasesByPublishDatePer100),color="purple") + 
            geom_line(aes(y=newCasesByPublishDateRollingRateDay)) +   
            geom_point(aes(y=newCasesByPublishDateRollingRateDay))
    })
    output$summary_stats <- renderText({
        paste0("Cases:",today_cases()$newCasesByPublishDate,
               "<br>Rate:",today_cases()$newCasesByPublishDateRollingRateDay)
    })
    output$utla_table <-renderTable({utla_cases()})
    
    # BUTTON to LOAD IN NEW DATA - then process accordingly
    observeEvent(input$refresh, {
        r <- GET('https://api.coronavirus.data.gov.uk/v2/data?areaType=utla&metric=newCasesByPublishDate&metric=newCasesByPublishDateRollingRate&metric=newCasesByPublishDateRollingSum&metric=newCasesBySpecimenDate&metric=newCasesBySpecimenDateRollingRate&format=csv')
        covid_cases <- content(r) %>% 
            mutate(newCasesByPublishDateRollingRateDay=newCasesByPublishDateRollingRate / 7,
                   newCasesBySpecimenDateRollingRateDay=newCasesBySpecimenDateRollingRate / 7,
                   utlaPop = newCasesByPublishDateRollingSum / newCasesByPublishDateRollingRate,
                   newCasesByPublishDatePer100=newCasesByPublishDate/utlaPop)
        write_csv(covid_cases,"data/covid_cases.csv")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

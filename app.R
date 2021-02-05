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
cases_by_utla <- read_csv('data/utla_cases.csv')
uk_cases <- read_csv("data/uk_cases.csv")
london_cases <- read_csv("data/london_cases.csv")

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
                        choices=levels(as.factor(cases_by_utla$areaName)),
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
    utla_cases <- reactive({ cases_by_utla %>% 
            filter(areaName==input$utla) %>% 
            arrange(date) %>% 
            mutate(area_pop = newCasesByPublishDateRollingSum / 
                       newCasesByPublishDateRollingRate,
                   new_cases=newCasesByPublishDate,
                   new_cases_rate_day=newCasesByPublishDateRollingRate / 7,
                   new_cases_change_day = (new_cases_rate_day-lag(new_cases_rate_day,1))
                                           /lag(new_cases_rate_day,1),
                   new_cases_change_week = (new_cases_rate_day-lag(new_cases_rate_day,7))
                   /lag(new_cases_rate_day,7),
                   new_cases_per_100=newCasesByPublishDate/area_pop) %>% 
            select(date,areaName,starts_with("new_cases")) })
    uk_plot_cases <- reactive({uk_cases %>% 
            arrange(date) %>% 
            mutate(area_pop = newCasesByPublishDateRollingSum / 
                       newCasesByPublishDateRollingRate,
                   new_cases=newCasesByPublishDate,
                   new_cases_sum=newCasesByPublishDateRollingSum,
                   new_cases_rate_day=newCasesByPublishDateRollingRate / 7,
                   new_cases_change_day = (new_cases_rate_day-lag(new_cases_rate_day,1))
                   /lag(new_cases_rate_day,1),
                   new_cases_change_week = (new_cases_rate_day-lag(new_cases_rate_day,7))
                   /lag(new_cases_rate_day,7),
                   new_cases_per_100=newCasesByPublishDate/area_pop) %>% 
            select(date,areaName,starts_with("new_cases")) })
    london_plot_cases <- reactive({london_cases %>% 
            arrange(date) %>% 
            mutate(area_pop = newCasesByPublishDateRollingSum / 
                       newCasesByPublishDateRollingRate,
                   new_cases=newCasesByPublishDate,
                   new_cases_rate_day=newCasesByPublishDateRollingRate / 7,
                   new_cases_change_day = (new_cases_rate_day-lag(new_cases_rate_day,1))
                   /lag(new_cases_rate_day,1),
                   new_cases_change_week = (new_cases_rate_day-lag(new_cases_rate_day,7))
                   /lag(new_cases_rate_day,7),
                   new_cases_per_100=newCasesByPublishDate/area_pop) %>% 
            select(date,areaName,starts_with("new_cases")) })
    #TODO: Switch to most recent date depending on specimen or pub date
    today_cases <- reactive({tail(utla_cases(),1)})
    # TODO: Switch plot based on specimen date or pub date
    # TODO: Make interactive
    output$case_plot <- renderPlot({
        bind_rows(utla_cases(),uk_plot_cases(),london_plot_cases()) %>% 
            filter(date >= input$date_range[1],
                   date <= input$date_range[2]) %>% 
        ggplot(aes(x=date)) + 
            geom_col(data= function(x) { filter(x, areaName==input$utla) },
                     aes(y=new_cases_per_100),color="purple",alpha=0.5) + 
            geom_line(aes(y=new_cases_rate_day,colour=areaName)) +   
            scale_x_date("Date",date_breaks="1 week") + 
            labs(y="New cases per 100k population",
                 x="Date") + 
            theme(axis.text.x=element_text(angle=90),
                  legend.position="bottom") 
    })
    output$today_cases <- renderText({
        sprintf("Cases: %d",today_cases()$new_cases)
    })
    output$today_rate <- renderText({
        sprintf("Rate: %6.1f",today_cases()$new_cases_rate_day)
    })
    output$change_yesterday <- renderText({
        sprintf("Rate change from yesterday: %5.1f %%",
                100*today_cases()$new_cases_change_day)
    })
    output$change_lastweek <- renderText({
        sprintf("Rate change from last week: %5.1f %%",
                100*today_cases()$new_cases_change_week)
    })
    #TODO: Clean up table
    output$utla_table <-renderTable({utla_cases() %>% 
            arrange(desc(date)) %>% 
            mutate(date=as.character(date))})
    
    # BUTTON to LOAD IN NEW DATA - then process accordingly
    # TODO: Grab national and London data 
    observeEvent(input$refresh, {
        url_root <- "https://api.coronavirus.data.gov.uk/v2/data?"
        area_type <- "areaType=utla&"
        metric_string <- "metric=newCasesByPublishDate&metric=newCasesByPublishDateRollingRate&metric=newCasesByPublishDateRollingSum&metric=newCasesBySpecimenDate&metric=newCasesBySpecimenDateRollingRate&format=csv"
        r <- GET(paste0(url_root,area_type,metric_string))
        cases_by_utla <<- content(r) 
        write_csv(cases_by_utla,"data/utla_cases.csv")
        area_type <- "areaType=overview&"
        r <- GET(paste0(url_root,area_type,metric_string))
        uk_cases <<- content(r) 
        write_csv(uk_cases,"data/uk_cases.csv")
        area_type <- "areaType=region&areaCode=E12000007&"
        r <- GET(paste0(url_root,area_type,metric_string))
        london_cases <<- content(r) 
        write_csv(london_cases,"data/london_cases.csv")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(highcharter)
library(DT)
library(tidyverse)
source("./lib.r")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    title = "COVID19",
    

    # Application title
    header = dashboardHeaderPlus(title = "World COVID19 Status"),
    
    # Sidebar with a slider input for number of bins 
    sidebar = dashboardSidebar(
       checkboxInput(inputId = "log", "Log Scale", value = FALSE),
       selectInput(inputId = "country", label = "Country", choices = c("All"), selected = "All"),
       selectInput(inputId = "state", label = "Province/State", choices = c("All"), selected = "All")
    ),
    
    body = dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 12,
            fluidRow(
                valueBoxOutput("activeCases"),
                valueBoxOutput("recovered"),
                valueBoxOutput("newcases"),
                valueBoxOutput("death")
            ),
            fluidRow(
                box(title = "World Stat", width = 6, solidHeader = TRUE, 
                    highchartOutput("map")),
                
                box(id = "top10", title = "Top 10", width = 6, height = 400,
                    tableOutput("top_10"))
            ),
            fluidRow(
                tabBox(id = "charts", title = "Trends", width = 12,
                tabPanel("Daily Trend", highchartOutput("trend")),
                tabPanel("Daily Changes", highchartOutput("rate")))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # get data from wikipedia
    df <- readRDS("./summary.rds")
    
    date_diff = today() - max(df$date)
    print(date_diff)
    
    if (date_diff > 0) {
        df <- get_jh_summary_data()
        saveRDS(df, "./summary.rds")
    }
    
    # get latest data
    df_latest <- df %>% filter(date == max(df$date))
    
    countries <- c("All", df$Country.Region %>% unique() %>% sort())
    updateSelectInput(session, "country", choices = countries, selected = "All")
    
    rv <- reactiveValues()
    
    
    observeEvent(input$country, {
        if(input$country == "All")
            updateSelectInput(session, "state", choices = c("All"), selected = "All")
        else {
            filter <- df %>% filter(Country.Region == input$country)
            states <- c("All", filter$Province.State %>% unique() %>% sort())
            updateSelectInput(session, "state", choices = states, selected = "All")
            
        }
    })
    
    df_output <- reactive({
        if(input$country == "All"){
            data <- df %>% group_by(date) %>% summarise(confirmed = sum(confirmed), recovered = sum(recovered), dead = sum(dead)) %>% ungroup()
        } else {
            if(input$state == "All"){
                data <- df %>% filter(Country.Region == input$country) %>% 
                    group_by(date) %>% summarise(confirmed = sum(confirmed), recovered = sum(recovered), dead = sum(dead)) %>% ungroup()
            } else {
                data <- df %>% filter(Country.Region == input$country & Province.State == input$state) %>% 
                    group_by(date) %>% summarise(confirmed = sum(confirmed), recovered = sum(recovered), dead = sum(dead)) %>% ungroup()
            }
            
        }
        
        data %>% filter(confirmed + dead + recovered > 0) %>%
            arrange(desc(date)) %>%
            mutate(rate_of_increase = round((confirmed - lead(confirmed))/lead(confirmed), 4) * 100,
                   mortality_rate = round(dead/(confirmed+recovered+dead), 4) * 100,
                   recovery_rate = round(recovered/(confirmed+recovered+dead), 4) * 100) 
    })
    
    df_info <- reactive({
        t <- df_output()
        df_info <- t %>% mutate(new_case = confirmed - lead(confirmed)) %>%
                    filter(date == max(t$date))
    })
    
    output$summary <- renderDataTable({
        df_output()
    })
    
    
    output$top_10 <- renderTable({
        #summary_table_fields = list(Country = "Country.Region", Active = "confirmed", `Mortality %` ="mortality_rate")
        df_latest %>% group_by(Country.Region) %>%
            summarise(confirmed = sum(confirmed), recovered = sum(recovered), dead = sum(dead)) %>% ungroup() %>%
            mutate(total = confirmed+recovered+dead,
                   mortality_rate = paste(round(dead/(confirmed+recovered+dead), 4) * 100, "%"),
                   recovery_rate = paste(round(recovered/(confirmed+recovered+dead), 4) * 100, "%")) %>%
            arrange(desc(total)) %>%
            head(10) %>%
            select(Country = Country.Region, Active = confirmed, Recovered = recovered, Dead = dead, `Mortality %`=mortality_rate, `Recovery %`= recovery_rate)
    })
    
    # output$top_10_state <- renderTable({
    #     df_latest %>% group_by(Country.Region, Province.State) %>%
    #         summarise(confirmed = sum(confirmed), recovered = sum(recovered), dead = sum(dead)) %>% ungroup() %>%
    #         mutate(total = confirmed+recovered+dead,
    #                mortality_rate = round(dead/(confirmed+recovered+dead), 4) * 100,
    #                recovery_rate = round(recovered/(confirmed+recovered+dead), 4) * 100) %>%
    #         arrange(desc(total)) %>%
    #         head(10) %>%
    #         select(Country = Country.Region, `Province/State` = Province.State, Active = confirmed, `Mortality %`=mortality_rate, `Recovery %`= recovery_rate)    
    # })
    
    
    output$trend <- renderHighchart({
        data <- df_output() %>% arrange(date)

        thm <- hc_theme(
              colors = c("#41B5E9", "#FA8832", "#34393C", "#E46151")
        )
        
        hc <- highchart(width = "100%") %>%
            hc_legend(enabled = TRUE) %>% 
            hc_xAxis(title = "date", categories = data$date, type = "column", crosshair = TRUE) %>%
            hc_yAxis_multiples(list(title = list(text = "# of cases"),opposite=FALSE),list(title = list(text = "rates"), opposite=TRUE)) %>% 
            hc_add_series(name = "Passed Away", type = "column", data = data$dead, yAxis = 0) %>%
            hc_add_series(name = "Recovered", type = "column", data = data$recovered, yAxis = 0) %>%
            hc_add_series(name = "Confirmed", type = "column", data = data$confirmed, yAxis = 0) %>%
            #hc_add_series(name = "Mortality Rate", type = "line", data = data$mortality_rate, color = "#380505", yAxis = 1, marker = list(enable = FALSE)) %>%
            #hc_add_series(name = "Recovery Rate", type = "line", data = data$recovery_rate, color = "#096904", yAxis = 1, marker = list(enable = FALSE)) %>%
            hc_add_series(name = "% New Case", type = "line", data = data$rate_of_increase, yAxis = 1, marker = list(enable = FALSE)) %>%
            hc_plotOptions(column=list(stacking='normal')) %>%
            hc_add_theme(hc_theme_tufte())
            #hc_colors(viridis(4))
            
        if(input$log){
            hc %>% hc_yAxis_multiples(list(title = list(text = "# of cases"),opposite=FALSE, type = 'logarithmic'),list(title = list(text = "rates", type = 'logarithmic'),opposite=TRUE)) 
        } else {
            hc
        }

    })
    
    
    output$rate <- renderHighchart({
        data <- df_output() %>% arrange(date)
        
        thm <- hc_theme(
            colors = c("#41B5E9", "#FA8832", "#34393C", "#E46151")
        )
        
        hc <- highchart(width = "100%") %>%
            hc_legend(enabled = TRUE) %>% 
            hc_xAxis(title = "date", categories = data$date, type = "column", crosshair = TRUE) %>%
            hc_yAxis(title = list(text = "# of cases"), opposite=FALSE, crosshair = TRUE) %>% 
            hc_add_series(name = "Mortality Rate", type = "line", data = data$mortality_rate, marker = list(enable = FALSE)) %>%
            hc_add_series(name = "Recovery Rate", type = "line", data = data$recovery_rate, marker = list(enable = FALSE)) %>%
            hc_add_series(name = "New Case Rate", type = "line", data = data$rate_of_increase, marker = list(enable = FALSE)) %>%
            hc_plotOptions(column=list(stacking='normal')) %>%
            hc_add_theme(hc_theme_tufte())
        #hc_colors(viridis(4))
        
        hc
        
    })
    
    output$map <- renderHighchart({
        data <- df_latest %>% 
                group_by(Country.Region) %>%
                summarise(confirmed = sum(confirmed), dead = sum(dead), recovered = sum(recovered))  %>% ungroup() %>%
                mutate(desc = paste("Country", Country.Region, "</br>", 
                                    "Confirmed", confirmed, "</br>", 
                                    "Dead", dead, "</br>", 
                                    "Recovered", recovered), Country.Region = ifelse(Country.Region == "US", "United States of America", Country.Region))        
        
        hcmap("custom/world-palestine-highres",
                     data = data, value = "confirmed",
                     joinBy = c("name", "Country.Region"),
                     dataLabels = list(enabled = TRUE, format = '{point.name}'),
                     borderColor = "#FAFAFA", borderWidth = 0.1,
                     tooltip = list(valueDecimals = 0))
    })

    #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
    output$activeCases <- renderValueBox({
        t <- df_info()
        valueBox(format(t$confirmed, big.mark=","), "Active Cases", icon = icon("fas fa-ambulance"), color = "orange")
    })
    
    output$recovered <- renderValueBox({
        t <- df_info()
        valueBox(format(t$recovered, big.mark=","), "Recovered", icon = icon("far fa-grin"), color = "green")
    })
    
    
    output$death <- renderValueBox({
        t <- df_info()
        valueBox(format(t$dead, big.mark=","), "Dead", icon = icon("fas fa-procedures"), color = "red")
    })
    
    output$newcases <- renderValueBox({
        t <- df_info()
        valueBox(format(t$new_case, big.mark=","), "New Cases", icon = icon("fas fa-lungs-virus"), color = "purple")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

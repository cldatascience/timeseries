#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(ggthemes)
dailymet <- read.csv(
  file="Met_HARV_Daily_2009_2011.csv",
  stringsAsFactors = FALSE)

dailymet$date <- as.Date(dailymet$date)

monthlymet <-read.csv(
  file="Temp_HARV_Monthly_09_11.csv",
  stringsAsFactors=FALSE
)

monthlymet$date <- as.Date(monthlymet$datetime)

ui <- navbarPage("NEON Time Series",
                 theme = shinytheme("flatly"),
                 tabPanel("Daily",
                          sidebarLayout(
                            sidebarPanel(
                              dateRangeInput("daterange1", "Date range:",
                                             start = "2009-01-01",
                                             end   = "2011-12-31",
                                             min = "2009-01-01",
                                             max   = "2011-12-31"),
                              checkboxInput("smooth1", "Plot smoothed data", 
                                            value = FALSE, width = NULL),
                              h5("This plot is an expanded, interactive version of this\n
                                 National Ecological Observatory Network tutorial:"),
                              a(href="http://neondataskills.org/R/time-series-plot-ggplot/", 
                                "NEON Data Skills")
                            ),
                            mainPanel(
                              plotOutput("plot")
                            )
                          )
                 ),
                 tabPanel("Montly",
                          sidebarLayout(
                            sidebarPanel(
                              dateRangeInput("daterange2", "Date range:",
                                             start = "2009-01-01",
                                             end   = "2011-12-31",
                                             min = "2009-01-01",
                                             max   = "2011-12-31"),
                              h5("This plot is expanded, interactive version of this\n
                                 National Ecological Observatory Network tutorial:"),
                              a(href="http://neondataskills.org/R/time-series-plot-ggplot/", 
                                "NEON Data Skills")
                            ),
                            mainPanel(
                              plotOutput("plotmonthly")
                            )
                          )
                 ),
                 navbarMenu("Yearly",
                            tabPanel("Facets",
                                     plotOutput("facets")
                            ))
                 
)


server <- function(input, output, session) {
  output$plot <- renderPlot({
    startTime <- as.Date(input$daterange1[1])
    endTime <- as.Date(input$daterange1[2])
    startend <- c(startTime,endTime)
    
    plot1 <- ggplot(dailymet,aes(date,airt)) + 
      geom_point() +
      ggtitle("Air Temperature 2009-2011\nNEON Harvard Forest Field Site") +
      xlab("Date") + ylab("Air Temperature (C)") +
      (scale_x_date(limits=startend,
                    breaks=date_breaks("6 months"),
                    labels=date_format("%b %y"))) +
      theme_economist()
    if (input$smooth1)
      plot1 <- plot1 + geom_smooth()
    plot1
  })
  
  output$plotmonthly <- renderPlot({
    startTime <- as.Date(input$daterange2[1])
    endTime <- as.Date(input$daterange2[2])
    startend <- c(startTime,endTime)
    
    ggplot(monthlymet,aes(date,mean_airt)) + 
      geom_point() +
      ggtitle("Mean Air Temperature 2009-2011\nNEON Harvard Forest Field Site") +
      xlab("Date") + ylab("Mean Air Temperature (C)") +
      (scale_x_date(limits=startend,
                    breaks=date_breaks("6 months"),
                    labels=date_format("%b %y"))) +
      theme_economist()
  })
  
  output$facets <- renderPlot({
    ggplot(dailymet, aes(jd, airt)) +
      geom_point() +
      ggtitle("Air Temperature\n NEON Harvard Forest Field Site") +
      xlab("Julian Day") + ylab("Temperature (C)") +
      theme(plot.title = element_text(lineheight=.8, face="bold",
                                      size = 20)) +
      theme_economist() +
      facet_grid(. ~ year)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


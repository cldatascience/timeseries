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
library(fpp2)

dailymet <- read.csv(
  file="data/dailymet.csv",
  stringsAsFactors = FALSE)

dailymet$date <- as.Date(dailymet$date)
dailymet$year <- year(dailymet$date)
dailymet$month<- month(dailymet$date)

dsn_pred <- read.csv(
  file="data/dsn_pred.csv",
  stringsAsFactors = FALSE)
dsn_pred$date <- as.Date(dsn_pred$date)

ui <- navbarPage("NEON Time Series",
                 theme = shinytheme("flatly"),
                 tabPanel("Daily air temperature",
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
                 navbarMenu("Facets",
                            tabPanel("Temperature by year",
                                     plotOutput("yearfacets")
                            ),
                            tabPanel("Yearly mean temperature",
                                     plotOutput("yearlymean")
                            ),
                            tabPanel("Temperature by month",
                                     plotOutput("monthfacets")
                            ),tabPanel("Monthly mean temperature",
                                       plotOutput("monthlymean")
                            )
                 ),
                 tabPanel("Forecast",
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
                 )
                 
                 
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

    ggplot(dsn_pred,aes(date,airt,color=status)) + 
      geom_point() +
      ggtitle("Forecasts using Seasonal Naive Model\n") +
      xlab("Date") + ylab("Mean Air Temperature (C)") +
      (scale_x_date(#limits=startend,
                    breaks=date_breaks("6 months"),
                    labels=date_format("%b %y"))) +
      theme_economist()
  })
  
  # output$plotmonthly <- renderPlot({
  #   startTime <- as.Date(input$daterange2[1])
  #   endTime <- as.Date(input$daterange2[2])
  #   startend <- c(startTime,endTime)
  #   
  #   ggplot(monthlymet,aes(date,mean_airt)) + 
  #     geom_point() +
  #     ggtitle("Mean Air Temperature 2009-2011\nNEON Harvard Forest Field Site") +
  #     xlab("Date") + ylab("Mean Air Temperature (C)") +
  #     (scale_x_date(limits=startend,
  #                   breaks=date_breaks("6 months"),
  #                   labels=date_format("%b %y"))) +
  #     theme_economist()
  # })
  
  output$yearfacets <- renderPlot({
    ggplot(dailymet, aes(jd, airt)) +
      geom_point() +
      ggtitle("Air Temperature by Year\nNEON Harvard Forest Field Site") +
      xlab("Julian Day") + ylab("Temperature (C)") +
      theme(plot.title = element_text(lineheight=.8, face="bold",
                                      size = 20)) +
      theme_economist() +
      facet_grid(. ~ year)
  })
  
  output$monthfacets <- renderPlot({
    ggplot(dailymet, aes(jd, airt)) +
      geom_point() +
      ggtitle("Air Temperature by Month\nNEON Harvard Forest Field Site") +
      xlab("Julian Day") + ylab("Temperature (C)") +
      theme(plot.title = element_text(lineheight=.8, face="bold",
                                      size = 20)) +
      theme_economist() +
      facet_grid(. ~ month)
  })
  
  output$yearlymean <- renderPlot({
    ggplot(dailymet, aes(as.factor(year), airt)) +
      geom_boxplot() +
      ggtitle("Yearly Mean Air Temperature\nNEON Harvard Forest Field Site") +
      xlab("Year") + ylab("Temperature (C)") +
      theme(plot.title = element_text(lineheight=.8, face="bold",
                                      size = 20)) +
      theme_economist()
  })
  
  output$monthlymean <- renderPlot({
    ggplot(dailymet, aes(as.factor(month), airt)) +
      geom_boxplot() +
      ggtitle("Monthly Mean Air Temperature\nNEON Harvard Forest Field Site") +
      xlab("Month") + ylab("Temperature (C)") +
      theme(plot.title = element_text(lineheight=.8, face="bold",
                                      size = 20)) +
      theme_economist()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


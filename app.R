#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



require(tidyverse)
require(shiny)
require(tsibble)
require(tsibbledata)
require(lubridate)
rm(list=ls())

#Initial read in of data
period1 <- read_csv('stock_period1.csv')
colnames(period1)[1] <- 'Period'
period1 <- period1%>%
  column_to_rownames('Period')


period2 <- read_csv('stock_period2.csv')
colnames(period2)[1] <- 'Period'
period2 <- period2%>%
  column_to_rownames('Period')

tickers <- colnames(period1)


#Creates a time series ggplot.
#The ifs determine whether the trend is daily, weekly, or monthly
time.series <- function(data,time = "Daily") {
  longer <- data %>%
    rownames_to_column(var = 'Period') %>%
    mutate(Period = as.Date(Period)) %>%
    pivot_longer(tickers, names_to = "Ticker", values_to = "Price")
  
  if (time == "Daily") {
    newdata <- longer
  }
  if (time == "Weekly") {
    newdata <- longer %>%
      group_by(Period = floor_date(Period, "week"),Ticker) %>%
      summarize(Price = mean(Price))
  }
  if (time == "Monthly") {
    newdata <- longer %>%
      group_by(Period = floor_date(Period, "month"),Ticker) %>%
      summarize(Price = mean(Price))
  }
  p1 = ggplot(newdata) +
    geom_line(aes(x = Period, y = Price, color = Ticker)) +
    labs(
      x=paste0("Time (",time,")"),
      y="Price",
      title=paste0(time," Price Trend")
    )
  return(p1)
}
#print(time.series(data = period1, time = "Monthly"))

#period1 %>%
#    rownames_to_column('Period') %>%
#  mutate(Period = as.Date(Period)) %>%
#     pivot_longer(tickers, names_to = "Ticker", values_to = "Price")

#Puts period data into data frame for total returns
totalr <- function(data) {
  total_returns_data <- data.frame(tickers,
                                   total_returns = as.double(((tail(data,1)-head(data,1))/head(data,1))[1,]))
  return(total_returns_data)
}

#Generates a bar plot for Total returns by ticker
barp <- function(data) {
  newdata <- totalr(data)
  p1 <- ggplot(newdata) +
    geom_bar(aes(x = total_returns,
                 y = reorder(tickers,total_returns)),
             stat = "identity") +
    scale_x_continuous(expand = c(0, 0, 0.1, 0.1)) +
    labs(
      x = "Return",
      y = "Ticker",
      title = "Total Returns"
    )
  return(p1)
}
#print(barp(period1))

#Generates a boxplot of prices for each ticker
boxes <- function(data) {
  longer <- data %>%
    pivot_longer(tickers, names_to = "Ticker", values_to = "Price")
  
  p1 <- ggplot(longer) +
    geom_boxplot(aes(x = Price, y = reorder(Ticker,Price,sd))) +
    labs(
      x = "Prices",
      y = "Ticker",
      title = "Ticker Volatility"
    )
  return(p1)
}
#print(boxes(period1))

#Generates portfolios
portfolio_generator <- function(data,num_of_portfolios) {
  portfolio = data.frame()
  portfolio$returns <- double()
  portfolio$sd <- double()
  portfolio$`returns/sd` <- double()
  
  weights_array = list()
  
  total_returns = as.double(((tail(data,1)-head(data,1))/head(data,1))[1,])
  num_of_stocks = ncol(data)
  stock_returns = lead(data,1)/data-1
  
  for (i in 1:num_of_portfolios){
    rnd_nums = runif(ncol(data))
    weights = rnd_nums/sum(rnd_nums)
    
    weights_array <- append(weights_array,weights)
    portfolio[i,1] <- sum(total_returns*weights)
    portfolio_returns <- stock_returns*t(weights)
    portfolio[i,2] <- sd(na.omit(as.double(rowSums(portfolio_returns))))
    portfolio[i,3] <- as.double(portfolio[i,1])/as.double(portfolio[i,2])
  }
  return(portfolio)
}
#print(portfolio_generator(period1,10))

#Identifies observations with min risk and max return/risk
#Then it gives a ggplot of the portfolio
frontier <- function(data) {
  min.risk <- data[(data$sd == min(data$sd)),]
  max.return.risk <- data[(data$`returns/sd` == max(data$`returns/sd`)),]
  p1 <- ggplot(data) +
    geom_point(aes(x=sd,y=returns),pch=19) +
    geom_point(data = min.risk,aes(x=sd,y=returns), col = "red",pch=17,cex=3) +
    geom_point(data = max.return.risk,aes(x=sd,y=returns),
               col="blue",pch=15,cex=3) +
    labs(
      x="Portfolio Standard Deviation",
      y="Portfolio Returns",
      title="Portfolio Optimization Based on Efficient Frontier"
    )
  #scale_color_manual(name = "hello", breaks = c("Random Portfolios",
  #"Minimum Risk Portfolio","Max Sharpe Portfolio"),
  #values = c("Random Portfolios"="black",
  #"Minimum Risk Portfolio"="red",
  #"Max Sharpe Portfolio"="blue"))
  
  return(p1)
}
#print(frontier(portfolio_generator(period1,1000)))





# Shiny:

## User Interface


time.options = c("Daily","Weekly","Monthly")
sample.options = c(5,10,20,50,100,200,500,1000)
period.options = c("Period 1","Period 2")

ui <- fluidPage(
  
  titlePanel("STAT 479 Project Milestone 3"),
  selectInput("period","Period",period.options),
  selectInput("time","Trend Type",time.options),
  selectInput("sample","Number of Portfolios",sample.options),
  textOutput("debug"),
  plotOutput("trendplot"),
  plotOutput("bar"),
  plotOutput("box"),
  plotOutput("ports"),
  
)

server <- function(input,output) {
  dat <- reactive({
    return(if ('Period 1' == "Period 1") period1 else period2)
  })
  
  output$trendplot <- renderPlot({
    time.series(dat(),input$time)
  })
  
  output$bar <- renderPlot(barp(dat()))
  
  output$box <- renderPlot(boxes(dat()))
  
  output$ports <- renderPlot({
    frontier(portfolio_generator(dat(),input$sample))
  })
  
  output$debug <- renderText({
    paste0(as.list(input$period))
  })
  
}


app <- shinyApp(ui = ui, server = server)

app

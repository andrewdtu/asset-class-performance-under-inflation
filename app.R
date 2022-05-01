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
require(ggthemes)
require(shinythemes)
require(magrittr)
rm(list=ls())

#Initial read in of data
read_stock_data <- function(filename){
  main_data <- read_csv(filename)
  
  colnames(main_data)[1] <- 'col1'
  stock_data <- main_data%>%
    column_to_rownames('col1')%>%
    select(-'SPY')%>%
    mutate_all(scale, center = FALSE, scale = FALSE)%>%
    setNames(sectors$sector)
  
  return(stock_data)
}

read_spx_data <- function(filename){
  main_data <- read_csv(filename)
  
  colnames(main_data)[1] <- 'col1'
  spx_data <- main_data%>%
    column_to_rownames('col1')%>%
    select('SPY')%>%
      
    mutate_all(scale, center = FALSE, scale = FALSE)
  
  return(spx_data)
}

sectors <-read_csv('ticker_to_sector.csv')

period1 <- read_stock_data('stock_period1.csv')
period2 <- read_stock_data('stock_period2.csv')
period1spx <- read_spx_data('stock_period1.csv')
period2spx <- read_spx_data('stock_period2.csv')
tickers <- colnames(period1)

return_text <- scan('returns.txt', character(), sep = '\n')
summary_text <- scan('summary.txt', character(), sep = '\n')

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
      y="Price scaled proportionately",
      title=paste0(time," Price Trend")
    )+
    scale_color_tableau('Tableau 20')+
    theme(legend.position="bottom")+
    guides(colour = guide_legend(nrow = 2,override.aes = list(size = 10)))
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
                 y = reorder(tickers,total_returns),
                 fill = tickers),
             stat = "identity",
             show.legend = FALSE) +
    scale_x_continuous(expand = c(0, 0, 0.1, 0.1)) +
    labs(
      x = "Return",
      y = "Ticker",
      title = "Total Returns"
    )+
    scale_fill_tableau('Tableau 20')
  return(p1)
}
#print(barp(period1))

#Generates a boxplot of prices for each ticker
boxes <- function(data) {
  longer <- data %>%
    pivot_longer(tickers, names_to = "Ticker", values_to = "Price")
  
  p1 <- ggplot(longer,) +
    geom_boxplot(aes(x = Price, y = reorder(Ticker,Price,sd), fill = Ticker),show.legend = FALSE) +
    labs(
      x = "Prices",
      y = "Ticker",
      title = "Ticker Volatility"
    )+
    #xlim(0,100)
    scale_fill_tableau('Tableau 20')
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
    
    weights_array <- append(weights_array,list(weights))
    portfolio[i,1] <- sum(total_returns*weights)
    portfolio_returns <- stock_returns*t(weights)
    portfolio[i,2] <- sd(na.omit(as.double(rowSums(portfolio_returns))))
    portfolio[i,3] <- as.double(portfolio[i,1])/as.double(portfolio[i,2])
    
  }
  portfolio$weights <- weights_array
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



# function to plot sp500 return and highest sharpe ratio portfolio return
portfolio_linreg <- function(portfolios, market, spx){
  max.return.risk <- portfolios[(portfolios$`returns/sd` == max(portfolios$`returns/sd`)),]
  weights <- max.return.risk[[4]][[1]]
  
  
  return_data <- lead(market,1)/market-1
  spx_return <- lead(spx,1)/spx-1
  #print(max.return.risk)
  market$returns <- as.matrix(return_data) %*% weights
  two_returns <- data.frame(market$returns,spx_return) %>%
    na.omit()
  
  fit <- lm(SPY~market.returns, data=two_returns)
  
  m <- lm(SPY~market.returns, data=two_returns);
  eq <- substitute(italic(y) == b %.% italic(x)+a*"; "~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 3),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  
  
  fit_label<- as.expression(eq)
  
  #print(two_returns)
  p1 = ggplot(two_returns, aes(two_returns[[1]],two_returns[[2]]))+
    geom_point()+
    geom_line(data = fortify(fit), aes(x = market.returns, y = .fitted), color = 'red')+
    geom_text(x = -0.02, y = 0.02, label = fit_label, parse = TRUE, color = 'red')+
    labs(
      x='Portfolio return',
      y='SP500 return',
      title='Linear Regression of highest Sharpe ratio portfolio and SP500 return'
    )
  return(p1)
}






# Shiny:

## User Interface




time.options = c("Daily","Weekly","Monthly")
sample.options = c(200,1000,5000,10000)
period.options = c("Period 1","Period 2")

ui <- fluidPage(
  
  titlePanel("STAT 479 Project Milestone 3"),
  # selectInput("period","Period",period.options),
  # selectInput("time","Trend Type",time.options),
  # selectInput("sample","Number of Portfolios",sample.options),
  textOutput("debug"),
  # plotOutput("trendplot"),
  # plotOutput("bar"),
  # plotOutput("box"),
  # plotOutput("ports"),
  # plotOutput('linreg'),
# 
#   sidebarLayout(
#     sidebarPanel( 
      fluidRow(
        column(4,selectInput("period","Period",period.options)),
        column(4,selectInput("time","Trend Type",time.options)),
        column(4,numericInput("sample", label = 'Number of Portfolios', value = 500)),
        
        
        
      ),
      


#    mainPanel(
       tabsetPanel(type = 'tabs',
         tabPanel("Dashboard",
                  fluidRow( 
                    plotOutput("trendplot"),
                  ),
                  fluidRow(column(4, plotOutput("box")),
                           column(4, plotOutput('linreg')),
                           column(4, plotOutput("bar")),
                  ),
                  fluidRow(plotOutput('ports'))
          ),
         tabPanel("Project Summary",
                  column(12,p(summary_text[1])),
                  column(12,p(summary_text[2])),
                  column(12,p(summary_text[3])),
         ),
         
       
         tabPanel("Returns",
                  column(12,p(return_text[1])),
                  column(12,p(return_text[2])),
                  column(12,p(return_text[3])),

          )
# #         tabPanel("Tab 4", tableOutput("table"))
#         
       )
    )
   

    

server <- function(input,output) {
  dat <- reactive({
    return(if (input$period == "Period 1") period1 else period2)
  })
  portfolios <- reactive({
    return(portfolio_generator(dat(),input$sample))
  })
  spx_data <- reactive({
    return(if (input$period == "Period 1") period1spx else period2spx)
  })
  
  #sector_returns <-
  
  
  output$trendplot <- renderPlot({
    dat()%>%
      mutate_all(scale,center=FALSE)%>%
    time.series(input$time)
  })
  
  output$bar <- renderPlot(barp(dat()))
  
  output$box <- renderPlot(boxes(dat()))
  
  output$ports <- renderPlot({
    frontier(portfolios())
  })
  output$linreg <- renderPlot({
    portfolio_linreg(portfolios(),dat(),spx_data())
  })
  output$debug <- renderText({
    paste0()
  })

  
}



# #portfolio.return for period 1 and period 2
# # period 1: totalr(data = period1)
# portfolios<-portfolio_generator(period1,1000)
# max.return.risk <- portfolios[(portfolios$`returns/sd` == max(portfolios$`returns/sd`)),]
# weights <- max.return.risk[[4]][[1]]
# #totalr(period2)%>%
# # mutate(weighted_return =total_returns*weights)
# #totalr(period2spx)
# 
# 
# portfolio_linreg(portfolios, period1,period1spx)%>%
#   colsums()
# # period 2: totalr(data = period2)
# portfolios2<-portfolio_generator(period2,1000)
# max.return.risk <- portfolios[(portfolios$`returns/sd` == max(portfolios$`returns/sd`)),]
# weights <- max.return.risk[[4]][[1]]
# #totalr(period2)%>%
# # mutate(weighted_return =total_returns*weights)
# #totalr(period2spx)
# 
# 
# portfolio_linreg(portfolios2, period2,period2spx)%>%
#   colsums()

app <- shinyApp(ui = ui, server = server)

app

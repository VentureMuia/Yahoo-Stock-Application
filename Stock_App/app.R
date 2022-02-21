# Importing the packages
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(plotly)
library(tidyquant)
library(tidyverse)
library(dygraphs)
library(prophet)

 # Setting the theme
theme_set(theme_tq())

# stocks
st<-c("F","AAPL","SPCE","BAC","WFC","PCG","T",
      "NIO","AMC","BBD","FCEL","PLUG","HUT","AMD",
      "AA","FCX","ITUB","RIOT","EDU","AAL","FUTU",
      "MSFT","INTC","PFE","VZ")
header<-dashboardHeader(title = "Yahoo Stocks Forecast",
                        titleWidth = 800)

sidebar<-dashboardSidebar(
    sidebarMenu(id="sidebarid",
                menuItem("dash",tabName = "Dash",icon=icon("chart-line")),
                conditionalPanel(condition = "input.sidebarid=='Dash'",
                                 selectInput("stock","Select asset",selected="AAPL",
                                             choices=st),
                                 dateRangeInput("dates",
                                                label = "Choose date range of interest:", 
                                                start = Sys.Date()-1000, 
                                                end = Sys.Date()-1),
                                 radioButtons("tm","Select Frequency",selected="weekly",
                                             choices=c("daily","weekly","monthly")),
                                 sliderInput("sl","Choose steps to Forecast ",min=1,max=30,
                                             value = 20,step=5),
                                 submitButton("Apply")
                                
                                 )
                )
)

body<-dashboardBody(
    tabItems(
        tabItem(tabName = "Dash",
                box(width = 12,height = 500,
                    dygraphOutput("plt4"))
                
                )
    )
)

UI<-dashboardPage(header,sidebar,body)

server<-function(input,output,session){
    data<-reactive({
      df<-data.frame(na.omit(tq_get(input$stock,
                   from =as.Date(input$dates[1]),
                   to =as.Date(input$dates[2]),
                   periodicity = input$tm,
                   )[,c(2,6)]))
      names(df)<-c("date","close") 
      df$date<-as.Date(df$date)
      return(df)
    })
    
    output$plt4<-renderDygraph({
      df<-data()
      names(df)<-c("ds","y")
      m<-prophet(df,daily.seasonality = T)
      future <- make_future_dataframe(m, periods = input$sl)
      forecast <- predict(m, future)
      dyplot.prophet(m, forecast)
    })
    
}


shinyApp(UI,server)


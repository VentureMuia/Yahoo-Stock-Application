# Importing the packages
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(plotly)
library(tidyquant)
library(modeltime)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)
library(dygraphs)
library(forecast)
library(forecastHybrid)


 # Setting the theme
theme_set(theme_test())

# stocks
st<-c("F","AAPL","SPCE","BAC","WFC","PCG","T",
      "NIO","AMC","BBD","FCEL","PLUG","HUT","AMD",
      "AA","FCX","ITUB","RIOT","EDU","AAL","FUTU",
      "MSFT","INTC","PFE","VZ")
header<-dashboardHeader(title = "Yahoo Stocks Forecast",
                        titleWidth = 800)

sidebar<-dashboardSidebar(
    sidebarMenu(id="sidebarid",
                menuitem("About App",tabName="app",icon=icon("laptop")),
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
        tabItem(tabName = "app",
                p("This app predicts the future behaviour of the Stock using facebook time series algorithm called prophet.
                  You must select your options and click apply button. 
                  The model will be updated according to the entered value. 
                  You should install and load both libraries required packages before running locally this app in R.
                  At this point you will be able to run the app locally running runApp('ShinyApp') from the folder containing the files app.R."
                                                                     )
                ),
        tabItem(tabName = "Dash",
                box(width = 12,
                    plotlyOutput("plt1")),
                box(width = 12,
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
    
    output$plt1<-renderPlotly({
      s<-data() %>% plot_time_series(date,close)
      ggplotly(s)%>% config(displayModeBar = F)
    })
    
    
    output$plt4<-renderDygraph({
      library(prophet)
      df<-data()
      names(df)<-c("ds","y")
      m<-prophet(df,daily.seasonality = T)
      future <- make_future_dataframe(m, periods = input$sl)
      forecast <- predict(m, future)
      dyplot.prophet(m, forecast)
    })
    
}


shinyApp(UI,server)


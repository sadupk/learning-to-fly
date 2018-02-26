# Learn to Fly - Group 6 - CS424 Spring 2017
# Inspired by the sample R + Shiny example for CS 424 Spring 2018 UIC - Andy Johnson
# www.evl.uic.edu/aej/424

# Libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(plotly)
library(reshape)
library(plyr)
library(data.table)

##  "13930","Chicago, IL: Chicago O'Hare International"   
### "13232","Chicago, IL: Chicago Midway International"

#Keep all data files in the 'Data' folder!!

#Load Lookup Tables
print("Reading lookup tables")
carrier_lookup = read.csv("Data/L_CARRIER_HISTORY.csv_")
colnames(carrier_lookup) = c("Code", "carrier_name")
airport_lookup = read.csv("Data/L_AIRPORT_ID.csv")
colnames(airport_lookup) = c("Code", "airport_name")

#Load Flight data
print("Reading data tables")
portdir = read.csv("Data/L_AIRPORT_ID.csv")
Jan=read.csv("Data/Jan.csv")
Feb=read.csv("Data/Feb.csv")
Mar=read.csv("Data/Mar.csv")
Apr=read.csv("Data/Apr.csv")
May=read.csv("Data/May.csv")
June=read.csv("Data/June.csv")
July=read.csv("Data/July.csv")
Aug=read.csv("Data/Aug.csv")
Sept=read.csv("Data/Sept.csv")
Oct=read.csv("Data/Oct.csv")
Nov=read.csv("Data/Nov.csv")
Dec=read.csv("Data/Dec.csv")

#Merge flight data
Month=list(Jan,Feb,Mar,Apr,May,June,July,Aug,Sept,Oct,Nov,Dec)
#Add Month dataframe with data for the whole year
Month_df = rbindlist(Month)
Monthnames=c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG","SEPT","OCT","NOV","DEC")

#Add lookup fields
Month_with_names = lapply(Month, function(x) merge(x, carrier_lookup, by.x = "CARRIER", by.y = "Code", incomparables = NA, all.x = TRUE))
colnames(airport_lookup) = c("Code", "origin_airport")
Month_with_names = lapply(Month_with_names, function(x) merge(x, airport_lookup, by.x = "ORIGIN_AIRPORT_ID", by.y = "Code", incomparables = NA, all.x = TRUE))
colnames(airport_lookup) = c("Code", "dest_airport")
Month_with_names = lapply(Month_with_names, function(x) merge(x, airport_lookup, by.x = "DEST_AIRPORT_ID", by.y = "Code", incomparables = NA, all.x = TRUE))


#days=c("mon","tues","wed","thur","fri","sat","sun")
#Jan$day=c(rep(days,length(Jan[[1]])/7),days[c(1:(length(Jan[[1]])%%7))])



#departures=Month[Month$ORIGIN_AIRPORT_ID=="13930" | Month$ORIGIN_AIRPORT_ID=="13232",]
#arrivals=Month[Month$DEST_AIRPORT_ID=="13930" | Month$DEST_AIRPORT_ID=="13232",]
#airportsdepart=data.frame(table(departures$CARRIER))
#airportsarrival=data.frame(table(arrivals$CARRIER))
#airporttimes=data.frame(ID=airportsdepart[[1]],departing=airportsdepart[[2]],arrivals=airportsarrival[[2]])

# assume all of the tsv files in this directory are data of the same kind that I want to visualize


ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2018 Example Dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    fluidRow(
      selectInput("Airport", "Airport", c("Chicago O'Hare", "Chicago Midway","Both")),
      selectInput("month", "Month", c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG","SEPT","OCT","NOV","DEC")),
      selectInput("timeframe", "timeframe", c("1-24","AM-PM"))
    ),
    
    fluidRow(
      tabPanel("AirlineFlightPlot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("AirlineFlightPlot",width="450px",height="450px")) ),
      tabPanel("AirlineFlightTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("AirlineFlightTable"))  ),
      tabPanel("HourlyFlights", box(title = "Airline Hourly Flights", solidHeader = TRUE, status = "primary", width = 8, plotOutput("HourlyFlights"))  ),
      tabPanel("HourlyTable", box(title = "Airline Hourly Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("HourlyTable"))  )
      
    ),
    
    fluidRow(
      tabPanel("Arrival Flights",box( title = "Arrival Flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("ArrivalFlightsPlot",width="450px",height="450px")) ),
      tabPanel("Arrival Flights Table", box(title = "Arrival Flights Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("ArrivalFlightsTable"))  )
      
    ),
    
    
    fluidRow(
      tabPanel("Depart Flights",box( title = "Depart Flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("DepartFlightsPlot",width="450px",height="450px")) ),
      tabPanel("Depart Flights Table", box(title = "Depart Flights Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("DepartFlightsTable"))  )
      
    ),
    fluidRow(
      tabPanel("Weekly Flights",box( title = "Weekly Flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("WeeklyFlightsPlot",width="750px",height="750px")) )
    ),
      fluidRow(
        tabPanel("Weekly Flights",box( title = "Weekly Flights", solidHeader = TRUE, status = "primary", width = 10, dataTableOutput("WeeklyFlightsTable",width="750px",height="750px")) )
      ),
      fluidRow(
        tabPanel("Arrival_Delays",box( title = "Arrival_Delays", solidHeader = TRUE, status = "primary", width = 10, plotOutput("ArrivalDelays",width="750px",height="750px")) ),
        tabPanel("Depart_Delays",box( title = "Depart_Delays", solidHeader = TRUE, status = "primary", width = 10, plotOutput("DepartDelays",width="750px",height="750px")) ),
        tabPanel("DepartDelayTable",box( title = "DepartDelayTable", solidHeader = TRUE, status = "primary", width = 10, dataTableOutput("DepartDelayTable",width="750px",height="750px")) ),
        tabPanel("ArrivalDelayTable",box( title = "ArrivalDelayTable", solidHeader = TRUE, status = "primary", width = 10, dataTableOutput("ArrivalDelayTable",width="750px",height="750px")) )
        
      ),
    fluidRow(
      tabPanel("Top 15 Destinations",box( title = "Top 15 Destinations", solidHeader = TRUE, status = "primary", width = 10, plotOutput("top_15_dest_Plot",width="750px",height="750px")) )
    ),
    fluidRow(
      tabPanel("Delay Causes",box( title = "Delay Causes", solidHeader = TRUE, status = "primary", width = 10, plotOutput("delay_Plot",width="750px",height="750px")) )
    )
    
    
  )
)

server <- function(input, output) {
  
  theme_set(theme_grey(base_size = 17)) 
  ######Helper functions delcared here
  getarrivals<- function(Airline)
  {
    
    arrtimes=Airline$ARR_TIME
    arrtimes <- as.character(unlist(Airline$ARR_TIME))
    arrivals=list()
    
    
    arr=arrtimes[nchar(arrtimes)<3 & !is.na(arrtimes)]
    arrivals[1]=length(arr)
    
    
    
    
    for (hour in 1:9)
    {
      h=toString(hour)
      
      arr=arrtimes[startsWith(arrtimes,h) & nchar(arrtimes)==3 & !is.na(arrtimes)]
      arrivals[hour+1]=length(arr)
      
    }
    
    
    for (hour in 10:24)
    {
      h=toString(hour)
      
      arr=arrtimes[startsWith(arrtimes,h) & nchar(arrtimes)==4 & !is.na(arrtimes)]
      arrivals[hour+1]=length(arr)
      
      
    }
    
    arrivals=unlist(arrivals)
    
    arrivals[1]=arrivals[1]+arrivals[25]
    
    
    arrivals=arrivals[c(1:24)]
    
    return(arrivals)
  }
  
  
  getdeps<- function(Airline)
  {
    
    dep=Airline$DEP_TIME
    deptimes <- as.character(unlist(Airline$DEP_TIME))
    departures=list()
    
    
    
    dep=deptimes[nchar(deptimes)<3 & !is.na(deptimes)]
    departures[1]=length(dep)
    
    
    
    
    for (hour in 1:9)
    {
      h=toString(hour)
      
      
      dep=deptimes[startsWith(deptimes,h) & nchar(deptimes)==3 & !is.na(deptimes)]
      departures[hour+1]=length(dep)
    }
    
    
    for (hour in 10:24)
    {
      h=toString(hour)
      
      
      dep=deptimes[startsWith(deptimes,h) & nchar(deptimes)==4 & !is.na(deptimes)]
      departures[hour+1]=length(dep)
    }
    
    departures=unlist(departures)
    departures[1]=departures[1]+departures[25]
    departures=departures[c(1:24)]
    
    return(departures)
  }
  
  
  output$AirlineFlightPlot <- renderPlot({   ###  Need to remove VX--columbian airlines, which shares a name with other airlines.
    
    
    if(input$Airport=="Both")
    {
      ports=c("13232", "13930")   #####May want to reconsider this
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
      airportsdepart=data.frame(table(departures$CARRIER))
      airportsarrival=data.frame(table(arrivals$CARRIER))
      airporttimes=data.frame(ID=airportsdepart[[1]],departing=airportsdepart[[2]],arrivals=airportsarrival[[2]])
      melted1=melt(airporttimes, id="ID")
      
      
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[2],]
      airportsdepart=data.frame(table(departures$CARRIER))
      airportsarrival=data.frame(table(arrivals$CARRIER))
      airporttimes=data.frame(ID=airportsdepart[[1]],departing2=airportsdepart[[2]],arrivals2=airportsarrival[[2]])
      melted2=melt(airporttimes, id="ID")
      
      
      melted=rbind(melted1,melted2) 
      ggplot(data=melted, aes(x=ID, y=value)) + geom_bar(stat = "identity",aes(fill=melted$variable), position = "dodge")
    }  
    
    else
    {
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
      arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,]
      airportsdepart=data.frame(table(departures$CARRIER))
      airportsarrival=data.frame(table(arrivals$CARRIER))
      airporttimes=data.frame(ID=airportsdepart[[1]],departing=airportsdepart[[2]],arrivals=airportsarrival[[2]])
      melted=melt(airporttimes, id="ID")
      ggplot(data=melted, aes(x=ID, y=value)) + geom_bar(stat = "identity",aes(fill=melted$variable), position = "dodge") 
    }
  })
  
  output$AirlineFlightTable <- DT::renderDataTable(
    
    
    
    DT::datatable({ 
      
      
      if(input$Airport=="Both")
      {
        ports=c("13232", "13930")
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        
        departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
        arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
        airportsdepart1=data.frame(table(departures$CARRIER))
        airportsarrival1=data.frame(table(arrivals$CARRIER))
        
        
        departures=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
        arrivals=Month[Month$DEST_AIRPORT_ID==ports[2],]
        airportsdepart2=data.frame(table(departures$CARRIER))
        airportsarrival2=data.frame(table(arrivals$CARRIER))
        
        airporttimes=data.frame(
          ID=airportsdepart1[[1]],
          departing1=airportsdepart1[[2]],
          arrivals1=airportsarrival1[[2]],
          departing2=airportsdepart2[[2]],
          arrivals2=airportsarrival2[[2]]
        )
      }  
      
      else
      {
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
        arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,]
        airportsdepart=data.frame(table(departures$CARRIER))
        airportsarrival=data.frame(table(arrivals$CARRIER))
        airporttimes=data.frame(ID=airportsdepart[[1]],departing=airportsdepart[[2]],arrivals=airportsarrival[[2]])
        
      }
      airporttimes
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
    )
  )
  
  output$HourlyTable <- DT::renderDataTable(
    
    
    
    DT::datatable({ 
      
      
      Month=Month[Monthnames ==input$month]
      # Month=read.csv("Jan.csv")
      Month=Month[[1]]
      #  Airline=Month[Month$ORIGIN_AIRPORT_ID=="13930",] 
      # Airline=Month[Month$ORIGIN_AIRPORT_ID==input$Airport,]
      
      
      if (input$Airport=="Both")
      {
        ports=c("13930","13232")
        
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[1],] 
        departures=getdeps(Airline)
        arrivals=getarrivals(Airline)
        times=c(1:24)
        TravelTimes=data.frame(Times=times,Arrivals=arrivals,  Departures=departures)
        
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[2],] 
        departures=getdeps(Airline)
        arrivals=getarrivals(Airline)
        times=c(1:24)
        TravelTimes2=data.frame(Times=times,Arrivals2=arrivals,  Departures2=departures)
        
        
      }  
      else
      {
        # Airline=Month[Month$ORIGIN_AIRPORT_ID==input$Airport,] ##### PROBLEM
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        Airline=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
        departures=getdeps(Airline)
        arrivals=getarrivals(Airline)
        
        times=c(1:24)
        TravelTimes=data.frame(Times=times,Arrivals=arrivals,  Departures=departures)
        
      } 
      TravelTimes
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
    )
  )
  
  
  
  output$HourlyFlights<- 
    
    renderPlot({
      
      
      Month=Month[Monthnames ==input$month]
      # Month=read.csv("Jan.csv")
      Month=Month[[1]]
      #  Airline=Month[Month$ORIGIN_AIRPORT_ID=="13930",] 
      # Airline=Month[Month$ORIGIN_AIRPORT_ID==input$Airport,]
      
      
      if (input$Airport=="Both")
      {
        ports=c("13930","13232")
        
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[1],] 
        departures=getdeps(Airline)
        arrivals=getarrivals(Airline)
        times=c(1:24)
        TravelTimes=data.frame(Times=times,Arrivals=arrivals,  Departures=departures)
        
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[2],] 
        departures=getdeps(Airline)
        arrivals=getarrivals(Airline)
        times=c(1:24)
        TravelTimes2=data.frame(Times=times,Arrivals2=arrivals,  Departures2=departures)
        
        if(input$timeframe=="1-24")
        {
          ggplot(TravelTimes, aes(x=Times))+labs(y="# Flights",x = "Times") + 
            geom_point(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
            geom_point(aes(y = TravelTimes[[3]], colour = "Departures",group=1))+
            geom_line(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
            geom_line(aes(y = TravelTimes[[3]], colour = "Departures",group=1))  +
            geom_point(aes(y = TravelTimes2[[2]], colour = "Arrivals2",group=1))+
            geom_point(aes(y = TravelTimes2[[3]], colour = "Departures2",group=1))+
            geom_line(aes(y = TravelTimes2[[2]], colour = "Arrivals2",group=1))+
            geom_line(aes(y = TravelTimes2[[3]], colour = "Departures2",group=1))
        }
        
        
        else
        {
          timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
          ggplot(TravelTimes, aes(x=Times))+labs(y="# Flights",x = "Times") +
            scale_x_discrete( name ="hour",limits=timeframe)+
            geom_point(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
            geom_point(aes(y = TravelTimes[[3]], colour = "Departures",group=1))+
            geom_line(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
            geom_line(aes(y = TravelTimes[[3]], colour = "Departures",group=1))  +
            geom_point(aes(y = TravelTimes2[[2]], colour = "Arrivals2",group=1))+
            geom_point(aes(y = TravelTimes2[[3]], colour = "Departures2",group=1))+
            geom_line(aes(y = TravelTimes2[[2]], colour = "Arrivals2",group=1))+
            geom_line(aes(y = TravelTimes2[[3]], colour = "Departures2",group=1))
        }
        
      }  
      else
      {
        # Airline=Month[Month$ORIGIN_AIRPORT_ID==input$Airport,] ##### PROBLEM
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        Airline=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
        departures=getdeps(Airline)
        arrivals=getarrivals(Airline)
        times=c(1:24)
        TravelTimes=data.frame(Times=times,Arrivals=arrivals,  Departures=departures)
        
        if(input$timeframe=="1-24")
        {
          
          ggplot(TravelTimes, aes(x=Times))+labs(y="# Flights",x = "Times") + 
            geom_point(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
            geom_point(aes(y = TravelTimes[[3]], colour = "Departures",group=1))+
            geom_line(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
            geom_line(aes(y = TravelTimes[[3]], colour = "Departures",group=1)) 
        }
        
        
        else
        {
          timeframe=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM","1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM","9PM","10PM","11PM")
          ggplot(TravelTimes, aes(x=Times))+labs(y="# Flights",x = "Times") +
            scale_x_discrete( name ="hour",limits=timeframe)+
            geom_point(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
            geom_point(aes(y = TravelTimes[[3]], colour = "Departures",group=1))+
            geom_line(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
            geom_line(aes(y = TravelTimes[[3]], colour = "Departures",group=1))
          
        }
        
      } 
      
      
    })
  
  
  
  output$ArrivalFlightsPlot <- renderPlot({
    if(input$Airport=="Both")
    {
      
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      ports=c("13232", "13930")
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
      go_to=data.frame(table(departures$DEST_AIRPORT_ID))
      ind=order(go_to[[2]],decreasing = T)
      indtop=ind[1:15]
      go_top=go_to[indtop,]
      go_top=go_top[complete.cases(go_top),]
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
      go_to=data.frame(table(departures$DEST_AIRPORT_ID))
      ind=order(go_to[[2]],decreasing = T)
      indtop=ind[1:15]
      go_top2=go_to[indtop,]
      go_top2=go_top2[complete.cases(go_top2),]
      go_tos=merge(go_top,go_top2, by="Var1",all=TRUE)
      go_tos[is.na(go_tos)] = 0
      go_tos=data.frame(ID=go_tos[[1]],num1=go_tos[[2]],num2=go_tos[[3]])
      melted=melt(go_tos, id='ID')
      ggplot(data=melted, aes(x=ID, y=value)) + geom_bar(stat = "identity",aes(fill=melted$variable),position = "dodge")
    }
    ### Be a bit a wary of this else clause, code  inside it is okay though
    else{
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]
      go_to=data.frame(table(departures$DEST_AIRPORT_ID))
      ind=order(go_to[[2]],decreasing = T)
      indtop=ind[1:15]
      go_top=go_to[indtop,]
      go_top=go_top[complete.cases(go_top),]
      ggplot(data=go_top, aes(x=Var1, y=Freq)) + geom_bar(stat = "identity",position = "dodge")
      
    }
  })
  
  output$ArrivalFlightsTable <- DT::renderDataTable(
    
    
    DT::datatable({
      if(input$Airport=="Both")    #####
      {
        
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        ports=c("13232", "13930")
        departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
        go_to=data.frame(table(departures$DEST_AIRPORT_ID))
        ind=order(go_to[[2]],decreasing = T)
        indtop=ind[1:15]
        go_top=go_to[indtop,]
        go_top=go_top[complete.cases(go_top),]
        
        departures=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
        go_to=data.frame(table(departures$DEST_AIRPORT_ID))
        ind=order(go_to[[2]],decreasing = T)
        indtop=ind[1:15]
        go_top2=go_to[indtop,]
        go_top2=go_top2[complete.cases(go_top2),]
        
        go_tos=merge(go_top,go_top2, by="Var1",all=TRUE)
        go_tos[is.na(go_tos)] = 0
        go_tos=data.frame(ID=go_tos[[1]],num1=go_tos[[2]],num2=go_tos[[3]])
        
      }
      ### Be a bit a wary of this else clause, code  inside code is statistically okay though
      else{
        
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        ports=c("13232", "13930")
        departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,] #####
        go_to=data.frame(table(departures$DEST_AIRPORT_ID))
        ind=order(go_to[[2]],decreasing = T)
        indtop=ind[1:15]
        go_top=go_to[indtop,]
        go_top=go_top[complete.cases(go_top),]
        go_tos=go_top
        
      }
      go_tos
    },  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) )
  )
  
  
  
  
  output$DepartFlightsPlot <- renderPlot({
    
    if(input$Airport=="Both")
    {
      ports=c("13232", "13930")
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
      come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
      ind=order(come_from[[2]],decreasing = T)
      indtop=ind[1:15]
      come_top=come_from[indtop,]
      come_top=come_top[complete.cases(come_top),]
      
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[2],]
      come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
      ind=order(come_from[[2]],decreasing = T)
      indtop=ind[1:15]
      come_top2=come_from[indtop,]
      come_top2=come_top2[complete.cases(come_top2),]
      
      come_froms=merge(come_top,come_top2, by="Var1",all=TRUE)
      come_froms[is.na(come_froms)] = 0
      come_froms=data.frame(ID=come_froms[[1]],num1=come_froms[[2]],num2=come_froms[[3]])
      
      melted=melt(come_froms, id='ID')
      ggplot(data=melted, aes(x=ID, y=value)) + geom_bar(stat = "identity",aes(fill=melted$variable),position = "dodge")
    }
    
    else {
      
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,] 
      come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
      ind=order(come_from[[2]],decreasing = T)
      indtop=ind[1:15]
      come_top=come_from[indtop,]
      come_top=come_top[complete.cases(come_top),]
      ggplot(data=come_top, aes(x=Var1, y=Freq)) + geom_bar(stat = "identity",position = "dodge")
      
    }
    
    
  })
  
  output$DepartFlightsTable <- DT::renderDataTable(
    
    
    DT::datatable({ 
      if(input$Airport=="Both")
      {
        ports=c("13232", "13930")
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        
        arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
        come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
        ind=order(come_from[[2]],decreasing = T)
        indtop=ind[1:15]
        come_top=come_from[indtop,]
        come_top=come_top[complete.cases(come_top),]
        
        arrivals=Month[Month$DEST_AIRPORT_ID==ports[2],]
        come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
        ind=order(come_from[[2]],decreasing = T)
        indtop=ind[1:15]
        come_top2=come_from[indtop,]
        come_top2=come_top2[complete.cases(come_top2),]
        
        come_froms=merge(come_top,come_top2, by="Var1",all=TRUE)
        come_froms[is.na(come_froms)] = 0
        come_froms=data.frame(ID=come_froms[[1]],num1=come_froms[[2]],num2=come_froms[[3]])
        
        
      }
      
      else {
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        
        Month=Month[Monthnames ==input$month]
        Month=Month[[1]]
        arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,] ###
        come_from=data.frame(table(arrivals$ORIGIN_AIRPORT_ID))
        ind=order(come_from[[2]],decreasing = T)
        indtop=ind[1:15]
        come_top=come_from[indtop,]
        come_top=come_top[complete.cases(come_top),]
        come_froms=come_top
        
      }
      come_froms
      
      
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
    )
  )
  
  output$WeeklyFlightsPlot <- renderPlot({
    
    if (input$Airport=="Both")
    {
      ports=c("13232", "13930")
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
      
      arr_day1=data.frame(table(arrivals$DAY_OF_WEEK))
      dep_day1=data.frame(table(departures$DAY_OF_WEEK))
      
      
      arrivals2=Month[Month$DEST_AIRPORT_ID==ports[2],]
      departures2=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
      
      arr_day2=data.frame(table(arrivals2$DAY_OF_WEEK))
      dep_day2=data.frame(table(departures2$DAY_OF_WEEK))
      
      daily_data=data.frame(ID=arr_day1[[1]],arr1=arr_day1[[2]],arr2=arr_day2[[2]],dep1=dep_day1[[2]],dep2=arr_day2[[2]])
      melted=melt(daily_data, id='ID')
      ggplot(melted, aes(x=ID, y=value,  color=variable, group=variable))+ geom_line()
    }
    
    
    else
    {
      
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,]   ####
      departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]  ###
      
      arr_day1=data.frame(table(arrivals$DAY_OF_WEEK))
      dep_day1=data.frame(table(departures$DAY_OF_WEEK))
      
      
      daily_data=data.frame(ID=arr_day1[[1]],arr1=arr_day1[[2]],dep1=dep_day1[[2]])
      melted=melt(daily_data, id='ID')
      ggplot(melted, aes(x=ID, y=value,  color=variable, group=variable))+ geom_line()
      
      
    }  
    
  })
  
  
output$WeeklyFlightsTable <- DT::renderDataTable(
  
  
  DT::datatable({ 
    if (input$Airport=="Both")
    {
      ports=c("13232", "13930")
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      arrivals=Month[Month$DEST_AIRPORT_ID==ports[1],]
      departures=Month[Month$ORIGIN_AIRPORT_ID==ports[1],]
      
      arr_day1=data.frame(table(arrivals$DAY_OF_WEEK))
      dep_day1=data.frame(table(departures$DAY_OF_WEEK))
      
      
      arrivals2=Month[Month$DEST_AIRPORT_ID==ports[2],]
      departures2=Month[Month$ORIGIN_AIRPORT_ID==ports[2],]
      
      arr_day2=data.frame(table(arrivals2$DAY_OF_WEEK))
      dep_day2=data.frame(table(departures2$DAY_OF_WEEK))
      
      daily_data=data.frame(ID=arr_day1[[1]],arr1=arr_day1[[2]],arr2=arr_day2[[2]],dep1=dep_day1[[2]],dep2=arr_day2[[2]])
      
    }
    
    
    else
    {
      
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]
      arrivals=Month[Month$DEST_AIRPORT_ID==Airportname,]   ####
      departures=Month[Month$ORIGIN_AIRPORT_ID==Airportname,]  ###
      
      arr_day1=data.frame(table(arrivals$DAY_OF_WEEK))
      dep_day1=data.frame(table(departures$DAY_OF_WEEK))
      daily_data=data.frame(ID=arr_day1[[1]],arr1=arr_day1[[2]],dep1=dep_day1[[2]])
 
      
    }  
    
    daily_data
    
    
  } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
  )
)




output$ArrivalDelays <- renderPlot({
  Month=Month[Monthnames ==input$month]   ####CHANGE THIS
  #Month=read.csv("Feb.csv")
     Month=Month[[1]]                     ###CHANGE THIS
  
  
  
  
  if (input$Airport=="Both")
  {
    ports=c("13232", "13930")
    delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[1] & Month$ARR_DEL15==1 ,] 
    Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[1],] 
    delays=getarrivals(delaysdata)
    arrivals=getarrivals(Airline)
    flights=c(delays,(arrivals-delays))
    
    delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[2] & Month$ARR_DEL15==1 ,] 
    Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[2],] 
    delays=getarrivals(delaysdata)
    arrivals=getarrivals(Airline)
    flights2=c(delays,(arrivals-delays))
    
    
    ####Colors are mislabeled BUT THE CHARTS IS CORRECT!!!!
    times=rep(c(1:24),2)
    t=rep("delays",24)
    d=rep("totals",24)
    coloring=c(d,t)
    TravelTimes=data.frame(Times=times,Num1=flights,Num2=flights2)
    melted=melt(TravelTimes, id="Times")
    melted$Coloring=coloring
    ggplot(melted, aes(x=Times, y=value)) + geom_bar(stat="identity",colour="white",aes(fill=melted$Coloring))+ 
      facet_grid(~ variable)
    
  }
  
  else 
  {
    Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
    delaysdata=Month[Month$ORIGIN_AIRPORT_ID==Airportname & Month$ARR_DEL15==1 ,] 
    Airline=Month[Month$ORIGIN_AIRPORT_ID==Airportname,] 
    delays=getarrivals(delaysdata)
    arrivals=getarrivals(Airline)
    times=rep(c(1:24),2)
    t=rep("delays",24)
    d=rep("totals",24)
    coloring=c(d,t)
    flights=c(delays,(arrivals-delays))
    
    TravelTimes=data.frame(Times=times,Flights=flights,Coloring=coloring)
    ggplot(TravelTimes, aes(x=Times, y=Flights)) + geom_bar(stat="identity",   
                                                            colour="white",aes(fill=Coloring))
  }
})
  
  
  output$DepartDelays <- renderPlot({
     Month=Month[Monthnames ==input$month]   ####CHANGE THIS
    #Month=read.csv("Feb.csv")
       Month=Month[[1]]                     ###CHANGE THIS
    
    
    if (input$Airport=="Both")
    {
      ports=c("13232", "13930")
      delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[1] & Month$DEP_DEL15==1 ,] 
      Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[1],] 
      delays=getdeps(delaysdata)
      departs=getdeps(Airline)
      flights=c(delays,(departs-delays))
      
      delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[2] & Month$DEP_DEL15==1 ,] 
      Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[2],] 
      delays=getdeps(delaysdata)
      departs=getdeps(Airline)
      flights2=c(delays,(departs-delays))
      
      
      ####Colors are mislabeled BUT THE CHARTS IS CORRECT!!!!
      times=rep(c(1:24),2)
      t=rep("delays",24)
      d=rep("totals",24)
      coloring=c(d,t)
      TravelTimes=data.frame(Times=times,Num1=flights,Num2=flights2)
      melted=melt(TravelTimes, id="Times")
      melted$Coloring=coloring
      ggplot(melted, aes(x=Times, y=value)) + geom_bar(stat="identity",colour="white",aes(fill=melted$Coloring))+ 
        facet_grid(~ variable)
      
    }
    
    else 
    {
      Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
      delaysdata=Month[Month$ORIGIN_AIRPORT_ID==Airportname & Month$DEP_DEL15==1 ,] 
      Airline=Month[Month$ORIGIN_AIRPORT_ID==Airportname,] 
      delays=getdeps(delaysdata)
      departs=getdeps(Airline)
      times=rep(c(1:24),2)
      t=rep("delays",24)
      d=rep("totals",24)
      coloring=c(d,t)
      flights=c(delays,(departs-delays))
      
      TravelTimes=data.frame(Times=times,Flights=flights,Coloring=coloring)
      ggplot(TravelTimes, aes(x=Times, y=Flights)) + geom_bar(stat="identity",   
                                                              colour="white",aes(fill=Coloring))
    }
    
  })
  
  output$DepartDelayTable <- DT::renderDataTable(
    
    
    DT::datatable({ 
       Month=Month[Monthnames ==input$month]   ####CHANGE THIS
      #Month=read.csv("Feb.csv")
       Month=Month[[1]]                     ###CHANGE THIS
      
      
      if (input$Airport=="Both")
      {
        ports=c("13232", "13930")
        delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[1] & Month$DEP_DEL15==1 ,] 
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[1],] 
        delays1=getdeps(delaysdata)
        departs1=getdeps(Airline)
     
        
        delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[2] & Month$DEP_DEL15==1 ,] 
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[2],] 
        delays2=getdeps(delaysdata)
        departs2=getdeps(Airline)

        
        ####Colors are mislabeled BUT THE CHARTS IS CORRECT!!!!
        times=rep(c(1:24),2)
        t=rep("delays",24)
        d=rep("totals",24)
        coloring=c(d,t)
        TravelTimes=data.frame(Times=times,Delays1=delays1,Proportion1=100*delays1/departs1,Delays2=delays2,Proportion2=100*delays2/departs2)
        
        
      }
      
      else 
      {
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        delaysdata=Month[Month$ORIGIN_AIRPORT_ID==Airportname & Month$DEP_DEL15==1 ,] 
        Airline=Month[Month$ORIGIN_AIRPORT_ID==Airportname,] 
        delays=getdeps(delaysdata)
        departs=getdeps(Airline)
        times=rep(c(1:24),2)
        flights=c(delays, delays/departs)
        
        TravelTimes=data.frame(Times=times,Delays=delays,Proportion=100*(delays/departs))
        
      }
      
      TravelTimes
      
      
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
    )
  )




  output$ArrivalDelayTable <- DT::renderDataTable(
    
    
    DT::datatable({ 
       Month=Month[Monthnames ==input$month]   ####CHANGE THIS
      #Month=read.csv("Feb.csv")
       Month=Month[[1]]                     ###CHANGE THIS
      
      
      if (input$Airport=="Both")
      {
        ports=c("13232", "13930")
        delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[1] & Month$ARR_DEL15==1 ,] 
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[1],] 
        delays1=getarrivals(delaysdata)
        arrivals1=getarrivals(Airline)

        delaysdata=Month[Month$ORIGIN_AIRPORT_ID==ports[2] & Month$ARR_DEL15==1 ,] 
        Airline=Month[Month$ORIGIN_AIRPORT_ID==ports[2],] 
        delays2=getarrivals(delaysdata)
        arrivals2=getarrivals(Airline)

        
        ####Colors are mislabeled BUT THE CHARTS IS CORRECT!!!!
        times=rep(c(1:24),2)
        TravelTimes=data.frame(Times=times,Delays1=delays1,Proportion1=100*delays1/arrivals1,Delays2=delays2,Proportion2=100*delays2/arrivals2)
        
        
      }
      
      else 
      {
        Airportname=  portdir[grepl(input$Airport,portdir[,2]),1]
        delaysdata=Month[Month$ORIGIN_AIRPORT_ID==Airportname & Month$ARR_DEL15==1 ,] 
        Airline=Month[Month$ORIGIN_AIRPORT_ID==Airportname,] 
        delays=getarrivals(delaysdata)
        arrivals=getarrivals(Airline)
        times=rep(c(1:24),2)

        flights=c(delays,(delays/arrivals))
        
        TravelTimes=data.frame(Times=times,Delays=delays,Proportion=100*(delays/arrivals))

      }
      TravelTimes
      
      
    } ,  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE) 
    )
  )  

###################PART B BEGINS HERE  
  
  output$top_15_dest_Plot <- renderPlot({
    dest_count = data.frame(table(Month_df$DEST_CITY_NAME))
    top_15_dest = dest_count[order(-dest_count$Freq),][1:15,]$Var1 %>% factor()
    Month_top_15 = Month_df[Month_df$ORIGIN_CITY_NAME %in% top_15_dest]
    Month_top_15$ORIGIN_CITY_NAME = factor(Month_top_15$ORIGIN_CITY_NAME, levels = top_15_dest)
    Month_top_15$FL_DATE = as.Date(Month_top_15$FL_DATE)
    Month_top_15$month = format(Month_top_15$FL_DATE, '%b')
    ggplot(Month_top_15, aes(factor(month, levels = month.abb))) + geom_bar(aes(fill = factor(ORIGIN_CITY_NAME)))
  })
  
  output$delay_Plot <- renderPlot({
    Month_df$FL_DATE = as.Date(Month_df$FL_DATE)
    Month_df$month = format(Month_df$FL_DATE, '%b')
    Month_delay = Month_df[,c("month", "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY")] %>%
      melt(id = "month") %>% na.omit()
    Month_delay = Month_delay[Month_delay$value>0]
    ggplot(Month_delay, aes(x = factor(month, levels = month.abb), y = value)) + 
      facet_wrap(~variable, scales = "free_y") +
      geom_col(aes(fill = variable))
  })

}

shinyApp(ui = ui, server = server)

# Learn to Fly - Group 6 - CS424 Spring 2017
# Inspired by the sample R + Shiny example for CS 424 Spring 2018 UIC - Andy Johnson
# www.evl.uic.edu/aej/424
#test
# Libraries to include
library(car)
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
library(scales)
library(gridExtra)
##  "13930","Chicago, IL: Chicago O'Hare International"   
### "13232","Chicago, IL: Chicago Midway International"

#Keep all data files in the 'Data' folder!!

#Load Lookup Tables
print("Reading lookup tables")
holidays = fread("Data/holidays.csv", header = T)[,.(V2,V3)]
setnames(holidays,c("holidays","date"))
holidays = data.frame(holidays)[2:11,]
holidays$date = as.Date(holidays$date, "%m-%d-%y")
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

#-----------------------
# Grade A last two points
#

data = copy(Month_df)
setnames(data, tolower(names(data)))

data[,":="(
  fl_date = as.Date(fl_date, "%Y-%m-%d"),
  dummy = 1)]

data[,":="(
  month_name = month(fl_date))]

busy_days = data[,.(daily_flight_count = length(month_name)), by = "fl_date"][order(-daily_flight_count)]

#holidays= head(busy_days[,.(holidays = as.character(daily_flight_count), date = fl_date)],10)
extra_cancellations = data[,.(
  cancellations = sum(cancelled, na.rm = T),
  flight_count = length(cancelled)),
  by = c("fl_date","origin_city_name")]

extra_cancellations[,":="(
  perc_cancellations = cancellations/ flight_count)
  ]

heavyCancellations = extra_cancellations[cancellations>0 & perc_cancellations>0.3][order(-cancellations)][]

specialDays = list(
  "Heavy Cancellations" = heavyCancellations[, .(fl_date, flight_count, cancellations, perc_cancellations)],
  "Holidays" = data.table(holidays)[, .(date, holidays)],
  "Busy Days" = busy_days
)
data[, ":="(origin_state= substr(origin_city_name, nchar(as.character(origin_city_name))-1,100),
            dest_state = substr(dest_city_name, nchar(as.character(dest_city_name))-1,100))]


depCount = data[,.(departure_count = sum(dummy)), by = "origin_state"]
depCount[,perc_departures := departure_count/sum(departure_count)]
arrivalCount = data[,.(arrival_count = sum(dummy)), by = "dest_state"]
arrivalCount[,perc_arrivals := arrival_count/ sum(arrival_count)]

allTakeOffs = merge(depCount[,.(state = origin_state, departure_count, perc_departures = paste(perc_departures*100,"%", sep = ""))], arrivalCount[,.(state = dest_state, arrival_count, perc_arrivals = paste(perc_arrivals*100, "%", sep = ""))], by = "state", all = T)


#days=c("mon","tues","wed","thur","fri","sat","sun")
#Jan$day=c(rep(days,length(Jan[[1]])/7),days[c(1:(length(Jan[[1]])%%7))])



#departures=Month[Month$ORIGIN_AIRPORT_ID=="13930" | Month$ORIGIN_AIRPORT_ID=="13232",]
#arrivals=Month[Month$DEST_AIRPORT_ID=="13930" | Month$DEST_AIRPORT_ID=="13232",]
#airportsdepart=data.frame(table(departures$CARRIER))
#airportsarrival=data.frame(table(arrivals$CARRIER))
#airporttimes=data.frame(ID=airportsdepart[[1]],departing=airportsdepart[[2]],arrivals=airportsarrival[[2]])

# assume all of the tsv files in this directory are data of the same kind that I want to visualize

choices_airport=unique(Month_df$ORIGIN_CITY_NAME)
choices_day=unique(May$DAY_OF_WEEK)
choices_delay=c("NAS_Delay","WEATHER_DELAY","CARRIER_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY")
choices_fl_num=unique(Month_df$FL_NUM)


ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Spring 2018 Example Dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
   
   #Ask the user to give inputs about the Airport, Month, and timeframe
    fluidRow(
      selectInput("Airport", "Airport", c("Chicago O'Hare", "Chicago Midway","Both")),
      selectInput("month", "Month", c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG","SEPT","OCT","NOV","DEC")),
      selectInput("timeframe", "timeframe", c("1-24","AM-PM"))
    ),
     
    fluidRow(
      #Part 2-a 
      tabPanel("AirlineFlightPlot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("AirlineFlightPlot",width="450px",height="450px")) ),
      tabPanel("AirlineFlightTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("AirlineFlightTable"))  ),
      #Part 2-b 
      tabPanel("HourlyFlights", box(title = "Airline Hourly Flights", solidHeader = TRUE, status = "primary", width = 8, plotOutput("HourlyFlights"))  ),
      tabPanel("HourlyTable", box(title = "Airline Hourly Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("HourlyTable"))  )
      
    ),
    
     #Part 2-e
    fluidRow(
      tabPanel("Arrival Flights",box( title = "Arrival Flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("ArrivalFlightsPlot",width="450px",height="450px")) ),
      tabPanel("Arrival Flights Table", box(title = "Arrival Flights Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("ArrivalFlightsTable"))  )
      
    ),
    
     #Part 2-e
    fluidRow(
      tabPanel("Depart Flights",box( title = "Depart Flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("DepartFlightsPlot",width="450px",height="450px")) ),
      tabPanel("Depart Flights Table", box(title = "Depart Flights Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("DepartFlightsTable"))  )
      
    ),
     #Part 2-c
    fluidRow(
      tabPanel("Weekly Flights",box( title = "Weekly Flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("WeeklyFlightsPlot",width="750px",height="750px")) )
    ),
      fluidRow(
        tabPanel("Weekly Flights",box( title = "Weekly Flights", solidHeader = TRUE, status = "primary", width = 10, dataTableOutput("WeeklyFlightsTable",width="750px",height="750px")) )
      ),
    
      #Part 2-d
      fluidRow(
        tabPanel("Arrival_Delays",box( title = "Arrival_Delays", solidHeader = TRUE, status = "primary", width = 10, plotOutput("ArrivalDelays",width="750px",height="750px")) ),
        tabPanel("Depart_Delays",box( title = "Depart_Delays", solidHeader = TRUE, status = "primary", width = 10, plotOutput("DepartDelays",width="750px",height="750px")) ),
        tabPanel("DepartDelayTable",box( title = "DepartDelayTable", solidHeader = TRUE, status = "primary", width = 10, dataTableOutput("DepartDelayTable",width="750px",height="750px")) ),
        tabPanel("ArrivalDelayTable",box( title = "ArrivalDelayTable", solidHeader = TRUE, status = "primary", width = 10, dataTableOutput("ArrivalDelayTable",width="750px",height="750px")) )
        
      ),
    
    #################################PART B BEGINS HERE
    fluidRow(
      tabPanel("2017 Overall Arrival Departure by hour",
               box( title = "2017 Overall Arrival Departure by hour", 
                    solidHeader = TRUE, status = "primary", width = 10, 
                    plotOutput("arrival_departure_times",width="750px",height="750px")) )
    ),
    fluidRow(
      tabPanel("2017 Overall Arrivals",box( title = "2017 Overall Arrivals", solidHeader = TRUE, status = "primary", width = 10, plotOutput("arrival_departure_2017",width="750px",height="750px")) )
    ),
    fluidRow(
      tabPanel("Top 15 Destinations",box( title = "Top 15 Destinations", solidHeader = TRUE, status = "primary", width = 10, plotOutput("top_15_dest_Plot",width="750px",height="750px")) )
    ),
    fluidRow(
      tabPanel("Delay Causes",box( title = "Delay Causes", solidHeader = TRUE, status = "primary", width = 10, plotOutput("delay_Plot",width="750px",height="750px")) )
    ),
    #################################PART A BEGINS HERE
    fluidRow(
      selectInput("State", "State", allTakeOffs$state)
    ),
    fluidRow(
      #Part 2-a 
      tabPanel("State Info",box( title = "Flight Landing and Take off info", solidHeader = TRUE, status = "primary", width = 10,dataTableOutput("takeOffs",width="750px",height="75px"))
      )
    ),
    fluidRow(
      selectInput("dateType", "Which dates would you like to see?", names(specialDays))
    ),
    fluidRow(
      #Part 2-a 
      tabPanel("Special Dates",box( title = "Special Dates", solidHeader = TRUE, status = "primary", width = 10,dataTableOutput("special_days",width="750px",height="75px"))
      )
    ),
    #################################A begins here
    fluidRow(
      
      selectInput("Select_Airport", "Select_Airport", choices_airport)
      
    ),
    fluidRow(
      tabPanel("Lauderdale_airport",box( title = "Lauderdale_airport", solidHeader = TRUE, status = "primary", width = 12, plotOutput("Lauderdale_airport",height="1000px")) )
    ),
    fluidRow(
      
      selectInput("Select_Day_of_the_Week", "Select_Day_of_the_Week", choices_day)
      
    ),
    fluidRow(
      tabPanel("Monday",box( title = "Monday", solidHeader = TRUE, status = "primary", width = 12, plotOutput("one_day_of_week",height="1000px")) )
    ),
    fluidRow(
      
      selectInput("Delay_Causes", "Delay_Causes", choices_delay)
      
    ),
    fluidRow(
      tabPanel("Weather Delay Causes",box( title = "Weather Delay Causes", solidHeader = TRUE, status = "primary", width = 12, plotOutput("nas_delay_Plot",height="750px")) )
    ),
    fluidRow(
      
      selectInput("Flight_No", "Flight_No", choices_fl_num)
      
    ),
    fluidRow(
      tabPanel("Flight No:200",box( title = "Flight No:200", solidHeader = TRUE, status = "primary", width = 12, plotOutput("airline_200",height="750px")) )
    ),
    fluidRow(
      tabPanel("One day",box( title = "One day", solidHeader = TRUE, status = "primary", width = 12, plotOutput("one_day",height="750px")) )
    ),
    #################################PART GRAD BEGINS HERE
    fluidRow(
      sliderInput("range", "Range:",
                  min = 0, max = max(Month_df$DISTANCE),
                  value = c(200,500))
    ),
    fluidRow(
      tabPanel("Number of Flights by Distance",box( title = "Number of Flights by Distance", solidHeader = TRUE, status = "primary", width = 10, plotOutput("distance_range_plot",width="750px",height="75px")))
    )
  )
)

server <- function(input, output) {
  
  theme_set(theme_grey(base_size = 17)) 
  
  #Helper functions delcared here
  
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
  
  
  
  ############################################Part 2-a
  output$AirlineFlightPlot <- renderPlot({   ###  VX airlines went out of business in 2003  :)  
    
    
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
  
############################################Part 2-a
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
  
  
  
  ############################################Part 2-b
  output$HourlyTable <- DT::renderDataTable(
    
    
    
    DT::datatable({ 
      
      
      Month=Month[Monthnames ==input$month]
      Month=Month[[1]]

      
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
  
  
 ############################################Part 2-b
  output$HourlyFlights<- 
    
    renderPlot({
      
      
      Month=Month[Monthnames ==input$month]

      Month=Month[[1]]

      
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
  
  
  
  ###########################################Part 2-e
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
  
    ###########################################Part 2-e
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
  
  
  
    ###########################################Part 2-e
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
  
    ###########################################Part 2-e
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
  
    ###########################################Part 2-c
  output$WeeklyFlightsPlot <- renderPlot({
   c
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
  
    ###########################################Part 2-c
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



  ###########################################Part 2-d
output$ArrivalDelays <- renderPlot({
  Month=Month[Monthnames ==input$month]
     Month=Month[[1]]




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


    ###########################################Part 2-d
  output$DepartDelays <- renderPlot({
     Month=Month[Monthnames ==input$month]
    #Month=read.csv("Feb.csv")
       Month=Month[[1]]


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


    ###########################################Part 2-d
  output$DepartDelayTable <- DT::renderDataTable(


    DT::datatable({
       Month=Month[Monthnames ==input$month]
      #Month=read.csv("Feb.csv")
       Month=Month[[1]]


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
        TravelTimes=data.frame(Times=times,Delays1=delays1,Proportion1=100*delays1/departs1,
                               Delays2=delays2,Proportion2=100*delays2/departs2)
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



  ###########################################Part 2-d
  output$ArrivalDelayTable <- DT::renderDataTable(


    DT::datatable({
       Month=Month[Monthnames ==input$month]
      #Month=read.csv("Feb.csv")
       Month=Month[[1]]


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
        TravelTimes=data.frame(Times=times,Delays1=delays1,Proportion1=100*delays1/arrivals1,
                               Delays2=delays2,Proportion2=100*delays2/arrivals2)
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

  output$arrival_departure_times <- renderPlot({
    times=c(1:24)
    travel_times = data.frame(times=times,
                              arrivals = getarrivals(Month_df),
                              departures = getdeps(Month_df)) %>%
      melt(., id = "times")

    ggplot(travel_times, aes(x=times, y = value, colour = variable)) +
      geom_line() +
      scale_x_continuous(limits=c(0,24),
                         breaks=0:12*2,
                         labels=c(paste(0:5*2,"am"),
                                  "12 pm",
                                  paste(7:11*2-12,"pm"),
                                  "0 am"))
  })

  output$arrival_departure_2017 <- renderPlot({
    Month_df$FL_DATE = as.Date(Month_df$FL_DATE)
    Month_df$month = format(Month_df$FL_DATE, '%b')
    Month_freq = data.frame(table(Month_df$month))
    arrival_departure = data.frame(month = factor(Month_freq$Var1, levels = month.abb),
                                   arrivals = Month_freq$Freq,
                                   departures = Month_freq$Freq) %>%
      melt(.,id="month")
    ggplot(arrival_departure, aes(x = factor(month, levels = month.abb), y = value)) +
      geom_bar(stat = "identity",aes(fill=variable), position = "dodge")
  })

  output$top_15_dest_Plot <- renderPlot({
    dest_count = data.frame(table(Month_df$DEST_CITY_NAME))
    top_15_dest = dest_count[order(-dest_count$Freq),][1:15,]$Var1 %>% factor()
    Month_top_15 = Month_df[Month_df$ORIGIN_CITY_NAME %in% top_15_dest]
    Month_top_15$ORIGIN_CITY_NAME = factor(Month_top_15$ORIGIN_CITY_NAME, levels = rev(top_15_dest))
    Month_top_15$FL_DATE = as.Date(Month_top_15$FL_DATE)
    Month_top_15$month = format(Month_top_15$FL_DATE, '%b')
    ggplot(Month_top_15, aes(factor(month, levels = month.abb))) +
      geom_bar(aes(fill = factor(ORIGIN_CITY_NAME)))
  })

  output$delay_Plot <- renderPlot({
    Month_df$FL_DATE = as.Date(Month_df$FL_DATE)
    Month_df$month = format(Month_df$FL_DATE, '%b')
    Month_delay = Month_df[,c("month", "SECURITY_DELAY", "WEATHER_DELAY", "NAS_DELAY", "CARRIER_DELAY", "LATE_AIRCRAFT_DELAY")] %>%
      melt(id = "month") %>% na.omit()
    Month_delay = Month_delay[Month_delay$value>0]
    ggplot(Month_delay, aes(x = factor(month, levels = month.abb), y = value, group =variable)) +
      aes(colour = variable) +
      stat_summary(fun.y = "sum", geom = "line") +
      coord_trans(y = "log10") +
      scale_y_continuous( breaks = trans_breaks('log10', function(x) 10^x),
                          labels = trans_format('log10', math_format(10^.x))) +
      labs(x="2017 Months", y="Number of Delays")
  })
  ###################PART A BEGINS HERE
  output$takeOffs <-renderDataTable(
    allTakeOffs[,.(State = state, `Departure Count` = departure_count, `% Departures` = perc_departures, `Arrival Count` = arrival_count, "% Arrivals" = perc_arrivals)], options = list(pageLength= 5)
  )
  
  output$special_days <-DT::renderDataTable(
    
    DT::datatable({
    
    dataSpecial = as.data.table(specialDays[input$dateType])
    
    if(input$dateType == "Heavy Cancellations"){
      setnames(dataSpecial, c("Date", "Flights","Cancellations", "Percentage Cancellations"))}
    else if (input$dateType == "Holidays"){
      setnames(dataSpecial, c("Date", "Holiday Name"))}
    else  {setnames(dataSpecial, c("Date", "No. of Flights"))}
    
    data.frame(dataSpecial)[1:10,]}, options = list(pageLength= 10)
  ))
  ####################C Part Begins here


  output$Lauderdale_airport<-renderPlot({
    Month_df$FL_DATE = as.Date(Month_df$FL_DATE)
    Month_df$month = format(Month_df$FL_DATE, '%b')

    display_data = Month_df[,c("month","DEP_TIME","ARR_TIME","ORIGIN_CITY_NAME","DEST_CITY_NAME")]
    display_data_dest=display_data[DEST_CITY_NAME==input$Select_Airport]
    display_data_dest=subset(display_data_dest,select =c(month,ARR_TIME))

    display_data_org=display_data[ORIGIN_CITY_NAME=='Fort Lauderdale, FL']
    display_data_org=subset(display_data_org,select =c(month,DEP_TIME))


    display_data_org=melt(display_data_org,id="month")
    display_data_org=na.omit(display_data_org)

    display_data_dest=melt(display_data_dest,id="month")
    display_data_dest=na.omit(display_data_dest)
    binded_data=rbind(display_data_dest,display_data_org)


    ##################
    #Jan
    Jan__melted=binded_data[binded_data$month=='Jan']
    Jan_gg<-ggplot(Jan__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Jan")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Feb
    Feb__melted=binded_data[binded_data$month=='Feb']
    Feb_gg<-ggplot(Feb__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Feb")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Mar
    Mar__melted=binded_data[binded_data$month=='Mar']
    Mar_gg<-ggplot(Mar__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Mar")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Apr
    Apr__melted=binded_data[binded_data$month=='Apr']
    Apr_gg<-ggplot(Apr__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Apr")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #May
    May__melted=binded_data[binded_data$month=='May']
    May_gg<-ggplot(May__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="May")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Jun
    Jun__melted=binded_data[binded_data$month=='Jun']
    Jun_gg<-ggplot(Jun__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Jun")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Jul
    Jul__melted=binded_data[binded_data$month=='Jul']
    Jul_gg<-ggplot(Jul__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Jul")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Aug
    Aug__melted=binded_data[binded_data$month=='Aug']
    Aug_gg<-ggplot(Aug__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Aug")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Sept
    Sep__melted=binded_data[binded_data$month=='Sep']
    Sep_gg<-ggplot(Sep__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Sept")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Oct
    Oct__melted=binded_data[binded_data$month=='Oct']
    Oct_gg<-ggplot(Oct__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Oct")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Nov
    Nov__melted=binded_data[binded_data$month=='Nov']
    Nov_gg<-ggplot(Nov__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Nov")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Dec
    Dec__melted=binded_data[binded_data$month=='Dec']
    Dec_gg<-ggplot(Dec__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Dec")+
      labs(x="", y="Hour") + theme(legend.position="none")



    grid.arrange(Jan_gg,Feb_gg,Mar_gg,Apr_gg,May_gg,Jun_gg,Jul_gg,Aug_gg,Sep_gg,Oct_gg,Nov_gg,Dec_gg,ncol=6)

  })

  output$one_day_of_week<-renderPlot({
    Month_df$FL_DATE = as.Date(Month_df$FL_DATE)
    Month_df$month = format(Month_df$FL_DATE, '%b')
    monday = Month_df[,c("DAY_OF_WEEK","month", "SECURITY_DELAY", "WEATHER_DELAY", "NAS_DELAY", "CARRIER_DELAY", "LATE_AIRCRAFT_DELAY","DEP_TIME","ARR_TIME")]

    monday=monday[DAY_OF_WEEK==input$Select_Day_of_the_Week]


    monday=na.omit(monday)
    monday$total_delay=monday$SECURITY_DELAY+monday$WEATHER_DELAY+monday$NAS_DELAY+monday$CARRIER_DELAY+monday$LATE_AIRCRAFT_DELAY
    monday_melted = monday[,c("month", "DEP_TIME", "ARR_TIME")]
    monday_melted=melt(monday_melted,id='month')
    monday_delay=monday[,c("month", "DEP_TIME", "total_delay")]
    ##################
    #############################JAN
    Jan_monday_melted=monday_melted[monday_melted$month=='Jan']
    Jan_monday_delay=monday_delay[monday_delay$month=='Jan']
    Jan_gg1<-ggplot(Jan_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+

      labs(x="", y="Hour") + theme(legend.position="none")
    Jan_gg2<-ggplot(Jan_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+

      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Jan_gg<-grid.arrange(Jan_gg1,Jan_gg2,ncol=2,top="JAN",widths=c(2,1))
    ##################################
    ################################Feb
    Feb_monday_melted=monday_melted[monday_melted$month=='Feb']
    Feb_monday_delay=monday_delay[monday_delay$month=='Feb']
    Feb_gg1<-ggplot(Feb_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")
    Feb_gg2<-ggplot(Feb_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Feb_gg<-grid.arrange(Feb_gg1,Feb_gg2,ncol=2,top="Feb",widths=c(2,1))
    ###################################
    ################################ MAR
    Mar_monday_melted=monday_melted[monday_melted$month=='Mar']
    Mar_monday_delay=monday_delay[monday_delay$month=='Mar']
    Mar_gg1<-ggplot(Mar_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")
    Mar_gg2<-ggplot(Mar_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Mar_gg<-grid.arrange(Mar_gg1,Mar_gg2,ncol=2,top="MAR",widths=c(2,1))
    ########################################
    #####################################Apr
    Apr_monday_melted=monday_melted[monday_melted$month=='Apr']
    Apr_monday_delay=monday_delay[monday_delay$month=='Apr']
    Apr_gg1<-ggplot(Apr_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")
    Apr_gg2<-ggplot(Apr_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Apr_gg<-grid.arrange(Apr_gg1,Apr_gg2,ncol=2,top="Apr",widths=c(2,1))
    #######################################
    ####################################May
    May_monday_melted=monday_melted[monday_melted$month=='May']
    May_monday_delay=monday_delay[monday_delay$month=='May']
    May_gg1<-ggplot(May_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")
    May_gg2<-ggplot(May_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    May_gg<-grid.arrange(May_gg1,May_gg2,ncol=2,top="May",widths=c(2,1))
    #######################################
    #####################################June
    June_monday_melted=monday_melted[monday_melted$month=='Jun']
    June_monday_delay=monday_delay[monday_delay$month=='Jun']
    June_gg1<-ggplot(June_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")
    June_gg2<-ggplot(June_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    June_gg<-grid.arrange(June_gg1,June_gg2,ncol=2,top="June",widths=c(2,1))
    ################################################
    #########################################July
    Jul_monday_melted=monday_melted[monday_melted$month=='Jul']
    Jul_monday_delay=monday_delay[monday_delay$month=='Jul']
    Jul_gg1<-ggplot(Jul_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="Hour") + theme(legend.position="none")
    Jul_gg2<-ggplot(Jul_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Jul_gg<-grid.arrange(Jul_gg1,Jul_gg2,ncol=2,top="July",widths=c(2,1))
    #################################################
    #############################################Aug
    Aug_monday_melted=monday_melted[monday_melted$month=='Aug']
    Aug_monday_delay=monday_delay[monday_delay$month=='Aug']
    Aug_gg1<-ggplot(Aug_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")
    Aug_gg2<-ggplot(Aug_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Aug_gg<-grid.arrange(Aug_gg1,Aug_gg2,ncol=2,top="Aug",widths=c(2,1))
    ########################################
    #############################################Sept
    Sep_monday_melted=monday_melted[monday_melted$month=='Sep']
    Sep_monday_delay=monday_delay[monday_delay$month=='Sep']
    Sep_gg1<-ggplot(Sep_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")
    Sep_gg2<-ggplot(Sep_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Sep_gg<-grid.arrange(Sep_gg1,Sep_gg2,ncol=2,top="Sep",widths=c(2,1))
    #######################################
    #####################################Oct
    Oct_monday_melted=monday_melted[monday_melted$month=='Oct']
    Oct_monday_delay=monday_delay[monday_delay$month=='Oct']
    Oct_gg1<-ggplot(Oct_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")
    Oct_gg2<-ggplot(Oct_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Oct_gg<-grid.arrange(Oct_gg1,Oct_gg2,ncol=2,top="Oct",widths=c(2,1))
    ####################################
    ################################Nov
    Nov_monday_melted=monday_melted[monday_melted$month=='Nov']
    Nov_monday_delay=monday_delay[monday_delay$month=='Nov']
    Nov_gg1<-ggplot(Nov_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")
    Nov_gg2<-ggplot(Nov_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Nov_gg<-grid.arrange(Nov_gg1,Nov_gg2,ncol=2,top="Nov",widths=c(2,1))
    ##################################
    ######################################Dec
    Dec_monday_melted=monday_melted[monday_melted$month=='Dec']
    Dec_monday_delay=monday_delay[monday_delay$month=='Dec']
    Dec_gg1<-ggplot(Dec_monday_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="") + theme(legend.position="none")
    Dec_gg2<-ggplot(Dec_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    Dec_gg<-grid.arrange(Dec_gg1,Dec_gg2,ncol=2,top="Dec",widths=c(2,1))
    ########################################
    grid.arrange(Jan_gg,Feb_gg,Mar_gg,Apr_gg,May_gg,June_gg,Jul_gg,Aug_gg,Sep_gg,Oct_gg,Nov_gg,Dec_gg,ncol=6)
  })

  output$nas_delay_Plot <- renderPlot({
    Month_df$FL_DATE = as.Date(Month_df$FL_DATE)
    Month_df$month = format(Month_df$FL_DATE, '%b')
    Month_delay = Month_df[,c("month", "WEATHER_DELAY","DEP_TIME")] %>%
      na.omit()
    Month_delay=Month_delay[WEATHER_DELAY>0]
    ggplot(Month_delay, aes(x = factor(month, levels = month.abb), y = DEP_TIME/100)) +
      geom_point(aes(colour = WEATHER_DELAY,size=WEATHER_DELAY/10+20),shape=1,stroke=3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+

      scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="2017 Months", y="Hour")

  })

  output$one_day <- renderPlot({
    Month_df$FL_DATE = as.Date(Month_df$FL_DATE)
    Month_df$month = format(Month_df$FL_DATE, '%b')
    day=Month_df[Month_df$FL_DATE=='2017-10-11']
    day = day[,c("month", "SECURITY_DELAY", "WEATHER_DELAY", "NAS_DELAY", "CARRIER_DELAY", "LATE_AIRCRAFT_DELAY","DEP_TIME","ARR_TIME")]
    day=na.omit(day)
    day$total_delay=day$SECURITY_DELAY+day$WEATHER_DELAY+day$NAS_DELAY+day$CARRIER_DELAY+day$LATE_AIRCRAFT_DELAY
    day_melted = day[,c("month", "DEP_TIME", "ARR_TIME")]
    day_melted=melt(day_melted,id='month')
    day_delay=day[,c("month", "DEP_TIME", "total_delay")]

    gg1<-ggplot(day_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable), size = 3, shape=1,stroke=3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+

      labs(x="", y="Hour") + theme(legend.position="none")
    gg2<-ggplot(day_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay),  shape=1,stroke=3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+

      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    grid.arrange(gg1,gg2,ncol=2,top="2017-10-11",widths=c(2,1))

  })
  output$airline_200 <- renderPlot({
    Month_df$FL_DATE = as.Date(Month_df$FL_DATE)
    Month_df$month = format(Month_df$FL_DATE, '%b')

    Month_delay = Month_df[,c("month","FL_NUM", "ARR_TIME","DEP_TIME")]
    Month_delay=Month_delay[Month_delay$FL_NUM==input$Flight_No]
    Month_delay = Month_delay[,c("month", "ARR_TIME","DEP_TIME")]
    Month_delay=melt(Month_delay,id='month')
    Month_delay=na.omit(Month_delay)
    ggplot(Month_delay, aes(x = factor(month, levels = month.abb), y = value/100)) +
      geom_point(aes(colour = variable,size=1),fill = "white", size = 3, shape=1,stroke=3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+

      labs(x="2017 Months", y="Hour")

  })
  ###################PART GRAD BEGINS HERE
  sliderValues <- reactive({
    input$range
  })

  output$distance_range_plot <- renderPlot({

    dist_min = sliderValues()[1]
    dist_max = sliderValues()[2]

    dist_values = Month_df[(Month_df$DISTANCE >= dist_min) & (Month_df$DISTANCE <= dist_max)]$DISTANCE
    dist_count = data.frame(label = "number of flights", dist_count = dist_values)
    options(scipen = 999)
    ggplot(dist_count, aes(x = label)) +
      ylab("Number of Flights") +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      geom_bar() +
      scale_y_continuous(labels = function(x){paste0(x/1000, ' K',sep = "")})+
      coord_flip() +
      ylim(0, dim(Month_df)[1])
  })
}

shinyApp(ui = ui, server = server)

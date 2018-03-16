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
print("Reading holiday tables")
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
rm(Jan,Feb,Mar,Apr,May,June,July,Aug,Sept,Oct,Nov,Dec)
#Add Month dataframe with data for the whole year
Month_df = rbindlist(Month)
Month_df$FL_DATE = as.Date(Month_df$FL_DATE) #diff
Monthnames=c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG","SEPT","OCT","NOV","DEC")

# Add lookup fields
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

days=c(1,2,3,4,5,6,7) #diff
names(days)=c("Mon","Tues","Wed","Thur","Fri","Sat","Sun")

#Jan$day=c(rep(days,length(Jan[[1]])/7),days[c(1:(length(Jan[[1]])%%7))])



#departures=Month[Month$ORIGIN_AIRPORT_ID=="13930" | Month$ORIGIN_AIRPORT_ID=="13232",]
#arrivals=Month[Month$DEST_AIRPORT_ID=="13930" | Month$DEST_AIRPORT_ID=="13232",]
#airportsdepart=data.frame(table(departures$CARRIER))
#airportsarrival=data.frame(table(arrivals$CARRIER))
#airporttimes=data.frame(ID=airportsdepart[[1]],departing=airportsdepart[[2]],arrivals=airportsarrival[[2]])

# assume all of the tsv files in this directory are data of the same kind that I want to visualize

choices_airport=unique(Month_df$ORIGIN_CITY_NAME)
choices_airport=choices_airport[order(choices_airport)]
choices_day=names(days)
choices_delay=c("NAS DELAY","WEATHER DELAY","CARRIER DELAY","SECURITY DELAY","LATE AIRCRAFT DELAY")
choices_fl_num=unique(Month_df$FL_NUM)
choices_fl_num=choices_fl_num[order(choices_fl_num)]


ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Sp 18 Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inputs", tabName = "item1"),
      menuItem("Overall Flights", tabName = "item2"),
      menuItem("Arrivals/Departures",tabName = "item3"),
      menuSubItem("Arrivals",tabName = "item3a"),
      menuSubItem("Departures",tabName = "item3b"),
      menuSubItem("Weekly",tabName = "item3c"),
      menuSubItem("Delays",tabName = "item3d"),
      menuItem("Top 15 Destinations", tabName = "item4"),
      menuItem("Landing/TakeOffs",tabName = "item5"),
      menuItem("Special Dates",tabName = "item6"),
      menuItem("Flights by distance",tabName = "item7"),
      menuItem("Outliers",tabName = "item8")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "item1",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset1", 
                #Ask the user to give inputs about the Airport, Month, and timeframe
                       tabPanel("Inputs", 
                                selectInput("Airport", "Airport", c("Chicago O'Hare", "Chicago Midway","Both")),
                                selectInput("month", "Month", c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG","SEPT","OCT","NOV","DEC")),
                                selectInput("timeframe", "timeframe", c("1-24","AM-PM")))
                )
              )
      ),
      tabItem(tabName = "item2",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset2", 
                       tabPanel("AirlineFlightPlot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("AirlineFlightPlot",width="450px",height="450px")) ),
                       tabPanel("AirlineFlightTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("AirlineFlightTable"))  ),
                       #Part 2-b 
                       tabPanel("HourlyFlights", box(title = "Airline Hourly Flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("HourlyFlights"))  ),
                       tabPanel("HourlyTable", box(title = "Airline Hourly Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("HourlyTable"))  )
                )
              )
      ),
#################################PART B
      tabItem(tabName = "item3",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset3", 
                       tabPanel("2017 Overall",box( title = "2017 Overall Arrival Departure by hour", solidHeader = TRUE, status = "primary", width = 10, plotOutput("arrival_departure_times",width="750px",height="750px")) ),
                       tabPanel("2017 Overall Arrivals",box( title = "2017 Overall Arrivals", solidHeader = TRUE, status = "primary", width = 10, plotOutput("arrival_departure_2017",width="750px",height="750px")) )
                )
              )
      ),
#################################Part 2-e
      tabItem(tabName = "item3a",
              fluidRow(
                tabBox(title = "",
                      width = "100%",
                      height = "2000px",
                      id = "tabset3a", 
                      tabPanel("Arrival Flights",box( title = "Arrival Flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("ArrivalFlightsPlot",width="450px",height="450px")) ),
                      tabPanel("Arrival Flights Table", box(title = "Arrival Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("ArrivalFlightsTable"))  )
                 
                )
              )
      ),
    
     #Part 2-e
      tabItem(tabName = "item3b",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset3b", 
                       tabPanel("Depart Flights",box( title = "Depart Flights", solidHeader = TRUE, status = "primary", width = 6, plotOutput("DepartFlightsPlot",width="450px",height="450px")) ),
                       tabPanel("Depart Flights Table", box(title = "Depart Flights Table", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("DepartFlightsTable"))  )
                 
                )
              )
      ),
      #Part 2-c
      tabItem(tabName = "item3c",
             fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset3c", 
                       tabPanel("Weekly Flights",box( title = "Weekly Flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("WeeklyFlightsPlot",width="750px",height="750px")) ),
                       tabPanel("Weekly Flights Table",box( title = "Weekly Flights Table", solidHeader = TRUE, status = "primary", width = 10, dataTableOutput("WeeklyFlightsTable",width="750px",height="750px")) )
                 
                )
              )
      ),
    
      #Part 2-d
     
      tabItem(tabName = "item3d",
             fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset3d", 
                       tabPanel("Arrival Delays",box( title = "Arrival Delays", solidHeader = TRUE, status = "primary", width = 10, plotOutput("ArrivalDelays",width="750px",height="750px")) ),
                       tabPanel("Arrival Delay Table",box( title = "Arrival Delay Table", solidHeader = TRUE, status = "primary", width = 10, dataTableOutput("ArrivalDelayTable",width="750px",height="750px")) ),
                       tabPanel("Depart Delays",box( title = "Depart Delays", solidHeader = TRUE, status = "primary", width = 10, plotOutput("DepartDelays",width="750px",height="750px")) ),
                       tabPanel("Depart Delay Table",box( title = "Depart Delay Table", solidHeader = TRUE, status = "primary", width = 10, dataTableOutput("DepartDelayTable",width="750px",height="750px")) ),
                       tabPanel("Delay Causes",
                                selectInput("Delay_Causes", "Select Delay", choices_delay),
                                box( title = "Delay Causes", solidHeader = TRUE, status = "primary", width = 10, plotOutput("delay_Plot",width="750px",height="750px")) ),
                       tabPanel("Delay Information",selectInput("delay", "Select Delay", choices_delay),box( title = "Delay Causes", solidHeader = TRUE, status = "primary", width = 12, plotOutput("nas_delay_Plot",height="750px")) )
                )
            )
      ),
      tabItem(tabName = "item4",
              fluidRow(
                tabBox(title = "",
                      width = "100%",
                      height = "2000px",
                      id = "tabset4", 
                      tabPanel("Top 15 Destinations",box( title = "", solidHeader = TRUE, status = "primary", width = 10, plotOutput("top_15_dest_Plot",width="750px",height="750px")) )
                )
              )
      ),

    #################################PART A BEGINS HERE
      tabItem(tabName = "item5",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset5", 
                       tabPanel("State Info",
                                selectInput("State", "State", c("AK","AL","AR","AZ","CA","CO","CT","DC","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")),
                                box(title = "Flight Landing and Take off info", solidHeader = TRUE, status = "primary", width = 10,dataTableOutput("takeOffs",width="750px",height="75px")))
                       )
                )
      ),
      tabItem(tabName = "item6",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset6", 
                       tabPanel("Special Dates",
                                selectInput("dateType", "Which dates would you like to see?", names(specialDays)),
                                box( title = "", solidHeader = TRUE, status = "primary", width = 10,dataTableOutput("special_days",width="750px",height="75px"))
                                )
                       )
                )
      ),

#################################PART GRAD BEGINS HERE
    tabItem(tabName = "item7",
            fluidRow(
              tabBox(title = "",
                     width = "100%",
                     height = "2000px",
                     id = "tabset7", 
                     tabPanel("Flights by distance",
                              sliderInput("range", "Flight Distance:", min = 0, max = max(Month_df$DISTANCE),value = c(200,500)),
                     tabPanel("Number of Flights by Distance",box( title = "Number of Flights by Distance", solidHeader = TRUE, status = "primary", width = 10, plotOutput("distance_range_plot",width="750px",height="75px"))),
                     tabPanel("Number of Flights by Air Time",
                              sliderInput("time_range", "Flight Time (minutes):", min = 0, max = 600, value = c(200,300)),
                              box( title = "Number of Flights by Air Time", solidHeader = TRUE, status = "primary", width = 10, plotOutput("time_range_plot",width="750px",height="75px")))
                              
                              )
                     )
              )
      ),
    #################################Part A begins here

      tabItem(tabName = "item8",
              fluidRow(
                tabBox(title = "",
                       width = "100%",
                       height = "2000px",
                       id = "tabset8", 
                       tabPanel("Airport Information",
                                selectInput("Select_Airport", "Select Airport", choices_airport),
                                box( title = "", solidHeader = TRUE, status = "primary", width = 12, plotOutput("Lauderdale_airport",height="1000px")) ),
                       tabPanel("A day of the week",
                                selectInput("Select_Day_of_the_Week", "Select Day of the Week", choices_day),
                                box( title = "", solidHeader = TRUE, status = "primary", width = 12, plotOutput("one_day_of_week",height="1000px")) ),
                       tabPanel("Flight Information",selectInput("Flight_No", "Select Flight No", choices_fl_num),box( title = "", solidHeader = TRUE, status = "primary", width = 12, plotOutput("airline_200",height="750px")) ),
                       
                       tabPanel("One day of the year",dateInput("date", "Date:", min="2017-01-01",max="2017-12-31", format = "yyyy-mm-dd",value="2017-01-01"),box( title = "", solidHeader = TRUE, status = "primary", width = 12, plotOutput("one_day",height="750px")) )
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  theme_set(theme_grey(base_size = 17)) 
  
  #Helper functions delcared here
  getValue<-function(timeValue){
    
    if(timeValue<100 & timeValue>=0){
      new_min =  0
      new_max =  99
      old_min =  0
      old_max =  59
    }
    else if(timeValue<200 & timeValue>=100){
      new_min = 100
      new_max = 199
      old_min =100
      old_max =  159
    }
    else if(timeValue<300 & timeValue>=200){
      new_min = 200
      new_max = 299
      old_min =200
      old_max =  259
    }
    else if(timeValue<400 & timeValue>=300){
      new_min = 300
      new_max = 399
      old_min =300
      old_max =  359
    }
    else if(timeValue<500 & timeValue>=400){
      new_min = 400
      new_max = 499
      old_min =400
      old_max =  459
    }
    else if(timeValue<600 & timeValue>=500){
      new_min = 500
      new_max = 599
      old_min =500
      old_max =  559
    }
    else if(timeValue<700 & timeValue>=600){
      new_min = 600
      new_max = 699
      old_min = 600
      old_max = 659
    }
    else if(timeValue<800 & timeValue>=700){
      new_min = 700
      new_max = 799
      old_min = 700
      old_max = 759
    }
    else if(timeValue<900 & timeValue>=800){
      new_min = 800
      new_max = 899
      old_min = 800
      old_max = 859
    }
    else if(timeValue<1000 & timeValue>=900){
      new_min = 900
      new_max = 999
      old_min = 900
      old_max = 959
    }
    else if(timeValue<1100 & timeValue>=1000){
      new_min = 1000
      new_max = 1099
      old_min = 1000
      old_max = 1059
    }
    else if(timeValue<1200 & timeValue>=1100){
      new_min = 1100
      new_max = 1199
      old_min = 1100
      old_max = 1159
    }
    else if(timeValue<1300 & timeValue>=1200){
      new_min = 1200
      new_max = 1299
      old_min = 1200
      old_max = 1259
    }
    else if(timeValue<1400 & timeValue>=1300){
      new_min = 1300
      new_max = 1399
      old_min = 1300
      old_max = 1359
    }
    else if(timeValue<1500 & timeValue>=1400){
      new_min = 1400
      new_max = 1499
      old_min = 1400
      old_max = 1459
    }
    else if(timeValue<1600 & timeValue>=1500){
      new_min = 1500
      new_max = 1599
      old_min = 1500
      old_max = 1559
    }
    else if(timeValue<1700 & timeValue>=1600){
      new_min = 1600
      new_max = 1699
      old_min = 1600
      old_max = 1659
    }
    else if(timeValue<1800 & timeValue>=1700){
      new_min = 1700
      new_max = 1799
      old_min = 1700
      old_max = 1759
    }
    else if(timeValue<1900 & timeValue>=1800){
      new_min = 1800
      new_max = 1899
      old_min = 1800
      old_max = 1859
    }
    else if(timeValue<2000 & timeValue>=1900){
      new_min = 1900
      new_max = 1999
      old_min = 1900
      old_max = 1959
    }
    else if(timeValue<2100 & timeValue>=2000){
      new_min = 2000
      new_max = 2099
      old_min = 2000
      old_max = 2059
    }
    else if(timeValue<2200 & timeValue>=2100){
      new_min = 2100
      new_max = 2199
      old_min = 2100
      old_max = 2159
    }
    else if(timeValue<2300 & timeValue>=2200){
      new_min = 2200
      new_max = 2299
      old_min = 2200
      old_max = 2259
    }
    else if(timeValue<2300 & timeValue>=2200){
      new_min = 2200
      new_max = 2299
      old_min = 2200
      old_max = 2259
    }
    else if(timeValue<2400 & timeValue>=2300){
      new_min = 2300
      new_max = 2399
      old_min = 2300
      old_max = 2359
    }
    
    
    return ((new_max - new_min) / (old_max - old_min) * (timeValue - old_min) + new_min)
  }
  
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

  output$arrival_departure_2017 <- renderPlot({ #diff
    Month_df$month = format(Month_df$FL_DATE, '%b')
    Month_freq = select(Month_df, month, CARRIER) %>%
      table() %>%
      data.frame()
    ggplot(Month_freq, aes(x = factor(month, levels = month.abb), y = Freq, group = CARRIER)) +
      aes(colour = CARRIER) +
      stat_summary(fun.y = "sum", geom = "line") +
      coord_trans(y = "log10") +
      scale_y_continuous( breaks = trans_breaks('log10', function(x) 10^x),
                          labels = trans_format('log10', math_format(10^.x))) +
      labs(x="2017 Months", y="Number of Flights")
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
    Month_df$month = format(Month_df$FL_DATE, '%b')
    
    display_data = Month_df[,c("month","DEP_TIME","ARR_TIME","ORIGIN_CITY_NAME","DEST_CITY_NAME")]
    display_data_dest=display_data[DEST_CITY_NAME==input$Select_Airport]#
    display_data_dest=subset(display_data_dest,select =c(month,ARR_TIME))
    
    display_data_org=display_data[ORIGIN_CITY_NAME==input$Select_Airport]#
    display_data_org=subset(display_data_org,select =c(month,DEP_TIME))
    
    
    display_data_org=melt(display_data_org,id="month")
    display_data_org=na.omit(display_data_org)
    display_data_org$value<-apply(display_data_org[,c('value')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    display_data_dest=melt(display_data_dest,id="month")
    display_data_dest=na.omit(display_data_dest)
    display_data_dest$value<-apply(display_data_dest[,c('value')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    binded_data=rbind(display_data_dest,display_data_org)
    
    
    ##################
    #Jan
    Jan__melted=binded_data[binded_data$month=='Jan']
    Jan_gg<-ggplot(Jan__melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable))+
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Jan")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Feb
    Feb__melted=binded_data[binded_data$month=='Feb']
    Feb_gg<-ggplot(Feb__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Feb")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Mar
    Mar__melted=binded_data[binded_data$month=='Mar']
    Mar_gg<-ggplot(Mar__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Mar")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Apr
    Apr__melted=binded_data[binded_data$month=='Apr']
    Apr_gg<-ggplot(Apr__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Apr")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #May
    May__melted=binded_data[binded_data$month=='May']
    May_gg<-ggplot(May__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="May")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Jun
    Jun__melted=binded_data[binded_data$month=='Jun']
    Jun_gg<-ggplot(Jun__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Jun")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Jul
    Jul__melted=binded_data[binded_data$month=='Jul']
    Jul_gg<-ggplot(Jul__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Jul")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Aug
    Aug__melted=binded_data[binded_data$month=='Aug']
    Aug_gg<-ggplot(Aug__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Aug")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Sept
    Sep__melted=binded_data[binded_data$month=='Sep']
    Sep_gg<-ggplot(Sep__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Sept")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Oct
    Oct__melted=binded_data[binded_data$month=='Oct']
    Oct_gg<-ggplot(Oct__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Oct")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Nov
    Nov__melted=binded_data[binded_data$month=='Nov']
    Nov_gg<-ggplot(Nov__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Nov")+
      labs(x="", y="Hour") + theme(legend.position="none")
    ##################
    #Dec
    Dec__melted=binded_data[binded_data$month=='Dec']
    Dec_gg<-ggplot(Dec__melted, aes(x = variable, y = value/100)) +
      scale_colour_manual(values = c("ARR_TIME"="red", "DEP_TIME"="blue")) +
      geom_point(aes(colour = variable))+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title="Dec")+
      labs(x="", y="Hour") + theme(legend.position="none")
    
    
    
    grid.arrange(Jan_gg,Feb_gg,Mar_gg,Apr_gg,May_gg,Jun_gg,Jul_gg,Aug_gg,Sep_gg,Oct_gg,Nov_gg,Dec_gg,ncol=6)
    
  })
  
  output$one_day_of_week<-renderPlot({
    Month_df$month = format(Month_df$FL_DATE, '%b')
    monday = Month_df[,c("DAY_OF_WEEK","month", "SECURITY_DELAY", "WEATHER_DELAY", "NAS_DELAY", "CARRIER_DELAY", "LATE_AIRCRAFT_DELAY","DEP_TIME","ARR_TIME")]
    
    monday=monday[DAY_OF_WEEK==days[[input$Select_Day_of_the_Week]]]#
    
    
    monday=na.omit(monday)
    monday$total_delay=monday$SECURITY_DELAY+monday$WEATHER_DELAY+monday$NAS_DELAY+monday$CARRIER_DELAY+monday$LATE_AIRCRAFT_DELAY
    monday_melted = monday[,c("month", "DEP_TIME", "ARR_TIME")]
    monday_melted=melt(monday_melted,id='month')
    monday_melted$value<-apply(monday_melted[,c('value')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    monday_delay=monday[,c("month", "DEP_TIME", "total_delay")]
    monday_delay$DEP_TIME<-apply(monday_delay[,c('DEP_TIME')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
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
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      # geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      
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
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      # geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      #ylim(0,25)+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
     # scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      #ylim(0,25)+
      #stat_bin2d(bins = 25, colour = "white")+
      #stat_summary(fun.y = "sum", geom = "line") +
      #coord_trans(y = "log10") +
      #scale_colour_gradient(low = "#4d4dff", high = "#000066")+
      labs(x="", y="Hour") + theme(legend.position="none")
    Jul_gg2<-ggplot(Jul_monday_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
      #geom_point(aes(size=total_delay),shape=1,stroke=1.5)+
      geom_point(aes(size=total_delay,colour=total_delay),shape=1,stroke=1.5)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
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
    Month_df$month = format(Month_df$FL_DATE, '%b')
    day = Month_df[,c("month", "SECURITY_DELAY", "WEATHER_DELAY", "NAS_DELAY", "CARRIER_DELAY", "LATE_AIRCRAFT_DELAY","DEP_TIME","ARR_TIME")]
    day=na.omit(day)
    day$value<-apply(day[,c('DEP_TIME')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    if(input$delay=="SECURITY DELAY"){
      day=day[SECURITY_DELAY>0]
      ggplot(day, aes(x = factor(month, levels = month.abb), y = DEP_TIME/100)) +
        geom_point(aes(colour = SECURITY_DELAY,size=SECURITY_DELAY/10+20),shape=1,stroke=3)+
        scale_y_continuous(breaks = seq(0, 24, by = 1))+
        scale_colour_gradient(low = "#EF597B", high = "red")+
        labs(x="2017 Months", y="Hour")}
    else if(input$delay=="WEATHER DELAY"){
      day=day[WEATHER_DELAY>0]
      ggplot(day, aes(x = factor(month, levels = month.abb), y = DEP_TIME/100)) +
        geom_point(aes(colour = WEATHER_DELAY,size=WEATHER_DELAY/10+20),shape=1,stroke=3)+
        scale_y_continuous(breaks = seq(0, 24, by = 1))+
        scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
        labs(x="2017 Months", y="Hour")}
    else if(input$delay=="NAS DELAY"){
      day=day[NAS_DELAY>0]
      ggplot(day, aes(x = factor(month, levels = month.abb), y = DEP_TIME/100)) +
        geom_point(aes(colour = NAS_DELAY,size=NAS_DELAY/10+20),shape=1,stroke=3)+
        scale_y_continuous(breaks = seq(0, 24, by = 1))+
        scale_colour_gradient(low = "#73B66B", high = "#006400")+
        labs(x="2017 Months", y="Hour")}
    else if(input$delay=="CARRIER DELAY"){
      day=day[CARRIER_DELAY>0]
      ggplot(day, aes(x = factor(month, levels = month.abb), y = DEP_TIME/100)) +
        geom_point(aes(colour = CARRIER_DELAY,size=CARRIER_DELAY/10+20),shape=1,stroke=3)+
        scale_y_continuous(breaks = seq(0, 24, by = 1))+
        scale_colour_gradient(low = "#FFCB18", high = "#666600")+
        labs(x="2017 Months", y="Hour")}
    else if(input$delay=="LATE AIRCRAFT DELAY"){
      day=day[LATE_AIRCRAFT_DELAY>0]
      ggplot(day, aes(x = factor(month, levels = month.abb), y = DEP_TIME/100)) +
        geom_point(aes(colour = LATE_AIRCRAFT_DELAY,size=LATE_AIRCRAFT_DELAY/10+20),shape=1,stroke=3)+
        scale_y_continuous(breaks = seq(0, 24, by = 1))+
        scale_colour_gradient(low = "#29A2C6", high = "#000066")+
        labs(x="2017 Months", y="Hour")}
    
    
    
  })
  
  output$one_day <- renderPlot({
    Month_df$month = format(Month_df$FL_DATE, '%b')
    day=Month_df[Month_df$FL_DATE==input$date]
    day = day[,c("month", "SECURITY_DELAY", "WEATHER_DELAY", "NAS_DELAY", "CARRIER_DELAY", "LATE_AIRCRAFT_DELAY","DEP_TIME","ARR_TIME")]
    day=na.omit(day)
    day$total_delay=day$SECURITY_DELAY+day$WEATHER_DELAY+day$NAS_DELAY+day$CARRIER_DELAY+day$LATE_AIRCRAFT_DELAY
    day_melted = day[,c("month", "DEP_TIME", "ARR_TIME")]
    day_melted=melt(day_melted,id='month')
    day_melted$value<-apply(day_melted[,c('value')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    day_delay=day[,c("month", "DEP_TIME", "total_delay")]
    day_delay$DEP_TIME<-apply(day_delay[,c('DEP_TIME')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    gg1<-ggplot(day_melted, aes(x = variable, y = value/100)) +
      geom_point(aes(colour = variable), size = 3, shape=1,stroke=3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      
      labs(x="", y="Hour") + theme(legend.position="none")
    gg2<-ggplot(day_delay, aes(x = "Total Delay", y = DEP_TIME/100)) +
      geom_point(aes(size=total_delay,colour=total_delay),  shape=1,stroke=3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      scale_colour_gradient(low = "#FF6D31", high = "#9C2A00")+
      labs(y="",x="") + theme(legend.position="none")+
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    grid.arrange(gg1,gg2,ncol=2,top=input$date,widths=c(2,1))
    
  })
  output$airline_200 <- renderPlot({
    Month_df$month = format(Month_df$FL_DATE, '%b')
    Month_delay = Month_df[,c("month","FL_NUM", "ARR_TIME","DEP_TIME")]
    Month_delay=Month_delay[Month_delay$FL_NUM==input$Flight_No]#
    Month_delay = Month_delay[,c("month", "ARR_TIME","DEP_TIME")]
    Month_delay=melt(Month_delay,id='month')
    Month_delay=na.omit(Month_delay)
    #Month_delay$value=(Month_delay$value*0)+getValue(Month_delay$value)
    Month_delay$value<-apply(Month_delay[,c('value')],MARGIN = 1 ,FUN=function(x2) {ifelse(x2==2400, 2400, getValue(x2))})
    
    gg1<-ggplot(Month_delay, aes(x = factor(month, levels = month.abb), y = value/100)) +
      geom_point(aes(colour = variable,size=1),fill = "white", size = 3, shape=1,stroke=3)+
      scale_y_continuous(breaks = seq(0, 24, by = 1))+
      expand_limits( y=c(0, 24))+
      labs(title = "")
    labs(x="2017 Months", y="Hour")
    grid.arrange(gg1,ncol=1)
    
  })
  ###################PART GRAD BEGINS HERE
  unitChoice <- reactive({
    input$units
  })
  
  sliderValues <- reactive({
    input$range
  })

  output$distance_range_plot <- renderPlot({
    print(unitChoice())
    
    if (unitChoice() == "miles") {
    dist_min = sliderValues()[1]
    dist_max = sliderValues()[2]
    }
    else {
    dist_min = sliderValues()[1] / 1.609
    dist_max = sliderValues()[2] / 1.609
    }

    dist_values = Month_df[(Month_df$DISTANCE >= dist_min) & (Month_df$DISTANCE <= dist_max)]$DISTANCE
    dist_count = data.frame(label = "number of flights", dist_count = dist_values)
    options(scipen = 999)
    ggplot(dist_count, aes(x = label)) +
      ylab("Number of Flights") +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      geom_bar() +
      ylim(0, dim(Month_df)[1]) +
      coord_flip()
  })
  
  sliderValues2 <- reactive({
    input$time_range
  })
  
  output$time_range_plot <- renderPlot({
    
    time_min = sliderValues2()[1]
    time_max = sliderValues2()[2]
    
    time_values = Month_df[(Month_df$AIR_TIME >= time_min) & (Month_df$AIR_TIME <= time_max)]$AIR_TIME
    time_count = data.frame(label = "Number of flights", time_count = time_values)
    options(scipen = 999)
    ggplot(time_count, aes(x = label)) +
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

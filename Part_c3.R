# sample R + Shiny example for CS 424 Spring 2018 UIC - Andy Johnson
# www.evl.uic.edu/aej/424

# This is a sample dashboard making use of the evl room temperature data and displaying
# it in a variery of ways to show off some of the different capabilities of R and Shiny
# and the Shiny Dashboard.

#libraries to include

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

##  "13930","Chicago, IL: Chicago O'Hare International"   
### "13232","Chicago, IL: Chicago Midway International"  
portdir=read.csv("L_AIRPORT_ID.csv")
Jan=read.csv("Jan.csv")
Feb=read.csv("Feb.csv")
Mar=read.csv("Mar.csv")
Apr=read.csv("Apr.csv")
May=read.csv("May.csv")
June=read.csv("June.csv")
July=read.csv("July.csv")
Aug=read.csv("Aug.csv")
Sept=read.csv("Sept.csv")
Oct=read.csv("Oct.csv")
Nov=read.csv("Nov.csv")
Dec=read.csv("Dec.csv")
Month=list(Jan,Feb,Mar,Apr,May,June,July,Aug,Sept,Oct,Nov,Dec)
Monthnames=c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG","SEPT","OCT","NOV","DEC")

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
      selectInput("month", "Month", c("JAN","FEB","MAR","APR","MAY","JUN","JULY","AUG","SEPT","OCT","NOV","DEC"))
      ),
      
      fluidRow(
        tabPanel("AirlineFlightPlot",box( title = "AirLine flights", solidHeader = TRUE, status = "primary", width = 10, plotOutput("AirlineFlightPlot",width="450px",height="450px")) ),
        tabPanel("AirlineFlightTable", box(title = "Airline Flights Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("AirlineFlightTable"))  ),
        tabPanel("HourlyFlights", box(title = "Airline Hourly Flights", solidHeader = TRUE, status = "primary", width = 8, plotOutput("HourlyFlights"))  )
        
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
      )
      

  )
)

server <- function(input, output) {
  
theme_set(theme_grey(base_size = 17)) 
  
  
output$AirlineFlightPlot <- renderPlot({

  
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


output$HourlyFlights<- 
  
  renderPlot({
    Month=Month[Monthnames ==input$month]
    Month=Month[[1]]
    Airline=Month[Month$ORIGIN_AIRPORT_ID==input$Airport,]
    arrtimes=Airline$ARR_TIME
    dep=Airline$DEP_TIME
    arrtimes <- as.character(unlist(Airline$ARR_TIME))
    deptimes <- as.character(unlist(Airline$DEP_TIME))
    arrivals=list()
    departures=list()
    
    
    arr=arrtimes[nchar(arrtimes)<3 & !is.na(arrtimes)]
    arrivals[1]=length(arr)
    dep=deptimes[nchar(deptimes)<3 & !is.na(deptimes)]
    departures[1]=length(dep)
    
    
    
    
    for (hour in 1:9)
    {
      h=toString(hour)
      
      arr=arrtimes[startsWith(arrtimes,h) & nchar(arrtimes)==3 & !is.na(arrtimes)]
      arrivals[hour+1]=length(arr)
      
      print(arrivals[hour+1])
      
      dep=deptimes[startsWith(deptimes,h) & nchar(deptimes)==3 & !is.na(deptimes)]
      departures[hour+1]=length(dep)
    }
    
    
    for (hour in 10:24)
    {
      h=toString(hour)
      
      arr=arrtimes[startsWith(arrtimes,h) & nchar(arrtimes)==4 & !is.na(arrtimes)]
      arrivals[hour+1]=length(arr)
      
      
      dep=deptimes[startsWith(deptimes,h) & nchar(deptimes)==4 & !is.na(deptimes)]
      departures[hour]=length(dep)
    }
    
    arrivals=unlist(arrivals)
    departures=unlist(departures)
    
    arrivals[1]=arrivals[1]+arrivals[25]
    departures[1]=departures[1]+departures[25]
    
    
    arrivals=arrivals[c(1:24)]
    departures=departures[c(1:24)]
    times=c(1:24)
    
    
    TravelTimes=data.frame(Times=times,Arrivals=arrivals,  Departures=departures) 
    
    
    ggplot(TravelTimes, aes(x=Times))+labs(y="# Flights",x = "Times") + 
      geom_point(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
      geom_point(aes(y = TravelTimes[[3]], colour = "Departures",group=1))+
      geom_line(aes(y = TravelTimes[[2]], colour = "Arrivals",group=1))+
      geom_line(aes(y = TravelTimes[[3]], colour = "Departures",group=1))
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

}

shinyApp(ui = ui, server = server)

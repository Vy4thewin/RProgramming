# Title: Data 608 Final
# By: Vyanna Hill
# Created: 4/30/23


#library used--
library(shiny)
library(tidyverse)
library(jsonlite)
library(stringr)
library(plotly)
library(shinythemes)
library(ggplot2)
#----

# Data handling ---

#Pulling in our data sets for the final
#To reduce the size of our pulls, we will create a query to pull the statistics we need

#For Regents data, let's create a query to reduce the rows return so we can utilize the entire dataset
#we need dbn #, school year, regent type, school year, total tested, sum of the totals for ( tested 65% and below, tested 65% and above, Tested above 80%, )
regents<-fromJSON("https://data.cityofnewyork.us/resource/bnea-fu3k.json?$select=Year,school_dbn,school_level,regents_exam,total_tested,mean_score,number_scoring_below_65,number_scoring_65_or_above,number_scoring_80_or_above,number_scoring_cr&$limit=33031")

#We noticed that we could not aggregate for the averages in the query as the data set has inputted NAs in the values
#Looking at the data, s in this set shows that the totals scores were not identified by its grade, so we can interpret this as NAs for removal
#For our scope on college readiness, we will filter out any exams that does not qualify to engage college readiness
c_regents<-regents%>%filter(!grepl("s",mean_score))%>%filter(!grepl("na",number_scoring_cr))

#Reduce the scope to kids that were currently attending High school, limit to only school levels that are specifically in 9th-12th school year
c_regents<-c_regents%>%filter(school_level %in% c("Secondary School","High school"))

#Now we can re-class the variables of all columns to compute our mutations
c_regents<-type_convert(c_regents)

#finding the percentages of the scores listed
c_regents<-c_regents%>%mutate(per_below_65=number_scoring_below_65/total_tested,per_above_65=number_scoring_65_or_above/total_tested,per_above_80=number_scoring_80_or_above/total_tested,college_readiness_rate=number_scoring_cr/total_tested)


#To limit misleading data, we will do a large pull of the available data and process it to our likening
url<-'https://data.cityofnewyork.us/resource/vww9-qguh.json?$select=year,dbn,grade,total_days,days_present&$group=year,dbn,grade,total_days,days_present&$limit=798411'
attendance<-fromJSON(url)

#removed un-accounted data from the table and 
c_attendance<-attendance%>%filter(!grepl("s",days_present))

#Reduce the scope to kids that were currently attending High school, limit to only school levels that are specifically in 9th-12th school year
c_attendance<-c_attendance%>%filter(grade %in% c("9","10","11","12"))

#Regents only take place at the second half of the school year, transform the year value to reflect
c_attendance<-c_attendance%>%mutate(year=replace(year,year=="2013-14","2014"))%>%mutate(year=replace(year,year=="2014-15","2015"))%>%mutate(year=replace(year,year=="2015-16","2016"))%>%mutate(year=replace(year,year=="2016-17","2017"))%>%mutate(year=replace(year,year=="2017-18","2018"))%>%mutate(year=replace(year,year=="2018-19","2019"))

#Convert types
c_attendance<-type_convert(c_attendance)
c_attendance<-c_attendance%>%rename(school_dbn=dbn)%>%rename(Year=year)

#Taking the average of the high school's attendance across the grades
c_attendance<-c_attendance%>%group_by(Year,school_dbn)%>%summarise(avg_attendance=sum(days_present)/sum(total_days))

#combining the two data sets by dbn with a inner join
combine_school<-c_attendance%>%inner_join(c_regents,by=c("Year","school_dbn"))


#For our heap map, lets retrieve locational data for dbns
school_location<-fromJSON("https://data.cityofnewyork.us/resource/a3nt-yts4.json?$select=the_geom,ats_code,city&$limit=1992")

#Combining the locational data by the dbn
loc_combine<-combine_school%>%inner_join(school_location,by=c("school_dbn"="ats_code"))
loc_combine$city<-str_to_title(loc_combine$city)

#Cannot go super detail by tract data with geojson sadly as there a variance in specificity with the available geojson with the provided resources, long island city/jamaica/basides will belong under queens
loc_combine<-loc_combine%>%mutate(city=replace(city,city=="Jamaica","Queens"))%>%mutate(city=replace(city,city=="Long Island City","Queens"))%>%mutate(city=replace(city,city=="Bayside","Queens"))%>%mutate(city=replace(city,city=="New York","Manhattan"))

#Last step, group all the borough data for our map
plot_user<-loc_combine%>%group_by(Year,city,avg_attendance)%>%summarise(college_readiness_rate=mean(college_readiness_rate))

#Using a different json package for geojson data---
library(rjson)
#----


#setting up the geo data following the setup in the plotly documentation, found a NYC geojson data set
geo<-rjson::fromJSON(file='https://raw.githubusercontent.com/codeforgermany/click_that_hood/main/public/data/new-york-city-boroughs.geojson')


#----



ui<-fluidPage(
  theme=shinytheme("cosmo"),
  
  titlePanel("Data 608 Final"),
  
  #Sidebarlayout-> where we will select the graph in need
  sidebarLayout(
    
    sidebarPanel(
      br(),
      h3("Visualize the effects of Attendance in College Readiness"),
      p("Select the Year and Attendance rate to view the results by Borough"),
      br(),
      
      sliderInput("yr_select", "Year",
                  min = 2015, max = 2019,
                  value = 2015),
      
      sliderInput("avg_att", "Attendance Rate",
                  min = 0.48, max = 1.00,
                  value = c(0.01,1.00))
      
    ),
    
    
    #main panel-> our body
    mainPanel(
      br(),
      br(),
      #plotlyOutput("fplot")
      tabsetPanel(type="tabs",
                  tabPanel("Borough Map",plotlyOutput("fplot"))
                  )
    )
  
  )
  
)

#Server side, the back-end of the app where each interaction prompts a function
server<- function(input,output){
  #filtering our data set with the year of the scoring and its attendance range
  get_data<-reactive({plot_user%>%filter(Year==input$yr_select)%>%filter(between(avg_attendance,input$avg_att[1],input$avg_att[2]))%>%summarise(college_readiness=mean(college_readiness_rate))})

  output$fplot<-renderPlotly({
    #Calling our user inputted data range
    data<-get_data()
    
    #Following the Plotly's documentation on geojson data with choropleth maps: Cited here https://plotly.com/r/choropleth-maps/
    g <- list(fitbounds = "locations",
              visible = FALSE)
    
    #creating the plotly graph
    fig <- plot_ly()
    
    #The figure with create the borough map using the geojson data provided earlier and overlaying the college readiness score calculate from the user's input
    #%h3
    fig <- fig %>% add_trace(
      type = "choropleth",
      geojson = geo,
      locations = data$city,
      text=~paste(data$college_readiness,"Overall Score of readiness"),
      z = data$college_readiness,
      colorscale = "Greens",
      featureidkey = "properties.name")
    
    fig <- fig %>% layout(geo = g)
    
    fig<-fig%>%colorbar(title="College Readiness Rate")%>%layout(title=" College Readiness rate throughout 2015-2019")
    
  })
  output$summary<-renderText({})
}




shinyApp(ui=ui,server=server)


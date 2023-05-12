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

# Data handling PT.2 ---

#I used a helper function to reduce the pull load on the app for it to deploy. Please refer to my dfinal_helper script for the first part of the handling process
##https://github.com/Vy4thewin/RProgramming/blob/Data-608/dfinal_helper.R

combine_school<-read.csv("https://raw.githubusercontent.com/Vy4thewin/RProgramming/Data-608/D608_Final/combine_school.csv")

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
    
    #Mapping the gejson data to our map
    fig <- fig %>% layout(geo = g)
    
    fig<-fig%>%colorbar(title="College Readiness Rate")%>%layout(title=" College Readiness rate throughout 2015-2019")
    
  })
  output$summary<-renderText({})
}




shinyApp(ui=ui,server=server)


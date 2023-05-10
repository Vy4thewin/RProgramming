# Title: Creating a shiny app to analyze data
# By: Vyanna Hill
# Created: 2/12/23


#library used--
library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
library(ggplot2)

#----
  
#downloading the data from the module folder
data<-read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv",header = TRUE)

#Using a additional source to help coding my first shiny app.https://www.youtube.com/watch?v=jxsKUxkiaLI&list=WL&index=1&ab_channel=BusinessScience


#Ui section of the app, our public facing face where we interact with the app

ui<-fluidPage(
  theme=shinytheme("cosmo"),
  
  titlePanel("Reactivity visualization with R"),
  
  #Sidebarlayout-> where we will select the graph in need
  sidebarLayout(
  
  
    sidebarPanel(
      
      helpText("Visualizing the Morality rate"),
      
      #Selecting the type of graph available 
      selectInput("var",label="Select the visualization",
                  choices=list("Crude Mortality in the US 2010","Morality Rate By State"),
                  selected="Morality Rate By State"
                  ),
      
      #Selecting the cause of death 
      radioButtons("cat",label="Select Cause of Mortality",
                  choices=c(unique(data$ICD.Chapter)),
                  selected="Certain infectious and parasitic diseases"
      )
                    
                    ),
    
    #main panel-> our body
    mainPanel(
              h1("Mortality rates across the US",align="center"),
              h3("By Vyanna Hill",align="center"),
              plotlyOutput("fplot")
              
              
              )
    )
)

#Server side, the back-end of the app where each interaction prompts a function

server<- function(input,output){
  #We can change response of the app by input using reactive
  
  #When user selects drop down, either run the bar graph or the line graph
  gselect<-reactive({
   switch(input$var,"Crude Mortality in the US 2010"="g1","Morality Rate By State"="g2")
  })
  
  #Looking at user input, filter by death cause
  icdSelect<-reactive({data%>%filter(ICD.Chapter==input$cat)})
  
  #When renderplotly runs, it it will check the last response of the user and change the graph by input
  #We label any functions by output$varname as in the UI, it can be call by varname
  output$fplot<-renderPlotly({
        choice<-gselect()
        if(choice=="g1"){
          #If user selects crude mortality 2010, It will filter the data set by cause of death and then year
          d1<-icdSelect()
          d1<-d1%>%filter(Year=="2010")
          
          
          #Using plotly, create a bar graph and group crude mortality by cause of death
          #To see and select the cause of death, we need to shift location of the legend to below the graph
          fig <- plot_ly(data = d1, x = ~State, y = ~Crude.Rate, type = "bar",color=~ICD.Chapter)
          fig <- fig %>% layout(title="Crude Mortality Rate By Cause in 2010",legend = list(orientation = 'h',xanchor="center",x=0.5,xaxis = list(categoryorder = "total descending")))
        }else{
          #Filtering by user's input on cause
          d1<-icdSelect()
          
          #Creating a new column for national average crude mortality, From the CDC page https://www.cdc.gov/csels/dsepd/ss1978/lesson3/section3.html
          #crude mortality rate= (all deaths/ living population) x 100,000 = x deaths per 100K population
          d1<-d1%>%group_by(Year,ICD.Chapter)%>%mutate(nat_mort=(sum(Deaths)/sum(Population))*10^5)
          
          #For the line graph, we must calculate the average rate to plot
          #d1, for plotting rates by year and state. d2, national average
          d1<-d1%>%group_by(Year,State)%>%mutate(state_mort=mean(Crude.Rate))
          d2<-d1%>%group_by(Year)%>%mutate(nat_mort=mean(nat_mort))
          
          #layer the state plots over the national data for a comparison
          fig<-ggplot()+geom_line(data=d1,aes(x=Year,y=state_mort,colour=State))+geom_line(data=d2,aes(x=Year,y=nat_mort),colour="black",linetype = 2)
          ggplotly(fig)
          
        }
      
  })

 
}

shinyApp(ui=ui,server=server)



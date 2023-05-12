# Title: Data 608 Final Helper Function
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


#Separating the data cleaning process into two steps to reduce the app load time


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

#Creating a file to load our combined data set to pull in the app for a faster load
write_csv(combine_school,"C:/Users/walki/Documents/GitHub/RProgramming/D608_Final/combine_school.csv")
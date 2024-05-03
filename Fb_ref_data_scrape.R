####Installing packages####
install.packages("netstat")
library(tidyverse)#For data Cleaning
library(dplyr)#Data Wrangling
library(stringr)#String Manipulation
#library(purrr)
library(rvest)#static webpage scraping
library(RSelenium)#dynamic scraping
library(wdman)
library(netstat)#find unused port
library(data.table)#for the rbindlist function
####Settting up the server####
wdman::selenium()
chrome()
binman::list_versions("chromedriver")
chromecommmand<-chrome(retcommand = T,verbose = F,check = F)
chromecommmand
rs_driver_object<-rsDriver(browser = "chrome",chromever = "104.0.5112.79",verbose = F,port = free_port())
remDr<-rs_driver_object$client
remDr$open()
####Getting the table from the page####
remDr$navigate("https://fbref.com/en/comps/Big5/gca/players/Big-5-European-Leagues-Stats")
data_table<-remDr$findElement(using='id',"stats_gca")#Using the tableid to pick the tables in the page
data_table_html<-data_table$getPageSource()
data_table_html
page<-read_html(data_table_html%>%unlist())
df<-html_table(page,header = T,trim = 1)
df
####Choosing our specified table####
df1<-df[[11]]
df1
####Cleaning the table####
names(df1)<-df1[1,]
df1<-df1[-1,]
names(df1)<-make.unique(names(df1))
####Closing the server and freeing the port for next use####
remDr$close()
rD$server$stop()
rm(rD, remDr)
gc()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

#Cleaning_Data####
Top_5<-df1

typeof(Top_5$SCA)
Top_5<-Top_5%>%mutate(Squad=str_replace(Squad,pattern ="^\\w{2,3} ",replacement = "" ))
Top_5<-Top_5%>%mutate(Comp=str_replace(Comp,pattern ="^\\w{2,3} ",replacement = "" ))
Top_5<-Top_5%>%mutate(Pos=str_replace(Pos,"MF,FW","MF"))
#Repeating this for FW/MF,MF/DF,DF/MF Classification
Top_5<-Top_5%>%mutate(Pos=str_replace(Pos,"FW,MF","MF"))
Top_5<-Top_5%>%mutate(Pos=str_replace(Pos,"MF,DF","MF"))
Top_5<-Top_5%>%mutate(Pos=str_replace(Pos,"DF,MF","MF"))
#Filtering out midfielders only
Top_5<-Top_5%>%filter(Pos=="MF")
#Ordering the data for top 30 since we can not plot all the players 
Top_5<-Top_5[order(as.numeric(Top_5$GCA),as.numeric(Top_5$SCA),decreasing = T),]
#Removing the rest of the players
Top_5_1<-Top_5[1:26,]
Top_5_1<-Top_5_1%>%mutate(SCA=as.numeric(SCA),GCA=as.numeric(GCA))
cor(as.numeric(Top_5$SCA),as.numeric(Top_5$GCA),use = "complete.obs")
x<-lm(Top_5_1$GCA~Top_5_1$SCA)

#view(Top_5)

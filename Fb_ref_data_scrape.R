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
#plotting####
library(ggrepel)
Palette<-distinctColorPalette(26)
Top5_Final<-Top_5_1%>%ggplot(aes(x=SCA,y=GCA,label=Player))+
  xlab("SCA")+ylab("GCA")+
  geom_hline(yintercept=mean(Top_5_1$GCA),linetype="dotted",col="yellow")+
  geom_vline(xintercept=mean(Top_5_1$SCA),linetype="dotted",col="yellow")+
  geom_smooth(method = "lm",color="white",fill="grey25")+
  geom_point(aes(fill=Palette,color= after_scale(clr_darken(fill,0.3))),shape=23,alpha=.75,size=3)+
  geom_text_repel(size=2.5,color="white",min.segment.length = unit(0.1,"lines"),fontface="bold" )+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill="grey10",colour ="grey10"),
    panel.background = element_rect(fill="grey10",colour="grey10"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "white"),
    axis.text = element_text(colour = "white"),
    axis.title = element_text(colour = "white"),
    plot.title = element_text(colour = "white",hjust=.5,face = "bold",family ="serif",size=15),
    plot.subtitle = element_text(colour = "white",hjust=.5,face = "bold",size=8))+
  labs(title="Shot Creating action VS Goal creating action(Midfielders)", subtitle = "Top 5 Leagues 2023-24")
Top5_Final
ggsave("Top_5.png",Laliga_Final,width = 11,height = 6 ,dpi = 300)

#####unused label part####
#geom_label(x=130,y=19.1,label="High Shot & Goal\n Creating Action",label.size = 0.10,
 #          color = "white",
  #         fill="gold")+
  #geom_label(x=130,y=2.6,label="High Shot Creating\n Actions",label.size = 0.10,
   #          color = "white",
    #         fill="gold")+
  #geom_label(x=80,y=19.1,label="High Goal Creating\n Actions",label.size = 0.10,
   #          color = "white",
          #   fill="gold")+#
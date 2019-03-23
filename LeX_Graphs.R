library(data.table)
library(ggplot2)
library(clustMixType)
library(cluster)
library(factoextra)
library(ggplot2)
library(dplyr)
library(caret)
library(cluster)
library(klaR)
library(clustMixType)
library(data.table)
library(factoextra)
library(tidyr)
library(Rtsne)
library(compareGroups)
library(gridExtra)
library(reshape)
library(purrr)
library(rworldmap)



## Read csv
dt = fread("WPP2017_Period_Indicators_Medium.csv")

## Get desired columns
dt = dt[,.(Location,Time,LEx,LExMale,LExFemale)]

## Cast year
dt[,Time:=apply(dt[,.(Time)],1,function(x){as.numeric(unlist(strsplit(x,"-"))[2])})]

# Remove rows with any null
dt = dt[complete.cases(dt),]

# Transpose table
dt_trans = dcast(dt, Location ~ Time  , value.var = "LEx")


## Get numeric table (Remove location)
dt_numeric = copy(dt_trans)[,Location:=NULL]

## Transform table, get diff between consecutive periods
f <- function (x){diff(x)}
cols <- tail(colnames(dt_numeric),-1)
dt_trend <- apply(dt_numeric,1, f)
dt_trend <- data.table(t(dt_trend))

## Get countries with very negative tendencies
indices <- data.frame(which(dt_trend <= -5, arr.ind=T))$row
# rows_with_neg <- apply(dt_trend, 1, function(r) any(r<0))

## Countries with highest negative trends
countries_neg_trends <- dt_trend[indices,]
countries_neg_trends$min_trend <- apply(countries_neg_trends,1,min)
countries <- dt_trans[indices, ]
countries$min_trend <- countries_neg_trends$min_trend
top_neg_countries <- countries[order(min_trend)]


plot_country_lex <- function (dt, location){
  country <- dt[Location==location & Time<2020,.(Time,LEx)]
  min_lex <- as.numeric(10*floor(min(country[,.(LEx)])/10))
  max_lex <- as.numeric(10*ceiling(max(country[,.(LEx)])/10))
  ggplot(data=country)+
    geom_line(data=country,aes(x=Time,y=LEx),group=1,size=1.5) +
    xlab("Year") + ylab("Life Expectancy")+
    theme(plot.title = element_text(face="bold", size=20))+
    theme(axis.title.x = element_text(size=18)) +
    theme(axis.title.y = element_text(size=18)) +
    theme(axis.text.x = element_text(size=18))+
    theme(axis.text.y = element_text(size=18)) +
    scale_x_continuous(breaks=seq(1950, 2020, by = 5))+
    scale_y_continuous(breaks=seq(0,100, by = 10)) +
    #geom_point(size=2)+
    geom_point(data=country[LEx==min(LEx)], aes(x=Time, y=LEx), colour="red", size=5)+
    #geom_ribbon(aes(ymin=min_lex, ymax=LEx, colour = "slateblue"))+
    guides(linetype=F)+
    ggtitle(paste("Life Expectancy in",location))
  

}


plot_country_gender_lex <- function (dt, location){
  country <- dt[Location==location & Time<2020,.(Time,LEx,LExMale,LExFemale)]
  min_lex <- as.numeric(10*floor(min(country[,.(LEx,LExMale,LExFemale)])/10))
  max_lex <- as.numeric(10*ceiling(max(country[,.(LEx,LExMale,LExFemale)])/10))
  country <- melt(country,id.vars="Time")
  ggplot(country, aes(Time,value, col=variable)) + 
    geom_line(size=1.5) +
    xlab("Year") + ylab("Life Expectancy")+
    theme(plot.title = element_text(face="bold", size=20))+
    theme(axis.title.x = element_text(size=18)) +
    theme(axis.title.y = element_text(size=18)) +
    theme(axis.text.x = element_text(size=18))+
    theme(axis.text.y = element_text(size=18)) +
    theme(legend.text=element_text(size=16)) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    scale_x_continuous(breaks=seq(1950, 2020, by = 5))+
    scale_y_continuous(breaks=seq(min_lex,max_lex, by = 10)) +
    guides(linetype=F)+
    ggtitle(paste("Life Expectancy in",location))
  
  
}

plot_country_lex(dt,"Rwanda")
plot_country_gender_lex(dt,"Rwanda")
#ggsave(paste0("Rwanda","_gender.png"),width=1192,height=649,limitsize = FALSE)




plot_countries_lex <- function (dt, locations){
  plt <- ggplot()
  for (loc in locations){
    country <- dt[Location==loc & Time<2020,.(Location,Time,LEx)]
    country_tail <- country[2:dim(country)[1]]
    
    plt <- plt + geom_line(data=country, aes(x=Time, y=LEx,col=Location),size=1.5) +
      geom_point(data=country_tail[LEx==min(LEx)], aes(x=Time, y=LEx), colour="black", size=5)
  }
  plt <- plt + geom_line(size=1.5)+
    xlab("Year") + ylab("Life Expectancy")+
    theme(plot.title = element_text(face="bold", size=20))+
    theme(axis.title.x = element_text(size=18)) +
    theme(axis.title.y = element_text(size=18)) +
    theme(axis.text.x = element_text(size=18))+
    theme(axis.text.y = element_text(size=18)) +
    theme(legend.text=element_text(size=16)) +
    theme(legend.title=element_text(size=16)) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    scale_x_continuous(breaks=seq(1950, 2020, by = 5))+
    scale_y_continuous(breaks=seq(0,100, by = 10)) +
    guides(linetype=F)+
    ggtitle("Life Expectancy")
  plt
}

plot_countries_lex(dt,head(top_neg_countries[,Location],5))










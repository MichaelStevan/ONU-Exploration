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

rows_to_remove = c("World","Africa","Eastern Africa","Middle Africa",
                   "Northern Africa", "Southern Africa","Western Africa",
                   "Asia","Eastern Asia","South-Central Asia","Central Asia", "Southern Asia",
                   "South-Eastern Asia", "Western Asia",
                   "Europe","Eastern Europe","Northern Europe",
                   "Southern Europe","Western Europe",
                   "Caribbean", "Latin America and the Caribbean", "Central America",
                   "South America","Northern America","Oceania",
                   "Australia/New Zealand","Melanesia","Micronesia","Polynesia",
                   "More developed regions","Less developed regions","Least developed countries",
                   "Less developed regions, excluding least developed countries",
                   "Less developed regions, excluding China", "High-income countries",
                   "Middle-income countries","Upper-middle-income countries",
                   "Lower-middle-income countries", "Low-income countries",
                   "Sub-Saharan Africa"
)

dt_trans = dt_trans[!Location %in% rows_to_remove]

## Get numeric table (Remove location)
dt_numeric = copy(dt_trans)[,Location:=NULL]

## Transform table, get diff between consecutive periods

cols <- tail(colnames(dt_numeric),-1)
dt_trend <- apply(dt_numeric,1, function (x){diff(x)})
dt_trend <- data.table(t(dt_trend))

## Get countries with very negative tendencies
indices_min <- unique(data.frame(which(dt_trend <= -5, arr.ind=T))$row)
# rows_with_neg <- apply(dt_trend, 1, function(r) any(r<0))

## Countries with highest negative trends
countries_neg_trends <- dt_trend[indices_min,]
countries_neg_trends$min_trend <- apply(countries_neg_trends,1,min)
countries_neg <- dt_trans[indices_min, ]
countries_neg$min_trend <- countries_neg_trends$min_trend
top_neg_countries <- countries_neg[order(min_trend)]


## Get countries with very positive tendencies
indices_max <- unique(data.frame(which(dt_trend >= 7, arr.ind=T))$row)
# rows_with_neg <- apply(dt_trend, 1, function(r) any(r<0))

## Countries with highest positive trends
countries_pos_trends <- dt_trend[indices_max,]
countries_pos_trends$max_trend <- apply(countries_pos_trends,1,max)
countries_pos <- dt_trans[indices_max, ]
countries_pos$max_trend <- countries_pos_trends$max_trend
top_pos_countries <- countries_pos[order(max_trend,decreasing = TRUE)]
top_pos_countries <- top_pos_countries[!Location %in% c("Rwanda","Cambodia","Zimbabwe","Timor-Leste","Botswana")]




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

#plot_country_lex(dt,"Germany")
#plot_country_gender_lex(dt,"Rwanda")
#ggsave(paste0("Rwanda","_gender.png"),width=1192,height=649,limitsize = FALSE)




plot_countries_min_lex <- function (dt, locations){
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


plot_countries_max_lex <- function (dt, locations){
  plt <- ggplot()
  for (i in seq(length(locations))){
    loc <- locations[i]
    country <- dt[Location==loc & Time<2020,.(Location,Time,LEx)]
    country_head <- country[1:dim(country)[1]-1]
    
    country_trend <- dcast(country, Location ~ Time  , value.var = "LEx")[,!"Location"]
    cols <- tail(colnames(country_trend),-1)
    country_trend <- apply(country_trend,1, function (x){diff(x)})
    country_trend  <- data.table(t( country_trend))
    index_max_increase <- which(country_trend == max(country_trend), arr.ind=T)[1,2] + 1

    plt <- plt + geom_line(data=country, aes(x=Time, y=LEx,col=Location),size=1.5) +
      geom_point(data=country[index_max_increase,], aes(x=Time, y=LEx), colour="black", size=5)
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

plot_countries_max_lex(dt,head(top_pos_countries[,Location],5))










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
dt = dt[,.(Location,MidPeriod,LEx)]

# Transpose table
dt_trans = dcast(dt, Location ~ MidPeriod  , value.var = "LEx")

# Remove rows with any null
dt_trans = dt_trans[complete.cases(dt_trans),]

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



countries_neg_trends <- dt_trend[indices,]
countries_neg_trends$min_trend <- apply(countries_neg_trends,1,min)
countries <- dt_trans[indices, ]
countries$min_trend <- countries_neg_trends$min_trend
countries <- countries[order(min_trend)]
#dt_trend <- dt_trend[,Location:= dt[,Location]]
#dt_trend_neg <- dt_trend[rows_with_neg,]

plot_country_lex <- function (dt, location){
  country <- dt[Location==location,.(MidPeriod,LEx)]
  
  ggplot(data=dt,aes(x=MidPeriod,y=LEx,col=Variant,linetype=gt2017))+
    geom_line(size=1.5)+
    theme_minimal()+
    guides(linetype=F)+
    ggtitle(paste(location,"'s Total Population by Variant"))
}

fviz_nbclust(dt_trend, kmeans, method = "wss")

# Get all negative trends
apply(dt_trend,1, function(row) any(row < 0))


ggplot(data=a, aes(x=year, y=value, group=1)) + geom_line()+ geom_point()

# Clustering
clusters <- kmeans(dt_trend, 6)
clusters$tot.withinss

## 6 clusters Camboya única en cluster 2 fila 33
dt$cluster <- clusters$cluster

#group<-compareGroups(cluster~.,data=dt_numeric)
#clustab<-createTable(group)
#print(clustab)

mapDevice('x11')
spdf <- joinCountryData2Map(dt, joinCode="NAME", nameJoinColumn="Location")
mapCountryData(spdf, nameColumnToPlot="cluster", addLegend=TRUE,
               catMethod="fixedWidth",numCats = 6,colourPalette="diverging")

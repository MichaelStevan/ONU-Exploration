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


dt = fread("WPP2017_Period_Indicators_Medium.csv")

dt[,`:=`(Location=factor(Location),Variant=factor(Variant),Time=as.Date(ISOdate(Time,12,31)))]

summary(dt)

dt = dt[,.(Location,MidPeriod,LEx)]

dt = dcast(dt, Location ~ MidPeriod  , value.var = "LEx")
dt = dt[complete.cases(dt),]


dt_numeric = copy(dt)[,Location:=NULL]
f <- function (x){diff(as.numeric(x))}
dt_trend <- apply(dt_numeric, 1, f)
dt_trend <- t(dt_trend)

fviz_nbclust(dt_trend, kmeans, method = "wss")

# Get all negative trends
apply(dt_trend,1, function(row) any(row < 0))


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

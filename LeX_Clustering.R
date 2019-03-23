library(data.table)
library(ggplot2)
library(factoextra)
library(ggplot2)
library(compareGroups)
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

max_val <- max(dt_trend)
min_val <- min(dt_trend)

dt_trend_norm <- apply(dt_trend,2,function(x){(x-min_val)/(max_val-min_val)})

## Optimal num of clusters
fviz_nbclust(dt_trend_norm, kmeans, method = "wss")



num_clusters <- 8
# Clustering
clusters <- kmeans(dt_trend_norm, num_clusters)
clusters$tot.withinss

## 6 clusters Camboya única en cluster 2 fila 33
dt$cluster <- clusters$cluster

#group<-compareGroups(cluster~.,data=dt_numeric)
#clustab<-createTable(group)
#print(clustab)

mapDevice('x11')
spdf <- joinCountryData2Map(dt, joinCode="NAME", nameJoinColumn="Location")
mapCountryData(spdf, nameColumnToPlot="cluster", addLegend=TRUE,
               catMethod="fixedWidth",numCats = num_clusters,colourPalette="diverging")

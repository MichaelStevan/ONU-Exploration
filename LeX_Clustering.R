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

# Remove rows of future prediction
dt_past = dt[Time<2020]

# Transpose table
dt_trans = dcast(dt_past, Location ~ Time  , value.var = "LEx")

rows_to_remove = c("World","Africa","Eastern Africa","Middle Africa",
                   "Northern Africa", "Southern Africa","Western Africa",
                   "Asia","Eastern Asia","South-Central Asia","Central Asia", "Southern Asia",
                   "South-Eastern Asia", "Western Asia",
                   "Europe","Eastern Europe","Northern Europe",
                   "Southern Europe","Western Europe",
                   "Caribbean", "Latin America and the Caribbean", "Central America",
                   "South America","Northern America","Oceania",
                   "Australia/New Zealand","Melanesia","Micronesia","Polynesia")

dt_trans = dt_trans[!Location %in% rows_to_remove]

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
#dt_trend_norm <- dt_trend

# Clustering
## Optimal num of clusters
f <- fviz_nbclust(dt_trend_norm, kmeans, method = "wss")
plot(f)

num_clusters <- 3

clusters <- kmeans(dt_trend_norm, num_clusters)
clusters$tot.withinss


fv <- fviz_cluster(clusters, geom = "point", data = dt_trend) +  ggtitle(paste("2D Cluster solution (k=", num_clusters, ")", sep=""))
plot(fv)

## 6 clusters Camboya única en cluster 2 fila 33
#dt$cluster <- clusters$cluster
dt_trend[,Location:=dt_trans[,Location]]
dt_trend[,cluster:=clusters$cluster]


plot_tendencies_by_cluster<- function(dt_trend,cluster){
  dt_trend[,cluster:=clusters$cluster]
  trend_cluster <- dt_trend[,!"Location"]
  trend_cluster <- dt_trend_mean_cluster[, lapply(.SD, mean), by=cluster]
  cnames <- colnames(trend_cluster)
  cnames <- cnames[cnames!="cluster"]
  melted <- melt(trend_cluster, measure.vars = cnames)
  melted$variable <- as.numeric(as.character(melted$variable))
  ggplot(melted, aes(variable,value, col=as.factor(cluster))) + geom_line(size=1.5)
  
}


plot_tendencies_by_cluster(dt_trend,cluster)
#group<-compareGroups(cluster~.,data=dt_trend, max.ylev=12, max.xlev = 21)
#clustab<-createTable(group)
#print(clustab)

mapDevice('x11')
spdf <- joinCountryData2Map(dt_trend, joinCode="NAME", nameJoinColumn="Location")
mapCountryData(spdf, nameColumnToPlot="cluster", addLegend=TRUE,
               catMethod="fixedWidth",numCats = num_clusters,colourPalette="diverging")


library(data.table)
library(ggplot2)
library(factoextra)
library(ggplot2)
library(compareGroups)
library(rworldmap)
library(clValid)
library(NbClust)


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

## Clustering sobre datos absolutos
max_val <- max(dt_numeric)
min_val <- min(dt_numeric)

## Normalize values
dt_clustering <- apply(dt_numeric,2,function(x){(x-min_val)/(max_val-min_val)})

## Optimal number of clusters
# fviz_nbclust(dt_clustering, kmeans, method = "wss")


# WSS
index <- 0
for (i in 2:10) {
  km.out <- kmeans(dt_clustering, i, nstar=5)
  #index[i] <- dunn(clusters=km.out$cluster, Data = dt_clustering, method = "euclidean")
  index[i-1] <- km.out$tot.withinss
}

## Silhouette, Dunn, DB
nb <- NbClust(dt_clustering, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,method="kmeans")
index <- data.frame(nb$All.index[,"Silhouette"])
colnames(index) <- "index"

ggplot(data=index,aes(x=seq(2,10),y=index)) + geom_line(color="cadetblue4",size=1.5) + 
  geom_point(color="cadetblue4",size=3) +
  theme_minimal() +
  xlab("Number of clusters k") + ylab("Silhouette")+
  theme(plot.title = element_text(face="bold", size=20))+
  theme(axis.title.x = element_text(size=16,colour="black")) +
  theme(axis.title.y = element_text(size=16, colour="black")) +
  theme(axis.text.x = element_text(size=16,colour="black"))+
  theme(axis.text.y = element_text(size=16, colour="black")) +
  scale_x_continuous(breaks=seq(2, 10, by = 1))+
  #scale_y_continuous(breaks=seq(0,100, by = 10)) +
  guides(linetype=F)+
  ggtitle("Silhouette")
  
num_clusters <- 5
clusters <- kmeans(dt_numeric, num_clusters,nstar=10)
dt_trans$cluster <- clusters$cluster


fv <- fviz_cluster(clusters, geom = "point", data = dt_numeric) +  ggtitle(paste("2D Cluster solution (k=", num_clusters, ")", sep=""))
plot(fv)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(num_clusters)


num_clusters <- 5
final_clustering <- fread("ClusteringDiscoveries/clustering_final_5.csv")
  
mapDevice('x11')
spdf <- joinCountryData2Map(final_clustering, joinCode="NAME", nameJoinColumn="Location")
mapCountryData(spdf, nameColumnToPlot="cluster", addLegend=TRUE,
               catMethod="fixedWidth",numCats = num_clusters,colourPalette=cols)





plot_tendencies_by_cluster<- function(dt_trans){
  trend_cluster <- dt_trans[,!"Location"]
  trend_cluster <- trend_cluster[, lapply(.SD, mean), by=cluster]
  cnames <- colnames(trend_cluster)
  cnames <- cnames[cnames!="cluster"]
  melted <- melt(trend_cluster, measure.vars = cnames)
  melted$variable <- as.numeric(as.character(melted$variable))
  ggplot(melted, aes(variable,value, col=as.factor(cluster))) + geom_line(size=1.5) +
    xlab("Year") + ylab("Life Expectancy") + labs(fill = "Cluster") +
    theme(plot.title = element_text(face="bold", size=20))+
    theme(axis.title.x = element_text(size=18)) +
    theme(axis.title.y = element_text(size=18)) +
    theme(axis.text.x = element_text(size=18))+
    theme(axis.text.y = element_text(size=18)) +
    theme(legend.text=element_text(size=16)) +
    theme(legend.title=element_text(size=16)) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    scale_x_continuous(breaks=seq(1950, 2020, by = 5))+
    scale_y_continuous(breaks=seq(30,100, by = 10)) +
    guides(linetype=F)+
    ggtitle("Life Expectancy by cluster")
}


plot_tendencies_by_cluster(final_clustering)
#group<-compareGroups(cluster~.,data=dt_trend, max.ylev=12, max.xlev = 21)
#clustab<-createTable(group)
#print(clustab)



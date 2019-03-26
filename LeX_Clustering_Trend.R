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

num_clusters <- 5

clusters <- kmeans(dt_trend_norm, num_clusters)
clusters$tot.withinss


## 6 clusters Camboya única en cluster 2 fila 33
#dt$cluster <- clusters$cluster
dt_trend[,Location:=dt_trans[,Location]]
dt_trend[,cluster:=clusters$cluster]


plot_tendencies_by_cluster<- function(dt_trend,cluster){
  dt_trend[,cluster:=clusters$cluster]
  dt_trend <- dt_trend[Location!="Cambodia"]
  trend_cluster <- dt_trend[,!"Location"]
  
  trend_cluster <- trend_cluster[, lapply(.SD, mean), by=cluster]
  cnames <- colnames(trend_cluster)
  cnames <- cnames[cnames!="cluster"]
  melted <- melt(trend_cluster, measure.vars = cnames)
  melted$variable <- as.numeric(as.character(melted$variable))
  ggplot(melted, aes(variable,value, col=as.factor(cluster))) + geom_line(size=1.5)+
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
    #scale_y_continuous(breaks=seq(0,30, by = 10)) +
    guides(linetype=F)+
    ggtitle("Life Expectancy by cluster")
  
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(num_clusters)

dt_plot <- dt_trend[Location!="Cambodia"]
plot_tendencies_by_cluster(dt_plot,cluster)
#group<-compareGroups(cluster~.,data=dt_trend, max.ylev=12, max.xlev = 21)
#clustab<-createTable(group)
#print(clustab)

mapDevice('x11')
spdf <- joinCountryData2Map(dt_trend, joinCode="NAME", nameJoinColumn="Location")
mapCountryData(spdf, nameColumnToPlot="cluster", addLegend=TRUE,
               catMethod="fixedWidth",numCats = num_clusters,colourPalette=cols)



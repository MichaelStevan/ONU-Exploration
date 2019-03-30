# We will clusterize urbanization rate per country and plot some relevant
# metrics and the results in a world map.
#
# Miguel Esteban
# miestgo@gmail.com 


library(data.table)
library(NbClust)
library(cluster)
library(factoextra)
library(rworldmap)

########################
# Read data
########################
dt_urban= fread('data/WUP2018-F02-Proportion_Urban.csv',skip=1)
setnames(dt_urban,c("Location",seq(1950,2050,5)))

########################
# Prepare data for clustering
########################

# Data cleaning:
remove_regions = c("World","More Developed regions","Less developed regions", 
                   "Least developed countries","High-income countries",
                   "Middle-income countries","Upper-middle-income countries",
                   "Lower-middle-income countries","More developed regions",
                   "Less developed regions, excluding least developed countries",
                   "Less developed regions, excluding China","AFRICA",
                   "Sub-Saharan Africa","Low-income countries",
                   "Africa","Eastern Africa","Middle Africa",
                   "Northern Africa", "Southern Africa","Western Africa",
                   "Asia","Eastern Asia","South-Central Asia","Central Asia", "Southern Asia",
                   "South-Eastern Asia", "Western Asia",
                   "Europe","Eastern Europe","Northern Europe",
                   "Southern Europe","Western Europe",
                   "Caribbean", "Latin America and the Caribbean", "Central America",
                   "South America","Northern America","Oceania",
                   "Australia/New Zealand","Melanesia","Micronesia","Polynesia")

dt_urban = dt_urban[!Location %in% remove_regions]
dt_urban_location = dt_urban[,.(Location)]
# Replace commas, turn into numeric values
dt_numeric = apply(dt_urban[,.SD,.SDcols=!"Location"],2,function(x){as.numeric(gsub(",",".",x))})
dt_urban = copy(cbind(dt_urban_location,dt_numeric))

# Normalize, mean = 0, sd = 1 per column
dt_numeric = scale(dt_numeric)

########################
# Choose optimal cluster number
########################

fviz_nbclust(dt_numeric, kmeans, method = "wss")
fviz_gap_stat(clusGap(dt_numeric,kmeans,K.max=8))

nb  = NbClust(dt_numeric, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,method="kmeans")
index  = data.frame(nb$All.index[,"Silhouette"])
colnames(index)  = "index"

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
  guides(linetype=F)+
  ggtitle("Silhouette")

########################
# Perform clustering
########################

num_clusters = 4
clusters = kmeans(dt_numeric, num_clusters,nstar=10)
dt_urban$cluster = clusters$cluster


fv = fviz_cluster(clusters, geom = "point", data = dt_numeric) +  
  ggtitle(paste("2D Cluster solution (k=", num_clusters, ")", sep="")) +
  theme_minimal()
plot(fv)

########################
# Plot Clusters per Country
########################

dt_to_map = copy(dt_urban)[,.(Location,cluster)]

gg_color_hue  = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(num_clusters)

dev.off()
spdf  = joinCountryData2Map(dt_to_map, joinCode="NAME", nameJoinColumn="Location")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(spdf, nameColumnToPlot="cluster",
               mapTitle = "Urbanization Clustering per Country",
               addLegend=T,
               catMethod="fixedWidth",numCats = num_clusters,colourPalette=cols)



plot_tendencies_by_cluster = function(dt){
  trend_cluster = dt[,!"Location"]
  mean_trend_cluster = trend_cluster[, lapply(.SD, mean), by=cluster]
  
  ## Confidence interval on the mean
  st_error_mean = trend_cluster[, lapply(.SD, function(x){sd(x)/sqrt(length(x))}), by=cluster]
  
  std_trend_cluster = trend_cluster[, lapply(.SD, sd), by=cluster]
  std_trend_cluster = trend_cluster[, lapply(.SD, function(x){qnorm(0.975)*sd(x)/sqrt(length(x))}), by=cluster]
  tp = trend_cluster[, lapply(.SD, function(x){quantile(x,probs=0.975)}), by=cluster]
  lp = trend_cluster[, lapply(.SD, function(x){quantile(x,probs=0.025)}), by=cluster]
  
  cnames = colnames(mean_trend_cluster)
  cnames = cnames[cnames!="cluster"]
  mean_melted = melt(mean_trend_cluster, measure.vars = cnames)
  mean_melted$variable = as.numeric(as.character(mean_melted$variable))
  std_melted = melt(std_trend_cluster, measure.vars = cnames)
  std_melted$variable = as.numeric(as.character(std_melted$variable))
  tp_melted = melt(tp, measure.vars = cnames)
  tp_melted$variable = as.numeric(as.character(tp_melted$variable))
  lp_melted = melt(lp, measure.vars = cnames)
  lp_melted$variable = as.numeric(as.character(lp_melted$variable))
  
  melted = mean_melted
  names(melted)[names(melted)=="value"] = "mean"
  melted$std = std_melted$value
  melted$tp = tp_melted$value
  melted$lp = lp_melted$value
  
  print(melted)
  
  ggplot(melted, aes(variable,mean, col=as.factor(cluster))) + geom_line(size=1.5) +
    geom_ribbon(aes(ymin=mean-std, ymax=mean+std), linetype=2, alpha=0.08) +
    xlab("Year") + ylab("Urbanization Rate") + labs(col = "Cluster") +
    theme(plot.title = element_text(face="bold", size=20))+
    theme(axis.title.x = element_text(size=18)) +
    theme(axis.title.y = element_text(size=18)) +
    theme(axis.text.x = element_text(size=12))+
    theme(axis.text.y = element_text(size=18)) +
    theme(legend.text=element_text(size=16)) +
    theme(legend.title=element_text(size=16)) +
    theme(legend.position="bottom", legend.box = "horizontal") +
    scale_x_continuous(breaks=seq(1950, 2050, by = 5))+
    scale_y_continuous(breaks=seq(30,80, by = 10)) +
    guides(linetype=F)+
    theme_minimal()+
    ggtitle("Urbanization Rate by Cluster")
}

plot_tendencies_by_cluster(dt_urban)

############################################

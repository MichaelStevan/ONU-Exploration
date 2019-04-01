# Attemp at explaining rural exodus per country
#
# We will clusterize urbanization rate, assign cluster measure cluster differences,
# and use these as labels to them try and predict the clusters based on other country attributes.
# We will explore feature importance to try and determine relevant factors related 
# to rural exodus.
#
# Miguel Esteban
# miestgo@gmail.com 

library(data.table)

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
# Perform clustering
########################

num_clusters = 3
set.seed(121)
clusters = kmeans(dt_numeric, num_clusters,nstar=10)
dt_urban$cluster = clusters$cluster

########################
# Prepare Dataset for classification
########################

# Let's look at the differences per cluster again:
plot_tendencies_by_cluster= function(dt){
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

# Cluster 1 shows the lowest urbanization rate, followed by cluster 3 and 
# finally cluster 2, showing the highest urbanization rate.

# We will map the cluster names to Low Urbanization, 
# Medium Urbanization, High Urbanization accordingly.

dt = copy(dt_urban)[,.(Location,cluster)]
map_cluster = c("LowUrbanization","HighUrbanization","MediumUrbanization")
dt[,cluster:=map_cluster[cluster]]

# Let's join our dataset to country indicators
dt_indicators = fread('data/WPP2017_Period_Indicators_Medium.csv')

# Clean indicators dataset
dt_indicators[,`:=`(Variant=NULL,LocID=NULL,VarID=NULL,Time=NULL)]

# Let's spred this dataset by MidPeriod, but first filter by MidPeriod < 2050, 
# since no more clustering data was used after this year
dt_indicators = dt_indicators[MidPeriod<=2050 ]
dt_indicators = dt_indicators[!Location %in% remove_regions]

colnames = names(dt_indicators)
colnames = colnames[!colnames%in%c("Location","MidPeriod")]

dt_indicators = dcast(dt_indicators, Location ~ MidPeriod, value.var = names(dt_indicators) )

dt_indicators[,paste0("Location.1_",1953:2050):=NULL]
dt_indicators[,paste0("MidPeriod.1_",1953:2050):=NULL]

setkey(dt_indicators,Location)
setnames(dt,"cluster","target")
setkey(dt,Location)

# Join with DT
dt = dt_indicators[dt]

# Filter NA
dt = dt[complete.cases(dt)]
str(dt)

# We already have a dataset to play with.
fwrite(dt,"urbanization_clustering_joined.csv")

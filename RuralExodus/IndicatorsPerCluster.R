
library(data.table)


########################
# Read and clean data
########################

dt_urban= fread('data/WUP2018-F02-Proportion_Urban.csv',skip=1)
setnames(dt_urban,c("Location",seq(1950,2050,5)))

# Cleaning
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

remove_regions = c(remove_regions,toupper(remove_regions))

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
# Join Data
########################
dt = copy(dt_urban)[,.(Location,cluster)]

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

dt_indicators = dcast(dt_indicators, Location ~ MidPeriod, value.var = colnames )

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







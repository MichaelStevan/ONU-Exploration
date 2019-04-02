# We will visualize how urbanization changed over the years to 
# get a hint at the general trend and spot possible outliers.
#
# Miguel Esteban
# miestgo@gmail.com 


library(data.table)
library(ggplot2)

########################
# Read data
########################
dt_urban= fread('data/WUP2018-F02-Proportion_Urban.csv',skip=1)
setnames(dt_urban,c("Location",seq(1950,2050,5)))

# Transform to long
dt_urban = melt(dt_urban,id.vars="Location",variable.name="Time",value.name="Urbanization")


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

dt_urban= dt_urban[!Location %in% remove_regions]
# Replace commas, turn into numeric values
dt_urban[,Urbanization := as.numeric(gsub(",",".",Urbanization))]


########################
# Prepare for world map
########################


dt_urban[Location=="United Kingdom", Location:="UK"]
dt_urban[Location=="United States of America", Location:="USA"]
dt_urban[Location=="Russian Federation", Location:="Russia"]
dt_urban[Location=="Venezuela (Bolivarian Republic of)", Location:="Venezuela"]
dt_urban[Location=="Bolivia (Plurinational State of)", Location:="Bolivia"]
dt_urban[Location=="Congo", Location:="Republic of Congo"]
dt_urban[Location=="Côte d'Ivoire", Location:="Ivory Coast"]
dt_urban[Location=="United Republic of Tanzania", Location:="Tanzania"]
dt_urban[Location=="Syrian Arab Republic", Location:="Syria"]
dt_urban[Location=="Iran (Islamic Republic of)", Location:="Iran"]
dt_urban[Location=="Czechia", Location:="Czech Republic"]
dt_urban[Location=="TFYR Macedonia", Location:="Macedonia"]
dt_urban[Location=="Republic of Moldova", Location:="Moldova"]
dt_urban[Location=="Viet Nam", Location:="Vietnam"]
dt_urban[Location=="Dem. People's Republic of Korea", Location:="North Korea"]
dt_urban[Location=="Republic of Korea", Location:="South Korea"]
dt_urban[Location=="Lao People's Democratic Republic", Location:="Laos"]
dt_urbanKosovo = dt_urban[Location=="Serbia",,]
dt_urbanKosovo = dt_urbanKosovo[,Location:="Kosovo"]
dt_urban = merge(dt_urban,dt_urbanKosovo, all=T)

dt_urban[,`:=`(Location=factor(Location))]
min_urb = min(dt_urban$Urbanization,na.rm = T)
max_urb = max(dt_urban$Urbanization,na.rm = T)


########################
# Paint world map
########################

mapYear <- 1980
dtYear = dt_urban[Time==mapYear]
head(dtYear)


map.world = as.data.table(map_data("world"))
setnames(map.world,"region","Location")
setkey(map.world,Location)
setkey(dtYear,Location)
map.world_joined = dtYear[map.world,allow.cartesian=T]

head(map.world_joined)

minor_break_gen <- function(arg_1, arg_2) {
  print(arg_1)
  res=seq(from = arg_1, to = arg_2, length.out = 100000);
  res;
}

ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = Urbanization)) +
  scale_fill_distiller(palette = "Spectral", na.value = "transparent",limits=c(min_urb,max_urb))+
  labs(title = 'Urbanization Percentage by Country'
       ,subtitle = mapYear, fill="") +
  theme(text = element_text(family = "Gill Sans", color="#444444" )
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 20)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "right"
  )





########################
# Extract images for GIFT
########################

years = seq(1950,2050,5)

for(year in years){
  
  dtYear = dt_urban[Time==year]
  
  map.world = as.data.table(map_data("world"))
  setnames(map.world,"region","Location")
  setkey(map.world,Location)
  setkey(dtYear,Location)
  map.world_joined = dtYear[map.world,allow.cartesian=T]
  
  ggplot() +
    geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = Urbanization)) +
    scale_fill_distiller(palette = "Spectral", na.value = "transparent",limits=c(min_urb,max_urb))+
    labs(title = 'Urbanization Percentage by Country'
         ,subtitle = year, fill="") +
    theme(text = element_text(family = "Gill Sans", color="#444444" )
          ,panel.grid = element_blank()
          ,plot.title = element_text(size = 20)
          ,plot.subtitle = element_text(size = 10)
          ,axis.text = element_blank()
          ,axis.title = element_blank()
          ,axis.ticks = element_blank()
          ,legend.position = "right"
    )
  
  
  # Save file
  ggsave(paste0("./UrbanizationMaps/","Urb_",year,".png"))
}




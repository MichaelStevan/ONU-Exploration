library(data.table)
library(ggplot2)
library(rgdal)
library(rvest)

dt = fread("data/WPP2017_Period_Indicators_Medium.csv")

# Select cols
dt = dt[,.(Location,Time,LEx)]

dt[Location=="United Kingdom", Location:="UK"]
dt[Location=="United States of America", Location:="USA"]
dt[Location=="Russian Federation", Location:="Russia"]
dt[Location=="Venezuela (Bolivarian Republic of)", Location:="Venezuela"]
dt[Location=="Bolivia (Plurinational State of)", Location:="Bolivia"]
dt[Location=="Congo", Location:="Republic of Congo"]
dt[Location=="Côte d'Ivoire", Location:="Ivory Coast"]
dt[Location=="United Republic of Tanzania", Location:="Tanzania"]
dt[Location=="Syrian Arab Republic", Location:="Syria"]
dt[Location=="Iran (Islamic Republic of)", Location:="Iran"]
dt[Location=="Czechia", Location:="Czech Republic"]
dt[Location=="TFYR Macedonia", Location:="Macedonia"]
dt[Location=="Republic of Moldova", Location:="Moldova"]
dt[Location=="Viet Nam", Location:="Vietnam"]
dt[Location=="Dem. People's Republic of Korea", Location:="North Korea"]
dt[Location=="Republic of Korea", Location:="South Korea"]
dt[Location=="Lao People's Democratic Republic", Location:="Laos"]
dtKosovo = dt[Location=="Serbia",,]
dtKosovo = dtKosovo[,Location:="Kosovo"]
dt = merge(dt,dtKosovo, all=TRUE)

dt[,Time:=apply(dt[,.(Time)],1,function(x){as.numeric(unlist(strsplit(x,"-"))[2])})]

# Save treated dt for map
fwrite(dt,"poblacion_tratada_mapa.csv")

dt[,`:=`(Location=factor(Location))]
head(dt)

mapYear <- 2050
dtYear = dt[Time==mapYear, .(Location,LEx)]
head(dtYear)


map.world = as.data.table(map_data("world"))
setnames(map.world,"region","Location")
setkey(map.world,Location)
setkey(dtYear,Location)
map.world_joined = dtYear[map.world,allow.cartesian=T]

head(map.world_joined)

minor_break_gen <- function(arg_1, arg_2) {
  print(arg_1)
  res<-seq(from = arg_1, to = arg_2, length.out = 100000);
  res;
}

ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = LEx)) +
  scale_fill_distiller(palette = "Spectral", na.value = "transparent")+
  labs(title = 'Life Expectancy by Countries'
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




# Extract Images for GIFT







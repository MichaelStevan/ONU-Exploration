library(data.table)
library(ggplot2)
library(rgdal)
library(rvest)

library(shiny)


dt = fread("./WPP2017_TotalPopulationBySex.csv")
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
dt <- merge(dt,dtKosovo, all=TRUE)
dt[,`:=`(Location=factor(Location))]
head(dt)

mapYear <- 2050
dtYear = dt[Time==mapYear, .(Location,PopTotal)]
head(dtYear)


map.world <- map_data("world")

head(map.world['region'=="Kosovo"])
as.factor(dtYear$Location) %>% levels()
map.world_joined <- left_join(map.world, dtYear, by = c('region' = 'Location'))
head(map.world)

minor_break_gen <- function(arg_1, arg_2) {
  print(arg_1)
  res<-seq(from = arg_1, to = arg_2, length.out = 100000);
  res;
}

ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = PopTotal)) +
  scale_fill_distiller(palette = "Spectral", na.value = "transparent")+
  labs(title = 'Population by Countries'
       ,subtitle = mapYear, fill="") +
  theme(text = element_text(family = "Gill Sans", color="#444444" )#color = "#FFFFFF")
        # ,panel.background = element_rect(fill = "#444444")
        # ,plot.background = element_rect(fill = "#444444")
        ,panel.grid = element_blank()
        ,plot.title = element_text(size = 20)
        ,plot.subtitle = element_text(size = 10)
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,legend.position = "right"
  )

# ggplot() +
#   geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = PopTotal)) +
#   scale_fill_distiller(palette = "Spectral", na.value = "transparent", 
#                        limits= c(min(dt[,PopTotal]), max(dt[,PopTotal])),
#                        breaks=c(min(dt[,PopTotal]), max(dt[,PopTotal])),
#                        minor_breaks=minor_break_gen(min(dt[,PopTotal]), max(dt[,PopTotal])),
#                        labels=c(min(dt[,PopTotal]), max(dt[,PopTotal]))) +
#   labs(title = 'Population by Countries'
#        ,subtitle = mapYear, fill="") +
#   theme(text = element_text(family = "Gill Sans", color="#444444" )#color = "#FFFFFF")
#         # ,panel.background = element_rect(fill = "#444444")
#         # ,plot.background = element_rect(fill = "#444444")
#         ,panel.grid = element_blank()
#         ,plot.title = element_text(size = 30)
#         ,plot.subtitle = element_text(size = 10)
#         ,axis.text = element_blank()
#         ,axis.title = element_blank()
#         ,axis.ticks = element_blank()
#         ,legend.position = "bottom"
#   )

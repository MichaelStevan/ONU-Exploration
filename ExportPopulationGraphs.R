library(data.table)
library(ggplot2)

dt = fread("WPP2017_TotalPopulationBySex.csv")

head(dt)
dt[,`:=`(Location=factor(Location),Variant=factor(Variant),Time=as.Date(ISOdate(Time,12,31)))]

summary(dt)

dt = dt[,.(PopMale,PopFemale,PopTotal),.(Location,Time,Variant)]
dt[,gt2017:=Time>as.Date(ISOdate(2017,12,31))]

trim = function(x) sub("^\\s+","",x)

# Caution! This takes long to execute, it will generate graphs for 273 locations.
for(location in unique(dt$Location)){
  location_dt = copy(dt[Location==location,.(PopTotal,Time,Variant,gt2017)])
  ggplot(data=location_dt,aes(x=Time,y=PopTotal,col=Variant,linetype=gt2017))+
    geom_line(size=1.5)+
    theme_minimal()+
    guides(linetype=F)+
    ggtitle(paste(location,"'s Total Population by Variant"))
  
  # Save file in new directory if it exists
  loc_v = unlist(strsplit(location,"/"))
  file = trim(tail(loc_v,1))
  folder = paste0(loc_v[1:length(loc_v)-1],collapse = "-")
  
  dir_path = paste0("./PopTotal/",folder,ifelse(folder=='','','-'))
  ggsave(paste0(dir_path,file,".png"))
}



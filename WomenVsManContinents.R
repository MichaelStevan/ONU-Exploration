#
# A simple exploratory analysis with population data.
# @author Miguel Esteban Gómez
# @email miestgo@gmail.com
#

# Data Manipulation
library(data.table)
# Data Visualization
library(ggplot2)
library(gridExtra)

dt = fread("WPP2017_TotalPopulationBySex.csv")

# Casting
dt[,`:=`(Location=factor(Location),Variant=factor(Variant),Time=as.Date(ISOdate(Time,12,31)))]

head(dt)
str(dt)
summary(dt)

# Use meaningful columns
dt[,`:=`(LocID=NULL,VarID=NULL)]

# Use medium variant
dt  = dt[Variant=="Medium"]

# Get unique rows
dt = unique(dt)


#################################
# Female vs male population
#################################

# Paint for a particular country
ggplot(data=dt[Location%in%c("China")],aes(x=Time,y=PopFemale))+
  geom_line(col="red")+
  geom_line(aes(y=PopMale),col="blue")+
  theme_minimal()+
  ylab("Population")+
  ggtitle("China")


# Graphs per continent:
ggplot(data=dt[Location=="Europe"],aes(x=Time,y=PopFemale))+
  geom_line(col="red")+
  geom_line(aes(y=PopMale),col="blue")+
  theme_minimal()+
  ylab("Population")+
  ggtitle("Europe")

ggplot(data=dt[Location=="Asia"],aes(x=Time,y=PopFemale))+
  geom_line(col="red")+
  geom_line(aes(y=PopMale),col="blue")+
  theme_minimal()+
  ylab("Population")+
  ggtitle("Asia")

ggplot(data=dt[Location=="Africa"],aes(x=Time,y=PopFemale))+
  geom_line(col="red")+
  geom_line(aes(y=PopMale),col="blue")+
  theme_minimal()+
  ylab("Population")+
  ggtitle("Africa")


dt_america = dt[Location%in%c("Central America","South America","Northern America")]
dt_america = dt_america[,.(PopMale = mean(PopMale),PopFemale=mean(PopFemale)),.(Time)]

ggplot(data=dt_america,aes(x=Time,y=PopFemale))+
  geom_line(col="red")+
  geom_line(aes(y=PopMale),col="blue")+
  theme_minimal()+
  ylab("Population")+
  ggtitle("America")



print(p1)
print(p2)
print(p3)
print(p4)

gA <- ggplotGrob(p1)
gB <- ggplotGrob(p2)
gC <- ggplotGrob(p3)
gD <- ggplotGrob(p4)

maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5],gC$widths[2:5],gD$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
gC$widths[2:5] <- as.list(maxWidth)
gD$widths[2:5] <- as.list(maxWidth)

p5 <- arrangeGrob(
  gA, gB,gC,gD, nrow = 3, heights = c(0.80, 0.80,0.80))

plot(p5)

#################################


#################################
# Experimenting with Female to Male ratio
#################################

# Female to Male ratio, How many women per men per year?
dt[,femToMaleRatio:=PopFemale/PopMale,.(Location,Time)]

# How have the ratio of women per men varied in several countries?
dt_sel_countries = copy(dt[Location%in%c("Spain","France","Potugal","Italy","Germany","United Kingdom","United States of America","Qatar")])

ggplot(data=dt_sel_countries,aes(x=Time,y=femToMaleRatio,col=Location))+
  geom_line(size=3,alpha=0.65)+
  theme_minimal()+
  ggtitle("Female to male ratio over the years")


# Which countries have the most women per men?
dt_ftm_location= copy(dt)
dt_ftm_location = dt_ftm_location[,avgFemToMaleRatio:=mean(femToMaleRatio),.(Location)]

head(dt_ftm_location,20)

# Which countries have the least women per men?
head(dt_ftm_location[order(FemToMaleRatio)],20)

# Paint in a world map:
# Bubble Plot
# Animated plots as years go by based on gender and age population.


#################################
# Let's do it by continents
#################################

continents = c("Africa",
            "Europe",
            "Central America",
            "South America",
            "Northern America",
            "Asia",
            "Australia")

dt_continent = copy(dt[Location%in%continents])
dt_continent[grep("America",dt_continent$Location),Location:="America"]
dt_continent[Location=="America",`:=`(PopFemale=mean(PopFemale),PopMale=mean(PopMale)),.(Location,Time)]
dt_continent[,femToMaleRatio:=PopFemale/PopMale,.(Location,Time)]

ggplot(data=dt_continent,aes(x=Time,y=femToMaleRatio,col=Location))+
  geom_line(size=3,alpha=0.65)+
  theme_minimal()+
  ggtitle("Female to male ratio over the years per continent")

#################################

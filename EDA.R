#
# A simple exploratory analysis with population data.
# @author Miguel Esteban Gómez
# @email miestgo@gmail.com
#

# Data Manipulation
library(data.table)
# Data Visualization
library(ggplot2)


dt = fread("WPP2017_TotalPopulationBySex.csv")

# Casting
dt[,`:=`(Location=factor(Location),Variant=factor(Variant),Time=as.Date(ISOdate(Time,12,31)))]

head(dt)
str(dt)
summary(dt)

# Use meaningful columns
dt[,`:=`(LocID=NULL,VarID=NULL)]

# Average across location and time, to get rid of variants
dt_avg  = copy(dt[,.(avgPopMale=mean(PopMale),
                avgPopFemale = mean(PopFemale),
                avgPopTotal = mean(PopTotal)),
                .(Location,Time)])
# Filter Nas
dt_avg = dt_avg[complete.cases(dt_avg)]

# Get unique rows
dt_avg = unique(dt_avg)

# Add new interesting columns:

# Female to Male ratio, How many women per men eper year?
dt_avg[,avgFemToMaleRatio:=avgPopFemale/avgPopMale,.(Location,Time)]

# How have the ratio of women per men varied in several countries?
dt_avg_sel_countries = copy(dt_avg[Location%in%c("Spain","France","Potugal","Italy","Germany","United Kingdom","United States of America","Qatar")])

ggplot(data=dt_avg_sel_countries,aes(x=Time,y=avgFemToMaleRatio,col=Location))+
  geom_line(size=3,alpha=0.65)+
  theme_minimal()+
  ggtitle("Female to male ratio over the years")


# Let's do it by continents
continents = c("Africa",
            "Europe",
            "America",
            "Asia",
            "Australia")

dt_avg_continent = copy(dt_avg[Location%in%continents])

ggplot(data=dt_avg_continent,aes(x=Time,y=avgFemToMaleRatio,col=Location))+
  geom_line(size=3,alpha=0.65)+
  theme_minimal()+
  ggtitle("Female to male ratio over the years per continent")


# Which countries have the most women per men?
dt_avg_ftm_location= copy(dt_avg)
dt_avg_ftm_location = dt_avg_ftm[,FemToMaleRatio:=mean(avgFemToMaleRatio),.(Location)][order(-FemToMaleRatio)]

head(dt_avg_ftm_location,20)

# Which countries have the least women per men?
head(dt_avg_ftm_location[order(FemToMaleRatio)],20)

# Paint in a world map:
# Bubble Plot

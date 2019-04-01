library(data.table)
library(ggplot2)

dt_life = fread('data/WPP2017_LifeTable.csv')
dt_urban = fread('data/WUP2018-F01-Total_Urban_Rural.csv')
dt_urban_2 = fread('data/WUP2018-F02-Proportion_Urban.csv')
dt_period_ind = fread('data/WPP2017_Period_Indicators_Medium.csv')

#######################################

# Checking dt_life
names(dt_life)

# Drop not useful cols
dt_life[,`:=`(LocID = NULL, VarID=NULL, SexID = NULL)]
names(dt_life)

# What are the meaning of these columns?

# Showing the mortality experience of a hypothetical group 
# of infants born at the same time and subject throughout their
# lifetime to the specific mortality rates of a given period. 
# The following series are provided: age specific mortality rates (mx),
# probabilities of dying (qx), probabilities of surviving (px), 
# number surviving (lx), number dying (dx), number of person-years lived (Lx), 
# survivorship ratios (Sx), cumulative stationary population (Tx),
# average remaining life expectancy (ex) and average number of years lived (ax).

dt_life = dt_life[Variant=='Medium'][,Variant:=NULL]

dt_life[,`:=`(MidPeriod=as.Date(ISOdate(MidPeriod,1,1)), 
              AgeGrp = factor(AgeGrp),
              Sex=factor(Sex),
              Sx = as.numeric(Sx))]

dt_life_afghanistan = copy(dt_life[Location=='Afghanistan'])

ggplot(data=dt_life_afghanistan[by=.(MidPeriod)],aes(x=MidPeriod,y=Sx,col=Sex))+
  geom_point()+
  theme_minimal()

#######################################


# Checking dt urban
names(dt_urban)
dt_urban
dt_urban_2

# Checking period ind
names(dt_period_ind)

# Col meaning?
dt_period_ind

# Several indicators that are available for 5-year periods, from 1950-1955 to 2095-2100.
# TFR: Total fertility (live births per woman)
# NRR: Net reproduction rate (surviving daughters per woman)
# CBR: Crude birth rate (births per 1,000 population)
# Births: Number of births, both sexes combined (thousands)
# LEx: Life expectancy at birth for both sexes combined (years)
# LExMale: Male life expectancy at birth (years)
# LExFemale: Female life expectancy at birth (years)
# IMR: Infant mortality rate, q(1), for both sexes combined (infant deaths per 1,000 live births)
# Q5: Under-five mortality, 5q0, for both sexes combined (deaths under age five per 1,000 live births)
# CDR: Crude death rate (deaths per 1,000 population)
# Deaths: Number of deaths, both sexes combined (thousands)
# DeathsMale: Number of male deaths (thousands)
# DeathsFemale: Number of female deaths (thousands)
# CNMR: Net migration rate (per 1,000 population)
# NetMigrations: Net number of migrants, both sexes combined (thousands)
# GrowthRate: Average annual rate of population change (percentage)
# NatIncr: Rate of natural increase (per 1,000 population)
# SRB: Sex ratio at birth (male births per female births)
# MAC: Female mean age of childbearing (years)
# * not published for variants other than Medium.






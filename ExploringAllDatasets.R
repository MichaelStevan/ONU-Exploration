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


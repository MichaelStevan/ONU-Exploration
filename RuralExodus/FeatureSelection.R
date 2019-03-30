library(data.table)
library(caret)
library(mlbench)
library(corrplot)
library(PerformanceAnalytics)
library(ggplot2)

# Load data
dt = fread('RuralExodus/urbanization_clustering_joined.csv')

# That's a hell of attributes, let's perform feature selection:
dt_location = copy(dt)$Location
dt_features = dt[,!c("target","Location")]

set.seed(27)

# 1) Finding highly correlated features
corr_matrix = cor(dt_features)

# index >0.75 correlation
high_corr = findCorrelation(corr_matrix, cutoff=0.75)

# highly correlated features
names(dt_features)[high_corr]

# Eliminate these highly correlated features
dt_features_lowcorr = copy(dt_features)[,names(dt_features)[high_corr]:=NULL]

names(dt_features_lowcorr)

# Graph non correlated features
corrplot(cor(dt_features_lowcorr), method = "square")
chart.Correlation(dt_features_lowcorr)

# 2) Feature importance ranking

control = trainControl(method="repeatedcv", number=10, repeats=3)

dt_train = cbind(dt_features_lowcorr,target = dt$target)
dt_train[,target:=factor(target)]

set.seed(27)
model = train(target~., 
               data=dt_train, 
               method="lvq", #rf
               preProcess="scale", 
               trControl=control)

# Estimated variable importance
importance = varImp(model, scale=F)

print(importance)
plot(importance)

# How are important variables distributed on target data?
dt_netMigrations2013=dt[,
                        .(avgNetMigrations2013=mean(NetMigrations_2013)),
                        by=target]

ggplot(data=dt_netMigrations2013,aes(x=target, 
                               y=avgNetMigrations2013,
                               fill=target))+
  geom_col()+
  theme_minimal()+
  guides(fill=F)

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

# 3) Recursive feature elimination

# Let's try first without the highly correlated features

# Random forest will be used as model
control <- rfeControl(functions=rfFuncs, 
                      method="cv", 
                      number=10)

set.seed(27)
rfe_res <- rfe(dt_train[,-"target"],
               dt_train[,target], 
               sizes=c(4:20),
               rfeControl=control)
# summarize the results
print(rfe_res)
# list the chosen features
predictors(rfe_res)
# plot the results
plot(rfe_res, type=c("g", "o"))

# Let's try it now with all features, see what we get
dt_full = dt[,!"Location"]
dt_full[,target:=factor(target)]

set.seed(27)
rfe_res_2 <- rfe(dt_full[,!"target"],
                 dt_full[,target], 
                 sizes=c(4:20),
                 rfeControl=control)
# summarize the results
print(rfe_res_2)
# list the chosen features
predictors(rfe_res_2)
# plot the results
plot(rfe_res_2, type=c("g", "o"))


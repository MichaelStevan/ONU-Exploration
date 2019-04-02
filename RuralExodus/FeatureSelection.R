# We will try an extract the most important features, related to falling into
# a certain level of urbanization.
#
# Miguel Esteban
# miestgo@gmail.com 


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
dt= dt[,!"Location"]


# 1) Selection by filtering + random forest
filterCtrl = sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)

set.seed(27)
rfWithFilter = sbf(target~., data=dt, sbfControl = filterCtrl)

rfWithFilter

dt_filtered = dt[,.SD,.SDcols=predictors(rfWithFilter)]
dt_filtered = cbind(dt_filtered,target = dt$target)

# 2) Finding highly correlated features
set.seed(27)
corr_matrix = cor(dt_filtered[,!"target"])

# index <0.75 correlation
high_corr = findCorrelation(corr_matrix, cutoff=0.75)

# highly correlated features
names(dt_filtered)[high_corr]

# Eliminate these highly correlated features
dt_filtered_lowcorr = copy(dt_filtered)[,names(dt_filtered)[high_corr]:=NULL]

names(dt_filtered_lowcorr)

# Graph non correlated features
corrplot(cor(dt_filtered_lowcorr), method = "square")
chart.Correlation(dt_filtered_lowcorr)

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

# I think 16 variables is a nice subset for a model, let's try 
# to make something with this

dt_filtered_lowcorr = cbind(Location = dt_location,dt_filtered_lowcorr)

fwrite(dt_filtered_lowcorr,'dt_subset_classification.csv')

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


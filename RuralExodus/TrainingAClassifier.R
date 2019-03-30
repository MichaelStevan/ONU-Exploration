library(data.table)
library(caret)
library(mlbench)
library(ggplot2)

##############################
# Load data
##############################
dt = fread('RuralExodus/urbanization_clustering_joined.csv')

dt_location = copy(dt)$Location
set.seed(27)

dt = dt[,!"Location"]
dt[,target:=factor(target)]

##############################
# Feature selection
##############################
control <- rfeControl(functions=rfFuncs, 
                      method="cv", 
                      number=10)
set.seed(27)
rfe_res <- rfe(dt[,!"target"],
                 dt[,target], 
                 rfeControl=control)

# summarize the results
print(rfe_res)
# list the chosen features
predictors(rfe_res)
# plot the results
plot(rfe_res, type=c("g", "o"))

##############################
# Model training & Evaluating
##############################

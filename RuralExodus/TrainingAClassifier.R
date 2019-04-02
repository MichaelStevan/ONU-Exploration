library(data.table)
library(caret)
library(mlbench)
library(ggplot2)

##############################
# Load data
##############################
dt = fread('RuralExodus/dt_subset_classification.csv')

dt_location = copy(dt)$Location
set.seed(27)

dt = dt[,!"Location"]

map_target = c("cluster1","cluster2","cluster3")

dt[,target:=map_target[target]]

dt_features = dt
dt_labels = dt[,target]

##############################
# Model training & Evaluating
##############################

# Create Data partition
index_train = createDataPartition(dt_labels,p=0.8,list=F)

train = dt_features[index_train]
test = dt_features[-index_train]

train[,target:=factor(target)]
test[,target:=factor(target)]

table(train$target)
table(test$target)

# Define control and metrics
set.seed(27)

control = trainControl(method="cv", 
                        number=5, 
                        classProbs= T, 
                        summaryFunction = multiClassSummary)
metric = "Accuracy"



#1 k-Nearest Neighbors
set.seed(27)
m_kknn = train(target~., data=train, method="kknn", metric=metric, 
                trControl=control, preProcess = c("center", "scale") )
print(m_kknn)


#2 xgbTree
set.seed(27)
m_xgbTree = train(target~., data=train, method="xgbTree", metric=metric, 
              trControl=control, preProcess = c("center", "scale") )
print(m_xgbTree)

#3 Boosted Logistic Regression
set.seed(27)
m_lb = train(target~., data=train, method="LogitBoost", metric=metric, 
                  trControl=control, preProcess = c("center", "scale") )
print(m_lb)

#4 SVM Radial
set.seed(27)
m_svm = train(target~., data=train, method="svmRadial", metric=metric, 
               trControl=control, preProcess = c("center", "scale") )
print(m_svm)

##############################
# Compare models:
##############################
results = resamples(list(KNN=m_kknn, XGB=m_xgbTree, LB=m_lb, SVM=m_svm))

summary(results)
bwplot(results)
dotplot(results)

# Xgboost model

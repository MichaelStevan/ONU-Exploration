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

dt_features = dt[,!"target"]
dt_labels = dt[,target]

##############################
# Feature selection
##############################

# Easy feature selection by avoiding highly correlated

corr_matrix = cor(dt_features)
# index >0.75 correlation
high_corr = findCorrelation(corr_matrix, cutoff=0.75)
# highly correlated features
names(dt_features)[high_corr]
# Eliminate these highly correlated features
dt_features= copy(dt_features)[,names(dt_features)[high_corr]:=NULL]

##############################
# Model training & Evaluating
##############################

# Create Data partition

# We concatenate again with labels
dt_features = cbind(dt_features,target = dt_labels)

index_train = createDataPartition(dt_labels,p=0.8,list=F)

train = dt_features[index_train]
test = dt_features[-index_train]

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


library(randomForest)
library(mlbench)
library(caret)
library(e1071)
install.packages('e1071')

# Load Dataset
data(Sonar)
dataset <- train
x <- train[,c("Pclass" , "Sex" , "Age" , "SibSp" , "Parch" , "Fare" , "Embarked" , "Title" , "FamilySize" , "FamilyID2" , "CabinLevel" , "CabinRoom")]
y <- train[,c("Survived")]


groupvars  <- c("Pclass" , "Sex" , "Age" , "SibSp" , "Parch" , "Fare" , "Embarked" , "Title" , "FamilySize" , "FamilyID2" , "CabinLevel" , "CabinRoom")
f <- as.formula(paste("as.factor(Survived)", paste(groupvars, collapse=" + "), sep=" ~ "))

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(x=x, y=as.factor(y), method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_default)








set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(x=x, y=as.factor(y), method="rf", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)




set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(x=x, y=as.factor(y), sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))



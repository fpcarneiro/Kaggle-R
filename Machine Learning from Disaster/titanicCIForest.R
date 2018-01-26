setwd("~/Documents/Kaggle/Titanic")
source('titanic_preprocessing.R')

library(randomForest)
library(party)

set.seed(1)
ind <- sample.int( nrow(train)  )
train  <- train[  ind, ]
labels <- labels[ ind ]

val.set   = train[  c(1:32000) , ]
train.set = train[ -c(1:32000) , ]
dim(val.set)
dim(train.set)

val.labels   = labels[ c(1:32000) ]
train.labels = labels[ -c(1:32000) ]
table(val.labels)
table(train.labels)


# Build condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID + CabinLevel + CabinRoom,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforest.csv", row.names = FALSE)

Prediction_train <- predict(fit, train, OOB=TRUE, type = "response")
Result <- mean(train$Survived == Prediction_train)
Result
#0.81818 no Kaggle
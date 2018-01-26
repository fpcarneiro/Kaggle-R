# Best Result So Far
source("titanic_preprocessing.R")

# Build condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID + CabinRoom + CabinLevel + TicketPrefix + TicketSufix,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforest.csv", row.names = FALSE)

Prediction_train <- predict(fit, train, OOB=TRUE, type = "response")
Result <- mean(train$Survived == Prediction_train)
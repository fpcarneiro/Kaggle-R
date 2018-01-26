
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Install and load required packages for decision trees and forests
library(rpart)
#install.packages('randomForest')
library(randomForest)
#install.packages('party')
library(party)

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Fill in Age NAs
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
# Check what else might be missing
summary(combi)
# Fill in Embarked blanks
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# New factor for Random Forests, only allowed <32 levels, so reduce number
combi$FamilyID2 <- combi$FamilyID
# Convert back to string
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build Random Forest Ensemble
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train, importance=TRUE, ntree=2000)
# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# Build condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforest.csv", row.names = FALSE)



#Fernando Carneiro - Cabin

combi$Age <- scale(combi$Age)
combi$Fare <- scale(combi$Fare)

# Create a new factor column CabinLevel, CabinRoom
AddCabinFeatures <- function(df) {
  CabinLevels <- c("NA", "A","B","C","D","E", "F", "G", "T")
  CabinLevel <- gsub('^([A-G]).*$', '\\1', df$Cabin)
  df$CabinLevel <- factor(CabinLevel, levels=CabinLevels)
  
  CabinRoom <- gsub('^[^0-9]+([1-9]+).*$', '\\1', df$Cabin)
  CabinRoom <- ifelse(CabinRoom=="", "0", CabinRoom)
  suppressWarnings(CabinRoom <- as.integer(CabinRoom))
  df$CabinRoom <- ifelse(is.na(CabinRoom), 0, CabinRoom)
  
  df
}

combi <- AddCabinFeatures(combi)

deckNA <- combi[!is.na(combi$CabinLevel),]
combi$CabinLevel[is.na(combi$CabinLevel)] <- sapply(combi$Ticket[is.na(combi$CabinLevel)], FUN=function(x) { deckNA$CabinLevel[deckNA$Ticket == x][1] } )

combi$CabinLevel[combi$Pclass == 1 & is.na(combi$CabinLevel)] <- "C"
combi$CabinLevel[combi$Pclass == 2 & is.na(combi$CabinLevel)] <- "F"
combi$CabinLevel[combi$Pclass == 3 & is.na(combi$CabinLevel)] <- "F"

train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID + CabinLevel + CabinRoom,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforestCabin.csv", row.names = FALSE)


# Fernando Carneiro - Ticket Number

combi$TicketPrefix <- gsub('^(.*\\s).*$', '\\1', toupper(combi$Ticket) )
combi$TicketPrefix <- gsub('(\\s)|(\\.)|(\\/)', '',  combi$TicketPrefix)
combi$TicketPrefix <- gsub('\\d{2,}', 'NA',  combi$TicketPrefix)
combi$TicketSufix <- gsub("(.*\\s)|(\\D)","" ,gsub("STON/O 2.","STON/O2.", combi$Ticket))

combi$TicketPrefix <- factor(combi$TicketPrefix)
suppressWarnings(combi$TicketSufix <- as.numeric(combi$TicketSufix))


train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID + CabinLevel + CabinRoom + TicketPrefix + TicketSufix,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforestTicket.csv", row.names = FALSE)
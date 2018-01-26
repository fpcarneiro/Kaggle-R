# Multilabel Classification
#install.packages('mlr')
#install.packages('randomForestSRC')
#install.packages('rFerns')
library(mlr)
library(rFerns)
library(randomForestSRC)

# Creating a task
yeast = getTaskData(yeast.task)
labels = colnames(yeast)[1:14]
yeast.task = makeMultilabelTask(id = "multi", data = yeast, target = labels)
yeast.task

# Constructing a learner

##Algorithm adaptation methods
lrn.rfsrc = makeLearner("multilabel.randomForestSRC")
lrn.rFerns = makeLearner("multilabel.rFerns")
lrn.rFerns

##Problem transformation methods
lrn.br = makeLearner("classif.rpart", predict.type = "prob")
lrn.br = makeMultilabelBinaryRelevanceWrapper(lrn.br)
lrn.br

lrn.br2 = makeMultilabelBinaryRelevanceWrapper("classif.rpart")
lrn.br2

mod = train(lrn.br, yeast.task)
mod = train(lrn.br, yeast.task, subset = 1:1500, weights = rep(1/1500, 1500))
mod

mod2 = train(lrn.rfsrc, yeast.task, subset = 1:100)
mod2

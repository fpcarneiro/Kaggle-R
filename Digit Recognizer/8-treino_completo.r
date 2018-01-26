rm(list=ls())
#install.packages('data.tables')
#library( data.table)
library( randomForest )
##################################################################
#1- Carrega Datasets#################################################
##################################################################
train = read.csv( 'Data/train.csv' )
test  = read.csv( 'Data/test.csv'  )
dim(train)
dim(test)

##################################################################
#2- Separa a variavel alvo###########################################
##################################################################
labels <- train$label
table(labels)
train$label = NULL
dim(train)
dim(test)

##################################################################
#3- Transforma em Matrix#############################################
##################################################################
train <- as.matrix( train )
test  <- as.matrix( test )
dim(train)
dim(test)

##################################################################
#4- Constroi trainset e target ######################################
##################################################################
set.seed(1)
ind <- sample.int( nrow(train)  )
train  <- train[  ind, ]
labels <- labels[ ind ]

train.set = train
dim(train.set)

train.labels = labels
table(train.labels)
#######
###########################################################

##################################################################
#5- Funcoes de Ajuda#################################################
##################################################################
print_numero <- function( var  ){
  M = matrix( var, nrow=28, ncol=28, byrow=T )
  M = t(apply( M, 2, rev))
  image( M, col=gray.colors(256) )
}

log_loss <- function( actual, pred ){
  pred[pred <    10e-15 ] <-    10e-15
  pred[pred > (1-10e-15)] <- (1-10e-15)
  -mean( actual * log( pred ) )
}

MultiClass_log_loss <- function( actual, pred ){
  score = rep( 0 , length(actual) )  
  for( col in 1:ncol(pred) ){
    score = score + log_loss( actual==(col-1) , pred[,col] )
  }
  mean( score/ncol(pred) )
}
##################################################################


##################################################################
#6- Augment trainset#################################################
##################################################################
#transforma todas as colunas de inteiro pra numerico
for( i in 1:ncol(train.set)){
  train.set[,i] <- as.numeric( train.set[,i]  )
  test[,i]      <- as.numeric( test[,i]  )
}
gc()

aug1 <- train.set
aug2 <- train.set
aug3 <- train.set
aug4 <- train.set
aug1[, 1:(ncol(aug1)-1)]  <- aug1[, 2:ncol(aug1)]      # desloca 1 pixel esquerda
aug2[, 2:ncol(aug2)]      <- aug2[, 1:(ncol(aug2)-1)]  # desloca 1 pixel direita
aug3[,29:ncol(aug3)]      <- aug3[, 1:(ncol(aug3)-28)] # desloca 1 pixel baixo
aug4[, 1:(ncol(aug4)-28)] <- aug4[,29:ncol(aug4)]      # desloca 1 pixel cima
train.aug.set = rbind( train.set , aug1 , aug2, aug3, aug4 )
dim(train.set)
dim(train.aug.set)

print_numero( train.aug.set[ 1,]  )

train.aug.labels <- rep(train.labels,5)
table(train.aug.labels)
rm(train, aug1, aug2, aug3, aug4, train.set, train.labels, labels, ind)
gc()
##################################################################


nlevels <- as.numeric( lapply(lapply(as.data.frame(train.aug.set), unique), length)  )
feat.with.1.level <- which( nlevels==1 )
print( feat.with.1.level )
train.aug.set <- train.aug.set[, -feat.with.1.level]
test          <-          test[, -feat.with.1.level]
gc()



##################################################################
#7- treina cross validando###########################################
##################################################################
set.seed(1)
cv <- rep( 1:5 , length.out = nrow(train.aug.set)  )
cv <- cv[ sample.int(length(cv)) ]
table(cv)
gc()


train_pred <- rep( 0, nrow(train.aug.set)  )
for( fold in unique(cv) ){
  ind_train <- which( cv != fold )
  ind_val   <- which( cv == fold )

  set.seed(1111)
  model <- randomForest( x=train.aug.set[ind_train,], y=factor(train.aug.labels[ind_train]), ntree=111, do.trace=F  )
  pred_cv = predict( model, train.aug.set[ind_val,], type="prob" )
  pred_cv = (max.col(pred_cv, ties.method="first")-1)
  print( paste( "Fold", fold,"score:",  mean( pred_cv==train.aug.labels[ind_val] ) ) )

  train_pred[ ind_val ] <- pred_cv
  rm(model);gc()
}
# "Fold 1 score: 0.946"
# "Fold 3 score: 0.953"
# "Fold 5 score: 0.9395"
# "Fold 2 score: 0.9445"
# "Fold 4 score: 0.9415"
print( mean( train_pred==train.aug.labels ) )
# 0.9449

##################################################################
#8- treina usando todo trainset######################################
##################################################################
set.seed(1111)
model <- randomForest( x=train.aug.set, y=factor(train.aug.labels), ntree=111, do.trace=F  )
pred_cv = predict( model, as.matrix(test), type="prob" )
pred_cv = (max.col(pred_cv, ties.method="first")-1)

##################################################################
#9- gera arquivo do Kaggle###########################################
##################################################################

predictions <- data.frame(ImageId=1:nrow(test), Label=pred_cv)
head(predictions)
write.table( predictions , 'submission3.csv' , col.names=F, quote=F, sep=","   )
##################################################################


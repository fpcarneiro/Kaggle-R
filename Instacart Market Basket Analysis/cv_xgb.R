up <- fread("up.csv")
orders_products <- fread("orders_products.csv")
orders <- fread("ordersf1.csv")

xgb_eval_f1 <- function (yhat, dtrain) {
  require(ModelMetrics)
  y = getinfo(dtrain, "label")
  dt <- data.table(user_id=train[user_id %in% val_users, user_id], purch=y, pred=yhat)
  f1 <- mean(dt[,.(f1score=f1Score(purch, pred, cutoff=0.2)), by=user_id]$f1score)
  return (list(metric = "f1", value = f1))
}

xgb_eval_f1 <- function (yhat, dtrain) {
  require(ModelMetrics)
  y = getinfo(dtrain, "label")
  f1 <- f1Score(y, yhat, cutoff=0.18)
  return (list(metric = "f1", value = f1))
}

xgb_eval_f1 <- function (yhat, dtrain) {
  return (list(metric = "f1", value = 1))
}

set.seed(200)
val_users <- sample(unique(train$user_id), size = 10000, replace = FALSE)

varnames <- c(prd_predictors,user_predictors,up_predictors,order_predictors)

dtrain <- xgb.DMatrix(data=data.matrix(train[!user_id %in% val_users,varnames,with=FALSE]), label=train[!user_id %in% val_users, reordered])
dval <- xgb.DMatrix(data=data.matrix(train[user_id %in% val_users,varnames,with=FALSE]), label=train[user_id %in% val_users, reordered])
watchlist <- list(dval=dval)

dtrain <- xgb.DMatrix(data=data.matrix(train[,varnames,with=FALSE]), label=train[, reordered])
watchlist <- list(dtrain=dtrain)

params <- list(booster="gbtree"
               ,objective="reg:logistic"
               ,eval_metric=xgb_eval_f1
               ,eta=0.1
               ,gamma=0
               ,max_depth=5
               ,subsample=1
               ,colsample_bytree=1
               ,base_score=0.2
               ,nthread=8
)

params <- list(
  objective           = "reg:logistic",
  eval_metric=xgb_eval_f1,
  eta                 = 0.1,
  max_depth           = 6,
  min_child_weight    = 10,
  gamma               = 0.70,
  subsample           = 0.76,
  colsample_bytree    = 0.95,
  alpha               = 2e-05,
  lambda              = 10,
  base_score=0.18
)

print("Train XGBoost")
set.seed(3000)
xgb1 <- xgb.train(params = params,
                  data = dtrain,
                  nrounds = 100,
                  watchlist = watchlist,
                  maximize = TRUE,
                  print_every_n = 1)
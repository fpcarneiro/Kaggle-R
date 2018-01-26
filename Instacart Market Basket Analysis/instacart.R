#install.packages('Ckmeans.1d.dp')
#install.packages('DiagrammeR')
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library('Ckmeans.1d.dp')
library(xgboost)
require(ModelMetrics)
require(DiagrammeR)

# Function to F1 Score ###################################################################################
f1 <- function(list_a, list_b) {
  list_a <- str_split(list_a, ' ')[[1]]
  list_b <- str_split(list_b, ' ')[[1]]
  pr <- length(intersect(list_a,list_b))/length(list_b)
  re <- length(intersect(list_a,list_b))/length(list_a)
  f1 <- 0
  #print(pr)
  #print(re)
  if (pr + re)
  {
    f1 <- 2 * pr * re /(pr + re)
  }
  return(f1)
}

teste <- function(a,dt) {
  #print(a[1])
  temp <- dt[user_id == a[1] & order_number > a[2] & order_number <= a[3],sum(c(days_since_prior_order),na.rm = TRUE)]
  # print(temp)
  return( temp )
}

Mode <- function(x) {
  uni <- unique(x)
  uni[which.max(tabulate(match(x, uni)))]
}

up_intervals <- function(orders_products_dt) {
  data <- orders_products_dt[, .(up_orders = .N, up_first_order = min(order_number), 
                                                                 up_last_order = max(order_number)), by = .(user_id, product_id)]
  
  up <- data[up_orders > 1,.(user_id, product_id, up_orders, up_first_order, up_last_order)]
  
  user <- unique(up[,c(user_id)])
  for (u in user) {
    print(u)
    x <- apply(as.matrix(up[user_id == u,.(user_id, up_first_order, up_last_order)]), FUN = teste,1,dt=orders[user_id == u])
    up[user_id == u, interval := x]
  }
  up[, up_mean_interval := round(interval/(up_orders-1),2)]
  
  return(up)
}

departments_stat <- function(orders_products_dt, up_dt) {
  num_users <- orders_products_dt[, length(c(unique(user_id)))]
  num_orders <- orders_products_dt[, length(c(unique(order_id)))]
  
  ax_dep <- orders_products_dt[product_id != 0, .(dep_num_itens = .N, dep_num_reorder = sum(reordered)), by = .(department_id)]
  ax_dep_order <- orders_products_dt[product_id != 0, .(total_itens_dep = .N), by = .(order_id, department_id)]
  ax_dep_user <- orders_products_dt[product_id != 0, .(total_users_dep = .N), by = .(user_id, department_id)]
  ax_dep <- merge(ax_dep, ax_dep_order[,.(dep_num_orders = .N), by = .(department_id)], by = c("department_id"))
  ax_dep <- merge(ax_dep, ax_dep_user[,.(dep_num_users = .N), by = .(department_id)], by = c("department_id"))
  
  ax_dep[, dep_reorder_ratio := dep_num_reorder / dep_num_itens]
  ax_dep[, dep_popular_users := dep_num_users / num_users]
  ax_dep[, dep_popular_orders := dep_num_orders / num_orders]
  
  return(ax_dep)
}

aisles_stat <- function(orders_products_dt, up_dt) {
  num_users <- orders_products_dt[, length(c(unique(user_id)))]
  num_orders <- orders_products_dt[, length(c(unique(order_id)))]
  
  ax_ais <- orders_products_dt[product_id != 0, .(ais_num_itens = .N, ais_num_reorder = sum(reordered)), by = .(aisle_id)]
  ax_ais_order <- orders_products_dt[product_id != 0, .(total_itens_ais = .N), by = .(order_id, aisle_id)]
  ax_ais_user <- orders_products_dt[product_id != 0, .(total_users_ais = .N), by = .(user_id, aisle_id)]
  ax_ais <- merge(ax_ais, ax_ais_order[,.(ais_num_orders = .N), by = .(aisle_id)], by = c("aisle_id"))
  ax_ais <- merge(ax_ais, ax_ais_user[,.(ais_num_users = .N), by = .(aisle_id)], by = c("aisle_id"))
  
  ax_ais[, ais_reorder_ratio := ais_num_reorder / ais_num_itens]
  ax_ais[, ais_popular_users := ais_num_users / num_users]
  ax_ais[, ais_popular_orders := ais_num_orders / num_orders]
  
  return(ax_ais)
}


products_stat <- function(orders_products_dt, up_dt, departments_dt, aisles_dt) {
  prd <- orders_products_dt[, .(prod_time = .N,
                                prod_time_p1 = sum(period_of_day == 1),
                                prod_time_p2 = sum(period_of_day == 2),
                                prod_reorders = sum(reordered)), 
                            by = .(user_id, product_id, department_id , aisle_id)]
  
  num_users <- orders_products_dt[, length(c(unique(user_id)))]
  num_orders <- orders_products_dt[, length(c(unique(order_id)))]
  #mean_basket_size
  aux <- orders_products_dt[, .(prod_mean_basket_size = mean(basket_size, na.rm = T), 
                                prod_mean_add_to_cart = round(mean(add_to_cart_order, na.rm = T)),
                                prod_mean_dow = round(mean(order_dow, na.rm = T)),
                                prod_mean_hour_day = round(mean(order_hour_of_day, na.rm = T))
                                ), 
                            by = .(product_id)]
  
  prd <- prd[, .(prod_n_orders = sum(prod_time),
                 prod_n_orders_p1 = sum(prod_time_p1),
                 prod_n_orders_p2  = sum(prod_time_p2),
                 prod_n_reorders = sum(prod_reorders),
                 prod_n_users = .N,
                 prod_n_users_twice = sum(prod_time >= 2)),
             by = .(product_id, department_id , aisle_id)]
  
  prd <- merge(prd, aux,  by = ("product_id"), all.x = TRUE)
  
  prd[, prod_reorder_probability := prod_n_users_twice / prod_n_users]
  prd[, prod_reorder_times := 1 + prod_n_reorders / prod_n_users]
  prd[, prod_reorder_ratio := prod_n_reorders / prod_n_orders]
  prd[, prod_popular_users := prod_n_users / num_users]
  prd[, prod_popular_orders := prod_n_orders / num_orders]
  #preferred_time
  prd[, prod_period_1_ratio := prod_n_orders_p1 / prod_n_orders]
  prd[, prod_period_2_ratio := prod_n_orders_p2 / prod_n_orders]
  #prefix?
  #mean_interval
  aux <- up_dt[,.(prod_mean_interval = round(mean(up_mean_interval, na.rm = T),2), 
                  prod_median_interval = round(median(up_mean_interval, na.rm = T),2),
                  prod_sd_interval = round(sd(up_mean_interval, na.rm = T),2)),
               by = .(product_id)]
  prd <- merge(prd, aux,  by = ("product_id"), all.x = TRUE)
  
  
  prd <- merge(prd, departments_dt,  by = ("department_id"), all.x = TRUE)
  prd <- merge(prd, aisles_dt,  by = ("aisle_id"), all.x = TRUE)
  
  prd[, prod_popular_aisles_i :=  round(prod_n_orders / ais_num_itens, 2)]
  prd[, prod_popular_aisles_o :=  round(prod_n_orders / ais_num_orders, 2)]
  prd[, prod_popular_dep_i :=  round(prod_n_orders / dep_num_itens, 2)]
  prd[, prod_popular_dep_o :=  round(prod_n_orders / dep_num_orders, 2)]
  
  return(prd)
  #  p <- orders_products[product_id < 10,(prod_num = .N), by = .(product_id)]
}

users_stat <- function(orders_dt, orders_products_dt, up_dt) {
  users <- orders_dt[eval_set == "prior", .(user_orders = max(order_number), 
                                            user_period = sum(days_since_prior_order, na.rm = T), 
                                            user_mean_days_since_prior = round(mean(days_since_prior_order, na.rm = T)), 
                                            user_mean_f1 = mean(f1, na.rm = T), 
                                            user_mean_dow = round(mean(order_dow, na.rm = T)), 
                                            user_mean_hour_day = round(mean(order_hour_of_day, na.rm = T)), 
                                            user_sd_basket = sd(basket_size, na.rm = T), 
                                            user_mean_basket_reordered = mean(basket_size_reordered, na.rm = T),
                                            user_orders_none = sum(none_order, na.rm = T), 
                                            user_orders_not_none = sum(none_order == 0, na.rm = T), 
                                            user_mode_basket = Mode(basket_size), 
                                            user_median_basket = round(median(basket_size, na.rm = T))
  ),
  by = .(user_id)]
  
  us <- orders_products_dt[, .(user_total_products = .N, 
                               user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
                               user_distinct_products = n_distinct(product_id), 
                               user_distinct_departments = n_distinct(department_id), 
                               user_distinct_aisles = n_distinct(aisle_id)), by = .(user_id)]
  
  users <- merge(users, us, by = ("user_id"), all=FALSE)
  rm(us)
  
  users[, user_average_basket := user_total_products / user_orders]
  users[, user_basket_size_proportion := user_mean_basket_reordered / user_average_basket]
  users[, user_orders_none_ratio := user_orders_none / user_orders]
  users[, user_orders_not_none_ratio := user_orders_not_none / user_orders]
  
  aux <- up_dt[,.(user_mean_interval = round(mean(up_mean_interval, na.rm = T),2), 
                  user_median_interval = round(median(up_mean_interval, na.rm = T),2),
                  user_sd_interval = round(sd(up_mean_interval, na.rm = T),2)),
               by = .(user_id)]
  users <- merge(users, aux, by=("user_id"), all.x = TRUE)
  
  aux <- orders_dt[eval_set != "prior", .(user_id, order_id, eval_set, time_since_last_order = days_since_prior_order)]
  
  users <- merge(users, aux, by=("user_id"), all.x = TRUE)
  
  return(users)
}

database_stat <- function(orders_products_dt, users_dt, up_dt) {
  data <- orders_products_dt[, .(up_orders = .N, up_reorders = sum(reordered), up_first_order = min(order_number), 
                                 up_last_order = max(order_number), 
                                 up_average_cart_position = mean(add_to_cart_order, na.rm = T), up_average_dow = mean(order_dow, na.rm = T),
                                 up_average_hour_of_day = mean(order_hour_of_day, na.rm = T), up_days_since_prior_order = mean(days_since_prior_order, na.rm = T),
                                 up_last_purchased_orders_ago = min(orders_ago), up_first_purchased_orders_ago = max(orders_ago)), 
                             by = .(user_id, product_id)]
  
  data <- merge(data, up_dt[,.(user_id, product_id, up_mean_interval)], by = c("user_id","product_id"), all.x = TRUE)
  
  #  data <- merge(data, products_dt, by = ("product_id"), all=FALSE)
  data <- merge(data, users_dt[,.(user_id, user_orders)], by = ("user_id"), all=FALSE)
  
  data[, up_order_rate := up_orders / user_orders]
  data[, up_orders_since_last_order := user_orders - up_last_order]
  data[, up_order_rate_since_first_order := up_orders / (user_orders - up_first_order + 1)]
  
  data[, user_orders:= NULL]
  
  return(data)
}

get_train_set <- function(data_dt, orders_products_dt, orders_dt) {
  train <- merge(data_dt, orders_products_dt[, .(user_id, product_id, reordered)], by = c("user_id", "product_id"), all.x = TRUE)
  
  us <- orders_dt[order_id %in% orders_train_set, .(user_id, order_id, eval_set, time_since_last_order = days_since_prior_order, order_dow, period_of_day, order_hour_of_day)]
  train <- merge(train, us, by = ("user_id"), all=FALSE)
  train[is.na(reordered), reordered := 0]
  return(train)
}


# Load Data ---------------------------------------------------------------
path <- "Data"
aisles <- fread(file.path(path, "aisles.csv"), key = "aisle_id")
departments <- fread(file.path(path, "departments.csv"), key = "department_id")
# Read files and Join ORDER_PRODUCT*
opp <- fread(file.path(path, "order_products__prior.csv"))
opt <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"), key = c("product_id","aisle_id", "department_id"))

# Reshape data ------------------------------------------------------------
aisles[,aisle := as.factor(aisle)]
departments[,department := as.factor(department)]
orders[,eval_set := as.factor(eval_set)]
products[, product_name := as.factor(product_name)]

products <- merge(products, aisles, by = c("aisle_id"), all=FALSE)
products <- merge(products, departments, by = c("department_id"), all=FALSE)
rm(aisles, departments)

orders[order_hour_of_day <= 11,period_of_day := 1]
orders[order_hour_of_day > 11,period_of_day := 2]

orders <- merge(orders, rbindlist(list(opp, opt), use.names=TRUE)[,.(basket_size = .N, basket_size_reordered = sum(reordered)), by = .(order_id)], by = c("order_id"), all.x = TRUE)
#orders <- merge(orders, rbindlist(list(opp, opt), use.names=TRUE)[reordered == 1,.(basket_size_reordered = .N), by = .(order_id)], by = c("order_id"), all.x = TRUE)
orders[order_number == 1 & eval_set != "test", basket_size_reordered := 0]
orders[basket_size_reordered == 0, none_order := 1]
orders[basket_size_reordered > 0, none_order := 0]
#orders[order_id %in% opt[,c(order_id)], basket_size := opt[,.(basket_size = .N), by = .(order_id)][,c(basket_size)]]

setkey(opp, order_id, product_id)
setkey(opt, order_id, product_id)
prod_list_opp <- opp[ , .(prods = paste(product_id, collapse = ' ')), by = order_id]
prod_list_opt <- opt[ , .(prods = paste(product_id, collapse = ' ')), by = order_id]

orders <- merge(orders, prod_list_opp, by = c("order_id"), all.x = TRUE)
orders[order_id %in% opt[,c(order_id)], prods := prod_list_opt[,c(prods)]]

rm(prod_list_opp)
rm(prod_list_opt)
gc()

setkey(orders,user_id,order_number)
orders[, previous_order := shift(.(order_id),1)][, previous_prods := shift(.(prods),1)]
orders[order_number == 1, previous_order := NA]
orders[order_number == 1, previous_prods := NA]
orders[eval_set != "test" & order_number != 1, f1 := mapply(f1, prods, previous_prods)]

orders[,previous_order := NULL]
orders[,previous_prods := NULL]
orders[,prods := NULL]

#write.csv(orders, file = "ordersf1.csv", row.names = F)

varOrders <- setdiff(colnames(orders), c("prods"))
varProducts <- c("department_id", "aisle_id", "product_id")

none_train <- orders[none_order == 1 & order_number > 1 & eval_set == "train", c(order_id)]
none_prior <- orders[none_order == 1 & order_number > 1 & eval_set == "prior", c(order_id)]

ax <- opt[order_id %in% none_train, .SD[add_to_cart_order == max(add_to_cart_order)], by = .(order_id)]
ax[, product_id := 0]
ax[, reordered := 1]
opt <- rbindlist(list(opt,ax), use.names=TRUE)

ax <- opp[order_id %in% none_prior, .SD[add_to_cart_order == max(add_to_cart_order)], by = .(order_id)]
ax[, product_id := 0]
ax[, reordered := 1]
opp <- rbindlist(list(opp,ax), use.names=TRUE)

rm(none_train, none_prior, ax)
gc()

opp_expanded <- merge(opp, orders[, varOrders, with=FALSE], by = c("order_id"), all.x = TRUE, sort = FALSE)
rm(opp)
opp_expanded <- merge(opp_expanded, products[, varProducts, with=FALSE], by = ("product_id"), all.x = TRUE, sort = FALSE)
opp_expanded[,":="(orders_ago = max(order_number) - order_number + 1), by = user_id]
#setkey(opp_expanded, user_id, order_number, product_id)

opt_expanded <- merge(opt, orders[, varOrders, with=FALSE], by = c("order_id"), all.x = TRUE, sort = FALSE)
rm(opt)
opt_expanded <- merge(opt_expanded, products[, varProducts, with=FALSE], by = ("product_id"), all.x = TRUE, sort = FALSE)
#opt_expanded[,":="(orders_ago = max(order_number) - order_number + 1), by = user_id]
#setkey(opt_expanded, user_id, order_number, product_id)

rm(products)
gc()

#write.csv(opt_expanded, file = "opt_expanded.csv", row.names = F)
#write.csv(opp_expanded, file = "opp_expanded.csv", row.names = F)

aisles_statistics <- aisles_stat(orders_products_dt = opp_expanded, up_dt = up)
departments_statistics <- departments_stat(orders_products_dt = opp_expanded, up_dt = up)
products_statistics <- products_stat(orders_products_dt = opp_expanded, up_dt = up, departments_statistics, aisles_statistics )
users_statistics <- users_stat(orders_dt = orders, orders_products_dt = opp_expanded, up_dt = up)
database <- database_stat(orders_products_dt = opp_expanded,  
                          users_dt = users_statistics[,.(user_id, user_orders)], up_dt = up)

rm(opp_expanded)
gc()

users2train <- opt_expanded[, unique(c(user_id))]
# varOptExpanded <- setdiff(colnames(opt_expanded), c("order_hour_of_day","order_number",
#                                                     "f1","basket_size","aisle_id","department_id",
#                                                     "eval_set","add_to_cart_order"))
varOptExpanded <- c("user_id", "product_id", "reordered")

database <- merge(database, opt_expanded[, varOptExpanded, with=FALSE], by = c("user_id", "product_id"), all = TRUE, sort = FALSE)
rm(opt_expanded)
gc()

database <- merge(database, users_statistics, by = c("user_id"), all.x = TRUE, sort = FALSE)

train <- database[eval_set == "train"]
train <- merge(train, products_statistics, by = c("product_id"), all.x = TRUE, sort = FALSE)
train[, eval_set := NULL]
train[, order_id := NULL]
#train2$user_id <- NULL
#train2$product_id <- NULL
train[is.na(reordered), reordered := 0]

test <- database[eval_set == "test"]
test <- merge(test, products_statistics, by = c("product_id"), all.x = TRUE, sort = FALSE)
test[, eval_set := NULL]
test[, user_id := NULL]
test[, reordered := NULL]

varDiff <- c("product_id","user_id","order_id","reordered","eval_set","aisle_id", "department_id")
# varDiff2 <- c("department_id","aisle_id","prod_median_interval","prod_sd_interval",'user_sd_interval',
#               'user_median_basket','user_sd_basket', 'user_mode_basket','prd_median_interval','prd_sd_interval',
#               'user_distinct_departments','user_distinct_aisles')
label <- c('reordered')

varnames <- setdiff(colnames(train), varDiff)
#varnames <- setdiff(varnames, varDiff2)
up_predictors <- setdiff(colnames(database), c(varDiff,"up_last_purchased_orders_ago","up_first_purchased_orders_ago"))
user_predictors <- setdiff(colnames(users_statistics), c('user_id', 'user_median_interval','user_sd_interval','user_median_basket','user_sd_basket', 'user_mode_basket', 'user_distinct_departments','user_distinct_aisles'))
prd_predictors <- setdiff(colnames(products_statistics), c('product_id','prod_median_interval','prod_sd_interval', "department_id", "aisle_id"))
order_predictors <- setdiff(colnames(orders), c("order_id","eval_set", "basket_size", "prods", "f1", 'user_id'))

varnames <- c(up_predictors, user_predictors, prd_predictors)

rm(users_statistics, products_statistics)
rm(departments_statistics, aisles_statistics)
rm(database)
gc()

#write.csv(train, file = "train.csv", row.names = F)
#write.csv(test, file = "test.csv", row.names = F)

set.seed(200)
s <- unique(train[,c(user_id)])
val_users <- sample(s, size = round(length(s)/10), replace = FALSE)

#train <- train[up_orders > 1]

dtrain <- xgb.DMatrix(data=data.matrix(train[!user_id %in% val_users, varnames, with=FALSE]), label=train[!user_id %in% val_users, reordered])
dval <- xgb.DMatrix(data=data.matrix(train[user_id %in% val_users, varnames, with=FALSE]), label=train[user_id %in% val_users, reordered])
watchlist <- list(dval=dval)

xgb_eval_f1 <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  dt <- data.table(train[user_id %in% val_users, .(user_id, product_id)], purch=y, pred=yhat)
  dt[,previsao := (dt$pred > 0.18) * 1]
  
  f1_able <- dt[,.(F1Able = sum(purch) > 0), by = .(user_id)][F1Able == 1, unique(c(user_id))]
  not_f1_able <- dt[!user_id %in% f1_able, unique(c(user_id))]
  
  f1_able_predicted <- dt[user_id %in% not_f1_able,.(NonePredicted = sum(previsao) > 0), by = .(user_id)][NonePredicted == 1, unique(c(user_id))]
  not_f1_able_predicted <- dt[,.(NonePredicted = sum(previsao) == 0), by = .(user_id)][NonePredicted == 1, unique(c(user_id))]
  
  #print(f1_able)
  #print(not_f1_able)
  
  #res <- data.table(user_id = dt[, unique(c(user_id))])
  res <- dt[,.(min = min(pred), max = max(pred)), by = .(user_id)]
  actual <- dt[purch == 1, .(actual_list = paste(product_id, collapse = ' ')), by = .(user_id)]
  res <- merge(res, actual, by = c("user_id"), all.x = TRUE )
  res[user_id %in% not_f1_able, actual_list := "None"]
  
  predicted <- dt[previsao == 1, .(predicted_list = paste(product_id, collapse = ' ')), by = .(user_id)]
  res <- merge(res, predicted, by = c("user_id"), all.x = TRUE)
  res[user_id %in% not_f1_able_predicted, predicted_list := "None"]
  
  res[, f1score := f1(actual_list, predicted_list), by = user_id]
  
  #write.csv(res, file = "resultado.csv", row.names = F)
  f1_none <- mean(res[user_id %in% not_f1_able, c(f1score)])
  f1_not_none <- mean(res[user_id %in% f1_able, c(f1score)])
  f1_total <- mean(res[, c(f1score)])
  
  # print(length(not_f1_able))
  # print(f1_none)
  # print(length(f1_able))
  # print(f1_not_none)
  
  return (list(metric = "f1score", value = f1_total))
}

# xgb_eval_f1 <- function (yhat, dtrain) {
#   require(ModelMetrics)
#   y = getinfo(dtrain, "label")
#   f1 <- f1Score(y, yhat, cutoff=0.18)
#   return (list(metric = "f1score", value = f1))
# }

#booster = "gblinear"
params <- list(booster="gbtree"
               ,objective="reg:logistic"
               ,eval_metric=xgb_eval_f1
               ,eta=0.1
               ,gamma=0.7
               ,max_depth=7
               ,subsample=1
               ,min_child_weight= 10
               ,colsample_bytree=0.95
               ,base_score=0.18
               ,alpha= 2e-05
               ,lambda=10
)

print("Train XGBoost")
set.seed(3000)
xgb1 <- xgb.train(params = params,
                  data = dtrain,
                  nrounds = 300,
                  watchlist = watchlist,
                  maximize = TRUE,
                  print_every_n = 1)

print("Most important features (look at column Gain):")
#imp_matrix <- xgb.importance(feature_names = colnames(train$data), model = xgb1)
imp_matrix <- xgb.importance(feature_names = colnames(dtrain), model = xgb1)
print(imp_matrix)

# Feature importance bar plot by gain
print("Feature importance Plot : ")
print(xgb.plot.importance(importance_matrix = imp_matrix))
xgb.plot.tree(model = xgb1)

dtest <- xgb.DMatrix(data=data.matrix(test[,c(varnames),with=FALSE]))
test$reordered_predicted <- predict(xgb1, dtest)
test$reordered_predicted

test$reordered_all_class <- (test$reordered_predicted > 0.19) * 1

submission <- test[reordered_all_class == 1, .(products = paste(product_id, collapse = " ")), by = .(order_id)]

m <- test[!order_id %in% submission[,c(order_id)], c(order_id)]

if (length(m)>0) {
  missing <- data.table(order_id = unique(m), products = "None")
  #submission <- data.table(submission %>% bind_rows(missing) %>% arrange(order_id))
  submission <- rbindlist(list(submission, missing),use.names=TRUE)
}

u <- orders[,.(score = mean(f1)), by = .(user_id)]
unone <- u[score < 0.01,c(user_id)]
orders_unone <- orders[user_id %in% unone & order_id %in% test[,c(order_id)],c(order_id)]

submission[order_id %in% orders_unone, products := "None"]

write.csv(submission, file = "submit.csv", row.names = F)

xgb.save(xgb1, "xgb1_03844697.model")
# xgb.DMatrix can also be saved using xgb.DMatrix.save
xgb.DMatrix.save(dtrain, "dtrain.buffer")
xgb.DMatrix.save(dval, "dval.buffer")
xgb.DMatrix.save(dtest, "dtest.buffer")
# to load it in, simply call xgb.DMatrix
dtrain2 <- xgb.DMatrix("dtrain.buffer")
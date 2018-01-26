# Function to F1 Score ###################################################################################
f1score <- function(list_a, list_b)
{
  list_a <- str_split(list_a, ' ')[[1]]
  list_b <- str_split(list_b, ' ')[[1]]
  pr <- length(intersect(list_a,list_b))/length(list_b)
  re <- length(intersect(list_a,list_b))/length(list_a)
  f1 <- 0
  if (pr + re)
  {
    f1 <- 2 * pr * re /(pr + re)
  }
  return(f1)
}

products_stat <- function(orders_set, orders_products_dt) {
  
  if(missing(orders_set)) {
    prd <- orders_products_dt[eval_set == "prior",
                              .(product_time = .N, prod_reorders = sum(reordered)), by = .(user_id,product_id)]
  } else {
    prd <- orders_products_dt[order_id %in% orders_set,
                              .(product_time = .N, prod_reorders = sum(reordered)), by = .(user_id,product_id)]
  }
  
  prd <- prd[, .(prod_orders = sum(product_time), prod_reorders = sum(prod_reorders), 
                 prod_first_orders = sum(product_time >= 1), prod_second_orders = sum(product_time >= 2)), 
             by = .(product_id)]
  
  prd[, prod_reorder_probability := prod_second_orders / prod_first_orders]
  prd[, prod_reorder_times := 1 + prod_reorders / prod_first_orders]
  prd[, prod_reorder_ratio := prod_reorders / prod_orders]
  #preferred_time
  #mean_interval
  #mean_basket_size
  #prefix?
  prd[, c("prod_reorders", "prod_first_orders", "prod_second_orders") := NULL]
  return(prd)
  
#  p <- orders_products[product_id < 10,(prod_num = .N), by = .(product_id)]
  
}

users_stat <- function(orders_set, orders_dt, orders_products_dt) {
  
  if(missing(orders_set)) {
    users <- orders_dt[eval_set == 'prior', .(user_orders = max(order_number), user_period = sum(days_since_prior_order, na.rm = T), 
                                                                  user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T), user_mean_f1 = mean(f1), user_mean_dow = mean(order_dow), 
                                                                  user_mean_hour_day = mean(order_hour_of_day)), by = .(user_id)]
  
    us <- orders_products_dt[eval_set == 'prior', .(user_total_products = .N, user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
                                                                        user_distinct_products = n_distinct(product_id)), by = .(user_id)]
  } else {
    users <- orders_dt[order_id %in% orders_set, .(user_orders = max(order_number), user_period = sum(days_since_prior_order, na.rm = T), 
                                                                         user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T), user_mean_f1 = mean(f1), user_mean_dow = mean(order_dow), 
                                                                         user_mean_hour_day = mean(order_hour_of_day)), by = .(user_id)]
    
    us <- orders_products_dt[order_id %in% orders_set, .(user_total_products = .N, user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
                                                                               user_distinct_products = n_distinct(product_id)), by = .(user_id)]
  }
  
  users <- merge(users, us, by = ("user_id"), all=FALSE)
  users[, user_average_basket := user_total_products / user_orders]
  #mean f1

  return(users)
}

database_stat <- function(orders_set, orders_products_dt, products_dt, users_dt) {
  data <- orders_products_dt[eval_set == 'prior' & order_id %in% orders_set, .(up_orders = .N, up_first_order = min(order_number), 
                                                                          up_last_order = max(order_number), 
                                                                          up_average_cart_position = mean(add_to_cart_order)), by = .(user_id, product_id)]
  
  data <- merge(data, products_dt, by = ("product_id"), all=FALSE)
  data <- merge(data, users_dt, by = ("user_id"), all=FALSE)
  
  data[, up_order_rate := up_orders / user_orders]
  data[, up_orders_since_last_order := user_orders - up_last_order]
  data[, up_order_rate_since_first_order := up_orders / (user_orders - up_first_order + 1)]
  
  return(data)
}

get_train_set <- function(orders_train_set, data_dt, orders_products_dt, orders_dt) {
  train <- merge(data_dt, orders_products_dt[order_id %in% orders_train_set,
                                       .(user_id, product_id, reordered)], 
                 by = c("user_id", "product_id"), all.x = TRUE)
  
  us <- orders_dt[order_id %in% orders_train_set, .(user_id, order_id, eval_set, time_since_last_order = days_since_prior_order)]
  train <- merge(train, us, by = ("user_id"), all=FALSE)
  train[is.na(reordered), reordered := 0]
  return(train)
}

get_test_set <- function(orders_test_set, data_dt, orders_dt) {
  us <- orders_dt[order_id %in% orders_test_set, .(user_id, order_id, eval_set, time_since_last_order = days_since_prior_order)]
  test <- merge(data_dt, us, by = ("user_id"), all=FALSE)
  return(test)
}

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library('Ckmeans.1d.dp')

# Load Data ---------------------------------------------------------------
path <- "Data"
aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
# Read files and Join ORDER_PRODUCT*
order_products <- rbindlist(list(fread(file.path(path, "order_products__prior.csv")),
                                 fread(file.path(path, "order_products__train.csv"))),use.names=TRUE)
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))

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
orders[,period_of_day := as.factor(period_of_day)]

setkey(order_products,order_id,product_id)
prod_list <- order_products[ , .(prods = paste(product_id, collapse = ' ')), by = order_id]
orders <- merge(orders, prod_list, by = c("order_id"), all.x = TRUE)

rm(prod_list)

# Sort orders by user_id and order_number to make it easier to find previuos order
setkey(orders,user_id,order_number)
orders[, previous_order := shift(.(order_id),1)][, previous_prods := shift(.(prods),1)]
orders[order_number == 1, previous_order := NA]
orders[order_number == 1, previous_prods := NA]
orders[, f1 := mapply(f1score, prods, previous_prods)]

orders_products <- merge(orders[,c(1:8)], order_products, by = c("order_id"))
setkey(orders_products,user_id, order_number, product_id)

orders[,previous_prods := NULL]

rm(order_products)

nfolds <- 1
# Orders for the training
  t.orders <- merge(orders[eval_set == 'prior'] , orders[eval_set == 'prior', 
                                                                 .(last = max(order_number)-nfolds), by = .(user_id)], 
                      by.x = c("user_id","order_number"), by.y = c("user_id","last"), all = FALSE)[
                        ,c(order_id)]
# Orders for statistics
  s.orders <- orders[eval_set == 'prior' & !(order_id %in% t.orders), c(order_id)]
  
  t.orders <- orders[eval_set == 'train',c(order_id)]
  s.orders <- orders[eval_set == 'prior',c(order_id)]
  
##########################################################################################################
  t.orders <- merge(orders[eval_set == 'prior'] , orders[eval_set == 'train',.(last = order_number-1), by = .(user_id)], 
                    by.x = c("user_id","order_number"), by.y = c("user_id","last"), all = FALSE)[
                      ,c(order_id)]
  
  tst.orders <- merge(orders[eval_set == 'prior'] , orders[eval_set == 'test',.(last = order_number-1), by = .(user_id)], 
                      by.x = c("user_id","order_number"), by.y = c("user_id","last"), all = FALSE)[
                        ,c(order_id)]
  s.orders <- orders[eval_set == 'prior' & !(order_id %in% t.orders) & !(order_id %in% tst.orders), c(order_id)]
##########################################################################################################

# Products
  prd <- products_stat(s.orders, orders_products_dt = orders_products)
# Users -------------------------------------------------------------------
  users <- users_stat(s.orders, orders_dt = orders, orders_products_dt = orders_products)
# Database ----------------------------------------------------------------
  data <- database_stat(s.orders, orders_products_dt = orders_products, products_dt = prd, users_dt = users)
  
  rm(prd)
  rm(users)
  gc()
  
  train <- get_train_set(orders_train_set = t.orders, data_dt = data, 
                         orders_products_dt = orders_products, orders_dt = orders)
  
  up_predictors <- c('up_orders','up_first_order','up_last_order','up_average_cart_position', 'up_order_rate',
                  'up_orders_since_last_order','up_order_rate_since_first_order','time_since_last_order')
  
  user_predictors <- c('user_orders','user_period','user_mean_days_since_prior','user_mean_f1','user_mean_dow','user_mean_hour_day',
                  'user_total_products','user_reorder_ratio','user_distinct_products','user_average_basket','time_since_last_order')
  
  prd_predictors <- c('prod_orders','prod_reorder_probability','prod_reorder_times',
                      'prod_reorder_ratio','time_since_last_order')
  
  label <- c('reordered')
  
  train <- as.data.frame(train)
  train$eval_set <- NULL
  train$set <- NULL
  train$user_id <- NULL
  train$product_id <- NULL
  train$order_id <- NULL
  
  library(xgboost)
  
  params <- list(
    "objective"           = "reg:logistic",
    "eval_metric"         = "logloss",
    "eta"                 = 0.1,
    "max_depth"           = 6,
    "min_child_weight"    = 10,
    "gamma"               = 0.70,
    "subsample"           = 0.76,
    "colsample_bytree"    = 0.95,
    "alpha"               = 2e-05,
    "lambda"              = 10
  )
  
  subtrain <- train %>% sample_frac(0.1)
  X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered)), label = subtrain$reordered)
  print("Train")
  model <- xgboost(data = X, params = params, nrounds = 80)
  
  tst.orders <- orders[eval_set == 'test',c(order_id)]
  test <- get_test_set(orders_test_set = tst.orders, data_dt = data, orders_dt = orders)
  
  test$eval_set <- NULL
  test$user_id <- NULL
  X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
  print("Predict")
  test$reordered <- predict(model, X)
  test$reordered <- (test$reordered > 0.21) * 1
  
  submission <- test %>%
    filter(reordered == 1) %>%
    group_by(order_id) %>%
    summarise(
      products = paste(product_id, collapse = " ")
    )
  
  missing <- data.frame(
    order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
    products = "None"
  )
  
  submission <- data.table(submission %>% bind_rows(missing) %>% arrange(order_id))
  write.csv(submission, file = "submit.csv", row.names = F)
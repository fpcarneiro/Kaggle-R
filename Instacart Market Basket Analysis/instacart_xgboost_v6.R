teste <- function(a,b,c) {
  print(paste(a,b,c))
  temp <- orders[user_id == a & order_number > b & order_number <= c,sum(c(days_since_prior_order),na.rm = TRUE)]
  # print(temp)
  return( temp)
}

teste2 <- function(a,dt) {
  #print(a[1])
  temp <- dt[user_id == a[1] & order_number > a[2] & order_number <= a[3],sum(c(days_since_prior_order),na.rm = TRUE)]
  # print(temp)
  return( temp )
}

Mode <- function(x) {
  uni <- unique(x)
  uni[which.max(tabulate(match(x, uni)))]
}

# Function to F1 Score ###################################################################################
f1score <- function(list_a, list_b) {
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

# prod_n_orders: Number of times the product was ordered
# prod_n_reorders: Number of times the product was reordered
# prod_n_users: Number of users that bought the product
# prod_n_users_twice

products_stat <- function(orders_set, orders_products_dt, up_dt) {
  if(missing(orders_set)) { orders_to_consider <- orders_products_dt[eval_set == 'prior', unique(order_id)] 
  } else { orders_to_consider <- orders_set }
  
    prd <- orders_products_dt[order_id %in% orders_to_consider,
                              .(product_time = .N, prod_reorders = sum(reordered)), by = .(user_id,product_id)]
    num_users <- orders_products[order_id %in% orders_to_consider, length(c(unique(user_id)))]
    aux <- orders_products[order_id %in% orders_to_consider, .(mean_basket_size = mean(basket_size)), by = .(product_id)]
  
  prd <- prd[, .(prod_n_orders = sum(product_time), prod_n_reorders = sum(prod_reorders), 
                 prod_n_users = .N, prod_n_users_twice = sum(product_time >= 2)), 
             by = .(product_id)]
  
  prd <- merge(prd, aux,  by = ("product_id"), all.x = TRUE)
  
  prd[, prod_reorder_probability := prod_n_users_twice / prod_n_users]
  prd[, prod_reorder_times := 1 + prod_n_reorders / prod_n_users]
  prd[, prod_reorder_ratio := prod_n_reorders / prod_n_orders]
  prd[, prod_popularity := prod_n_users / num_users]
  #preferred_time
  #mean_interval
  #mean_basket_size
  #prefix?
  #  prd[, c("prod_reorders", "prod_first_orders", "prod_second_orders") := NULL]
  #  prd[, c("prod_first_orders") := NULL]
  aux <- up_dt[,.(prd_mean_interval = round(mean(up_mean_interval),2), 
                  prd_median_interval = round(median(up_mean_interval),2),
                  prd_sd_interval = round(sd(up_mean_interval),2)),
               by = .(product_id)]
  prd <- merge(prd, aux,  by = ("product_id"), all.x = TRUE)
  return(prd)
  #  p <- orders_products[product_id < 10,(prod_num = .N), by = .(product_id)]
}

# user_orders: number of orders of a user
# user_period: how long the user is a client of Instacart
# user_mean_days_since_prior: 
# user_mean_f1: how similar the orders are comparing to the previous one
# user_mean_dow: 
# user_mean_hour_day: 

users_stat <- function(orders_set, orders_dt, orders_products_dt, up_dt) {
  if(missing(orders_set)) { orders_to_consider <- orders_products_dt[eval_set == 'prior', unique(order_id)] 
  } else { orders_to_consider <- orders_set }
  
    users <- orders_dt[order_id %in% orders_to_consider, .(user_orders = max(order_number), user_period = sum(days_since_prior_order, na.rm = T), 
                                                   user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T), user_mean_f1 = mean(f1), user_mean_dow = mean(order_dow), 
                                                   user_mean_hour_day = mean(order_hour_of_day), user_median_basket = median(basket_size), user_sd_basket = sd(basket_size)), by = .(user_id)]
    
    user_mode <- orders_dt[order_id %in% orders_to_consider, .(user_mode_basket = Mode(basket_size)), by = .(user_id)]
    us <- orders_products_dt[order_id %in% orders_to_consider, .(user_total_products = .N, user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
                                                         user_distinct_products = n_distinct(product_id), user_distinct_departments = n_distinct(department_id), user_distinct_aisles = n_distinct(aisle_id)), by = .(user_id)]
  
  users <- merge(users, user_mode, by = ("user_id"), all=FALSE)
  users <- merge(users, us, by = ("user_id"), all=FALSE)
  users[, user_average_basket := user_total_products / user_orders]
  #mean f1
  aux <- up_dt[,.(user_mean_interval = round(mean(up_mean_interval),2), 
                  user_median_interval = round(median(up_mean_interval),2),
                  user_sd_interval = round(sd(up_mean_interval),2)),
               by = .(user_id)]
  users <- merge(users,aux,by=("user_id"),all.x = TRUE)
  return(users)
}

database_stat <- function(orders_set, orders_products_dt, products_dt, users_dt, up_dt) {
  if(missing(orders_set)) { orders_to_consider <- orders_products_dt[eval_set == 'prior', unique(order_id)] 
  } else { orders_to_consider <- orders_set }
  
  data <- orders_products_dt[order_id %in% orders_to_consider, .(up_orders = .N, up_first_order = min(order_number), 
                                                           up_last_order = max(order_number), 
                                                           up_average_cart_position = mean(add_to_cart_order), up_average_dow = mean(order_dow),
                                                           up_average_hour_of_day = mean(order_hour_of_day), up_days_since_prior_order = mean(days_since_prior_order, na.rm = T),
                                                           last_purchased_orders_ago = min(orders_ago), first_purchased_orders_ago = max(orders_ago)), by = .(user_id, product_id)]
  
  data <- merge(data, up_dt[,.(user_id, product_id,up_mean_interval)], by = c("user_id","product_id"), all.x = TRUE)
  
  data <- merge(data, products_dt, by = ("product_id"), all=FALSE)
  data <- merge(data, users_dt, by = ("user_id"), all=FALSE)
  
  data[, up_order_rate := up_orders / user_orders]
  data[, up_orders_since_last_order := user_orders - up_last_order]
  data[, up_order_rate_since_first_order := up_orders / (user_orders - up_first_order + 1)]
  
  return(data)
}

up_intervals <- function(orders_set, orders_products_dt) {
  if(missing(orders_set)) { orders_to_consider <- orders_products_dt[eval_set == 'prior', unique(order_id)] 
  } else { orders_to_consider <- orders_set }
  
  data <- orders_products_dt[order_id %in% orders_to_consider, .(up_orders = .N, up_first_order = min(order_number), 
                                                    up_last_order = max(order_number), 
                                                    up_average_cart_position = mean(add_to_cart_order), up_average_dow = mean(order_dow),
                                                    up_average_hour_of_day = mean(order_hour_of_day), up_days_since_prior_order = mean(days_since_prior_order, na.rm = T)), by = .(user_id, product_id)]
  
  up <- data[up_orders > 1,.(user_id, product_id, up_orders, up_first_order, up_last_order)]
  
  user <- unique(up[,c(user_id)])
  for (u in user) {
    print(u)
    x <- apply(as.matrix(up[user_id == u,.(user_id, up_first_order, up_last_order)]), FUN = teste2,1,dt=orders[user_id == u])
    up[user_id == u, interval := x]
  }
  up[, up_mean_interval := round(interval/(up_orders-1),2)]
  
  return(up)
}

get_train_set <- function(orders_train_set, data_dt, orders_products_dt, orders_dt) {
  train <- merge(data_dt, orders_products_dt[order_id %in% orders_train_set,
                                             .(user_id, product_id, reordered)], 
                 by = c("user_id", "product_id"), all.x = TRUE)
  
  us <- orders_dt[order_id %in% orders_train_set, .(user_id, order_id, eval_set, time_since_last_order = days_since_prior_order, order_dow, period_of_day, order_hour_of_day)]
  train <- merge(train, us, by = ("user_id"), all=FALSE)
  train[is.na(reordered), reordered := 0]
  return(train)
}

get_test_set <- function(orders_test_set, data_dt, orders_dt) {
  us <- orders_dt[order_id %in% orders_test_set, .(user_id, order_id, eval_set, time_since_last_order = days_since_prior_order, order_dow, period_of_day, order_hour_of_day)]
  #us <- merge(data_dt, us, by = ("user_id"), all=FALSE)
  us <- merge(us, data_dt, by = ("user_id"), all.x = TRUE)
  return(us)
}

#install.packages('Ckmeans.1d.dp')
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library('Ckmeans.1d.dp')

# Load Data ---------------------------------------------------------------
path <- "Data"
aisles <- fread(file.path(path, "aisles.csv"), key = "aisle_id")
departments <- fread(file.path(path, "departments.csv"), key = "department_id")
# Read files and Join ORDER_PRODUCT*
order_products <- rbindlist(list(fread(file.path(path, "order_products__prior.csv")),
                                 fread(file.path(path, "order_products__train.csv"))),use.names=TRUE)
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
#orders[,period_of_day := as.factor(period_of_day)]

orders <- merge(orders, order_products[,.(basket_size = .N), by = .(order_id)], by = c("order_id"), all.x = TRUE)

order_products[,":="(orders_ago = max(order_number) - order_number + 1), by = user_id]

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

orders_products <- merge(orders[,.(order_id, user_id, eval_set, order_number, order_dow, order_hour_of_day, 
                                   days_since_prior_order, period_of_day, basket_size)], order_products, by = c("order_id"))
orders_products <- merge(orders_products, products[,.(department_id, aisle_id, product_id)], by = ("product_id"), all.x = TRUE)

setkey(orders_products,user_id, order_number, product_id)

orders[,previous_prods := NULL]

rm(order_products)
rm(products)

# Orders for the training
#   t.orders <- merge(orders[eval_set == 'prior'] , orders[eval_set == 'prior', 
#                                                                  .(last = max(order_number)-nfolds), by = .(user_id)], 
#                       by.x = c("user_id","order_number"), by.y = c("user_id","last"), all = FALSE)[
#                         ,c(order_id)]
# # Orders for statistics
#   s.orders <- orders[eval_set == 'prior' & !(order_id %in% t.orders), c(order_id)]
#   
#   t.orders <- orders[eval_set == 'train',c(order_id)]
#   s.orders <- orders[eval_set == 'prior',c(order_id)]
#   tst.orders <- orders[eval_set == 'test',c(order_id)]

##########################################################################################################
t.orders <- merge(orders[eval_set == 'prior'] , orders[eval_set == 'train',.(last = order_number-1), by = .(user_id)], 
                  by.x = c("user_id","order_number"), by.y = c("user_id","last"), all = FALSE)[
                    ,c(order_id)]

tst.orders <- merge(orders[eval_set == 'prior'] , orders[eval_set == 'test',.(last = order_number-1), by = .(user_id)], 
                    by.x = c("user_id","order_number"), by.y = c("user_id","last"), all = FALSE)[
                      ,c(order_id)]
s.orders <- orders[eval_set == 'prior' & !(order_id %in% t.orders) & !(order_id %in% tst.orders), c(order_id)]
##########################################################################################################

up <- up_intervals(s.orders, orders_products_dt = orders_products)
# Products
prd <- products_stat(s.orders, orders_products_dt = orders_products, up_dt = up)
# Users -------------------------------------------------------------------
users <- users_stat(s.orders, orders_dt = orders, orders_products_dt = orders_products, up_dt = up)
# Database ----------------------------------------------------------------
data <- database_stat(s.orders, orders_products_dt = orders_products, products_dt = prd, users_dt = users, up_dt = up)

rm(prd)
rm(users)
gc()

up_predictors <- c('up_orders','up_first_order','up_last_order','up_average_cart_position', 'up_order_rate',
                   'up_orders_since_last_order','up_order_rate_since_first_order', 'up_average_dow', 'up_average_hour_of_day', 'up_days_since_prior_order','up_mean_interval','last_purchased_orders_ago', 'first_purchased_orders_ago')

user_predictors <- c('user_orders','user_period','user_mean_days_since_prior','user_mean_f1','user_mean_dow','user_mean_hour_day',
                     'user_total_products','user_reorder_ratio','user_distinct_products','user_average_basket', 'user_mean_interval',
                     'user_median_interval','user_sd_interval','user_median_basket', 'user_mode_basket', 'user_distinct_departments','user_distinct_aisles')

prd_predictors <- c('prod_n_orders','prod_n_reorders','prod_n_users','prod_n_users_twice','mean_basket_size',
                    'prod_reorder_probability','prod_reorder_times','prod_reorder_ratio','prod_popularity','prd_mean_interval','prd_median_interval')

order_predictors <- c('time_since_last_order', 'order_dow','period_of_day')

,'prd_sd_interval'
,'user_sd_basket'

label <- c('reordered')

library(xgboost)

train <- get_train_set(orders_train_set = t.orders, data_dt = data[up_orders > 1,], 
                       orders_products_dt = orders_products, orders_dt = orders)

train <- as.data.frame(train)
train$eval_set <- NULL
train$set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL

test <- get_test_set(orders_test_set = tst.orders, data_dt = data, orders_dt = orders)
test <- as.data.frame(test)
test$eval_set <- NULL
test$user_id <- NULL

rm(orders_products)
#rm(orders)
rm(data)
gc()

all_test <- test[,c(prd_predictors,user_predictors,up_predictors,order_predictors,"order_id", "product_id")]
X2_all <- xgb.DMatrix(as.matrix(all_test %>% select(-order_id, -product_id)))

dtest <- xgb.DMatrix(data=data.matrix(test[,varnames,with=FALSE]))

rm(all_test)
gc()

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
best_score <- 0
best_sub <- 0
best_round <- 0
best_treshold <- 0

for(sub in seq(0.3, 1, by = 0.1)) {
  print( paste( "Subtrain size ", sub))
  subtrain <- train %>% sample_frac(1.0)
  
  all_subtrain <- subtrain[,c(prd_predictors,user_predictors,up_predictors,order_predictors,label)]
  X_all <- xgb.DMatrix(as.matrix(all_subtrain %>% select(-reordered)), label = all_subtrain$reordered)
  
  rm(all_subtrain)
  rm(subtrain)
  #  rm(train)
  
  for(r in seq(90, 90, by = 1)) {
    model_all <- xgboost(data = X_all, params = params, nrounds = 100, verbose = 1)
    #model.rf <- randomForest(x = all_subtrain %>% select(-reordered), y = as.factor(all_subtrain$reordered), importance=TRUE, ntree=3, nodesize = 50)
    test$reordered_all <- predict(model_all, X2_all)
    
    for(treshold in seq(0.2, 0.21, by = 0.01)) {
      test$reordered_all_class <- (test$reordered_all > 0.2) * 1
      #test[test$reordered_all_class == 1 & is.na(test$up_mean_interval) & test$reordered_all < 0.40,]$reordered_all_class <- 0
      #predictor.rf <-predict(model.rf,all_test,type = "prob")
      #predictor.rf <-predict(model.rf,all_test,type='response')
      #test$reordered_rf_class <- predictor.rf
      #test$reordered_majority <-as.factor(ifelse(test$reordered_prd_user_class == 1 & test$reordered_up_class == 1, 1,
      #                                          ifelse(test$reordered_prd_user_class == 1 & test$reordered_all_class == 1, 1,
      #                                                 ifelse(test$reordered_up_class == 1 & test$reordered_all_class == 1, 1, 0))))
      
      submission <- test %>%
        filter(reordered_all_class == 1) %>%
        group_by(order_id) %>%
        summarise(
          products = paste(product_id, collapse = " "))
      
      m <- test$order_id[!test$order_id %in% submission$order_id]
      if (length(m)>0) {
        missing <- data.frame(
          order_id = unique(m),
          products = "None")
        submission <- data.table(submission %>% bind_rows(missing) %>% arrange(order_id))
      } else {
        submission <- data.table(submission)
      }
      
      submission <- merge(submission, orders[,.(order_id,prods)],by=("order_id"),all=FALSE)
      submission[, f1 := mapply(f1score, prods, products)]
      #  rate <- submission[,.(mean(f1))]
      #  print(submission[,.(mean(f1))])
      score <- submission[,.(mean(f1))]
      if(score > best_score) {
        best_score <- score
        best_round <- r
        best_sub <- sub
        best_treshold <- treshold
      }
      print( paste( "Round ", r, " Treshold ", treshold," scored :",  score ) )
    }
    
    rm(submission)
    rm(missing)
    rm(model_all)
    gc()
  }
  rm(X_all)
}
rm(X2_all)
rm(test)
rm(test)

u <- orders[,.(score = mean(f1)), by = .(user_id)]
unone <- u[score < 0.01,c(user_id)]
orders_unone <- orders[user_id %in% unone & order_id %in% tst.orders,c(order_id)]

submission[order_id %in% orders_unone, products := "None"]

write.csv(submission, file = "submit2.csv", row.names = F)
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('ggthemes')
#install.packages('plotly')
#install.packages('knitr')
#install.packages('stringr')
#install.packages('tidyr')
#install.packages('data.table')
#install.packages('dtplyr')
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(knitr)
library(stringr)
library(tidyr)
library(data.table)
library(dtplyr)

make_prediction <- function(a_row,all.same,all.dif,all.users,all.users.prod) {
  u_id <- as.numeric(a_row[['user_id']])
#  print(u_id)
  if (all.same[user_id == u_id, .N > 0]) {
    return (a_row[['previous_prods']]) 
  } else {
    n_itens <- round(all.users[user_id == u_id,avg_basket_size])
    prod_list <- all.users.prod[user_id == u_id, head(.SD, n_itens)][, .(prods = paste(product_id, collapse = ' '))][,prods]
    if (n_itens > 1) {
      if (all.dif[user_id == u_id, .N > 0]) {
        if (all.dif[user_id == u_id, (mean_f1 + mean_f1_l5 + mean_f1_l3 + mean_f1_last == 0)]) {
          return( "None" )
        } else {
          none <- paste("None",prod_list,sep = " ")
          return( none )
        }
      } else {
        return( prod_list ) }
    } else { return (prod_list ) }
  }
}

make_submission <- function() {
  return (data.frame(order_id = orders[eval_set == 'test',.(order_id)], products = orders[eval_set == 'test',apply(.SD,1,make_prediction,perfect,imperfect,users,users.products)]))
}

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

unpivot_users <- function(ds) {
  aux <- ds[,.(t = .N),by=user_id]
  ds[,n := 1]
  num <- unique(aux[,t])
  for(i in sort(num)) {
    range <- c(aux[t == i,user_id])
    print(i)
    if(length(range)>0) {
      ds[user_id %in% range, n := as.numeric(rep(1:i, length(range)))]
    }
  }
#  spread <- dcast(ds, user_id  ~ n, fun=mean, fill="", value.var = c("product_id"),sep="_")
#  setnames(spread, old=colnames(spread[,2:ncol(spread)]), new=paste("Item",colnames(spread[,2:ncol(spread)]),sep="_"))
#  return(spread)
}

aisles <- fread('Data/aisles.csv')
departments <- fread('Data/departments.csv')
orders <- fread('Data/orders.csv')
products <- fread('Data/products.csv')
# Read files and Join ORDER_PRODUCT*
order.products <- rbindlist(list(fread('Data/order_products__train.csv'),fread('Data/order_products__prior.csv')),use.names=TRUE)
prod_list <- order.products[ , .(prods = paste(product_id, collapse = ' ')), by = order_id]
basket.size <- order.products[ , .(basket_size = .N), by = order_id]

n <- max(order.products$add_to_cart_order)
# Feature Engineering
orders[order_hour_of_day <= 6,period_of_day := 'D']
orders[order_hour_of_day > 6 & order_hour_of_day <=12,period_of_day := 'M']
orders[order_hour_of_day > 12 & order_hour_of_day <=18,period_of_day := 'A']
orders[order_hour_of_day > 18,period_of_day := 'E']
orders <- merge(orders, basket.size, by = c("order_id"), all.x = TRUE)
orders <- merge(orders, prod_list, by = c("order_id"), all.x = TRUE)
# Sort orders by user_id and order_number to make it easier to find previuos order
setkey(orders,user_id,order_number)
# Feature Engineering
orders[, previous_order := shift(.(order_id),1)][, previous_prods := shift(.(prods),1)]
orders[, f1 := mapply(f1score, prods, previous_prods)]
# Orders and Products Details
order.products <- merge(order.products,products, all = FALSE)
# Orders x Products and Orders Details
order.products <- merge(order.products, orders[eval_set != 'test',.(order_id,user_id,eval_set,order_number,order_dow,order_hour_of_day,days_since_prior_order,period_of_day)], by = c("order_id"), all = FALSE)

# NEW DATASETs
## USERS x Products
#users.products <- data.table(data.frame(order.products) %>% filter(eval_set == 'prior') %>% group_by(user_id,product_id) %>% 
#  summarise(prod_times = n()) %>% arrange(user_id,desc(prod_times)) %>% top_n(n, product_id) %>% select(user_id,product_id,prod_times))
users.products <- setorder(order.products[eval_set == 'prior', .(prod_times = .N), by = c("user_id","product_id")],user_id,-prod_times)
## USERS SUMMARY
users <- orders[eval_set == 'prior',.(n_orders = .N, avg_basket_size = round(mean(basket_size),2), med_basket_size = median(as.double(basket_size),na.rm=T), sd_basket_size = sd(basket_size,na.rm=T), var_basket_size = var(basket_size,na.rm=T), min_basket_size = min(basket_size), max_basket_size = max(basket_size), how_long_client = sum(days_since_prior_order,na.rm=T)), by=.(user_id)]
users.products <- merge(users.products, users[,.(user_id,n_orders)], by = c("user_id"), all.x = TRUE) 
users.products[, prod_frequency := prod_times/n_orders]
#users.spread.count <- unpivot_users(users.products)
#users.spread.frequency <- unpivot_users(users.products %>% arrange(user_id,desc(prod_frequency)),n)

#order.products.spread <- order.products %>% select(-reordered,-product_name,-aisle_id,-department_id) %>% 
#  spread(add_to_cart_order,product_id,fill="",convert=TRUE)
#names(order.products.spread)[9:(9+n-1)] <- sapply(names(order.products.spread)[9:(9+n-1)], FUN=function(x) {x = paste('Item',x,sep="_")})

f1 <- orders[eval_set == 'prior' & order_number != 1, .(mean_f1 = mean(f1)), by = .(user_id)]
users <- merge(users, f1, all.x = TRUE)

last.orders <- orders[eval_set == 'prior' & order_number != 1,  tail(.SD, 5), by = .(user_id), .SDcols = c("order_id", "order_number","f1")]

f1.5 <- last.orders[ , .(mean_f1_l5 = mean(f1)), by = .(user_id)]
users <- merge(users, f1.5, all.x = TRUE)

f1.3 <- last.orders[, tail(.SD, 3), by = .(user_id), .SDcols = c("order_id", "order_number","f1")][ , .(mean_f1_l3 = mean(f1)), by = .(user_id)]
users <- merge(users, f1.3, all.x = TRUE)

f1.1 <- last.orders[, tail(.SD, 1), by = .(user_id), .SDcols = c("order_id", "order_number","f1")][ , .(mean_f1_last = mean(f1)), by = .(user_id)]
users <- merge(users, f1.1, all.x = TRUE)

perfect <- users[mean_f1_l5 >= 0.7,.(user_id,n_orders,avg_basket_size,mean_f1,mean_f1_l5,mean_f1_l3,mean_f1_last)]
imperfect <- users[mean_f1_l5 <= 0.05,.(user_id,n_orders,avg_basket_size,mean_f1,mean_f1_l5,mean_f1_l3,mean_f1_last)]

submit <- make_submission()
write.csv(submit, "submission7.csv", row.names=FALSE)




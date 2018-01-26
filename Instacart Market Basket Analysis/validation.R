library(ggplot2) # Data visualization
library(data.table) # CSV file I/O, e.g. the read_csv function
library(stringr)

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

## orders
orders <- fread('Data/orders.csv')

# order_products
order_products_train2 <- fread('Data/order_products__train.csv')
order_products_prior2 <- fread('Data/order_products__prior.csv')

## validation ####
orders_test <- orders[eval_set == 'test']
orders_train <- orders[eval_set == 'train']
orders_prior <- orders[eval_set == 'prior']

order_products_prior.reorderd <- order_products_prior[order_products_prior$reordered == 1]
order_products_train.reorderd <- order_products_train[order_products_train$reordered == 1]

# create order strings 
prior_list <- order_products_prior.reorderd[ , .(prods = paste(product_id, collapse = ' ')), by = order_id]
train_list <- order_products_train.reorderd[ , .(prods = paste(product_id, collapse = ' ')), by = order_id]

# identify previous order
setkey(orders, user_id, order_number)
orders[,order_id_lag1:=shift(order_id, 1)]
# subset to relevant columns only
orders <- orders[, c('order_id', 'eval_set', 'order_id_lag1')]

# attach original order
prval <- merge(orders, train_list, by.x = 'order_id', by.y = 'order_id',all.x = T)
setnames(prval, "prods", "original_order")
prval <- merge(prval, prior_list, by.x = 'order_id_lag1', by.y = 'order_id', all.x = T)
setnames(prval, "prods", "predicted_order")
prval <- prval[eval_set != 'prior']
# split
prfull <- prval[eval_set == 'test']
prval <- prval[eval_set == 'train']

# evaluate performance on the validation part
f1 <- mapply(f1score, prval$original_order, prval$predicted_order)
print(mean(f1))

# prediction
submission <- prfull[,c('order_id', 'predicted_order')]
setnames(submission, "predicted_order", "products")
submission$products[is.na(submission$products)] <- "None"
write.csv(submission, "sub_with_valid.csv", row.names=FALSE)
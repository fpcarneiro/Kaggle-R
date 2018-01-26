install.packages('data.table')
library(data.table)

# 02 Load data ----

#dt_aisles <- fread('../input/aisles.csv')
#dt_departments <- fread('../input/departments.csv')
#dt_products <- fread('../input/products.csv')
dt_orders <- fread('Data/orders.csv')
dt_prior <- fread('Data/order_products__prior.csv')
#dt_train <- fread('../input/order_products__train.csv')
dt_submission <- fread('Data/sample_submission.csv')


# 03 identify prior order ID----

# how many user_id only have 1 order?
table(dt_orders[,.(max=max(order_number)), by=user_id]$max)
hist(dt_orders[,.(max=max(order_number)), by=user_id]$max)

#good minimum number of orders is 4 so everyone has a prior order to copy

# setkey to ensure correct roworder
setkey(dt_orders, user_id, order_number)

# add new field containing previous order_id using the shift function
dt_orders[,order_id_lag1:=shift(order_id, 1)]

# add previous order_id to submissions file
setkey(dt_orders,order_id)
setkey(dt_submission,order_id)

#dt_submission <- merge(dt_submission, dt_orders, all.x = TRUE)
dt_submission <- dt_orders[dt_submission]


# 04 extract products from previous order----
setkey(dt_prior,order_id)
setkey(dt_submission,order_id_lag1)

dt_prior <- dt_prior[dt_submission]

# now only keep products that have been re-ordered
dt_prior <- dt_prior[dt_prior$reordered == 1, ]

# concatenate product list by previous i.order_id
# this will be our "predictions" file
dt_preds <- dt_prior[,.(pred_products = paste(product_id, collapse = " ")),  by = i.order_id]
setnames(dt_preds, "i.order_id", "order_id")

# 05 now create our submission file ----
dt_sub <- data.table(order_id = dt_submission$order_id,
                     products = "None")

setkey(dt_preds,order_id)
setkey(dt_sub,order_id)

dt_sub <- merge(dt_sub, dt_preds, all.x = TRUE)
dt_sub[!is.na(dt_sub$pred_products)]$products <- dt_sub[!is.na(dt_sub$pred_products)]$pred_products
dt_sub$pred_products <- NULL

#write out submission
dt_sub$products[dt_sub$order_id %in% none$order_id] <- "None"
write.csv(dt_sub, "sub_previousrepeat2.csv", row.names=FALSE)
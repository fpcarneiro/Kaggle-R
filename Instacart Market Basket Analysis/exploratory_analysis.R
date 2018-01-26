#install.packages('dplyr')
#install.packages('plotly')
#install.packages('ggthemes')
#install.packages('stringr')
#install.packages('knitr')
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(knitr)
library(stringr)
library(tidyr)
library(data.table)

theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "blue", fill = "black"),
        text=element_text(size = 12, family = "Serif", color = "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"))


setwd("D:/Data Science/Machine Learning/Competitions/Instacart Market Basket Analysis")
aisles <- read.csv("Data/aisles.csv")
departments <- read.csv("Data/departments.csv")
order_products_prior <- read.csv("Data/order_products__prior.csv")
order_products_train <- read.csv("Data/order_products__train.csv")
orders <- read.csv("Data/orders.csv")
products <- read.csv("Data/products.csv")

str(aisles)
str(order_products_prior)
str(products)

head(orders)

orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))

orders.first <- order_products_prior %>% filter(order_id == 2539329) %>% arrange(user_id, order_number)
orders.test <- orders %>% filter(eval_set %in% c('test')) %>% arrange(user_id, order_number)
orders.train <- orders %>% filter(eval_set %in% c('train')) %>% arrange(user_id, order_number)

table(orders.prior$order_dow)

p1 <- orders %>% 
  ggplot(aes(x=order_dow)) + 
  geom_histogram(stat="count",fill="red")
print(p1)

p1 <- orders %>% filter(order_number != 1)  %>% ggplot(aes(days_since_prior_order)) + geom_bar(stat="count",color='red',fill='pink',alpha=0.7) + xlab('Hour of the Day')+ ylab('Number of Orders') + ggtitle('What Time People Buy?') + theme_bw()

orders %>% ggplot() + geom_bar(mapping = aes(x = order_hour_of_day, alpha = 'count')) + xlab('Hour of the Day')+ ylab('Number of Orders') + ggtitle('What Time People Buy?') + theme_bw()

print(ggplotly(p1))

# Feature Engineering
orders$period[orders$order_hour_of_day <= 12 ] <- 'Early'
orders$period[orders$order_hour_of_day > 12 ] <- 'Late'

order_products <- rbind(order_products_train,order_products_prior)

bestsellers <- order_products %>% 
  group_by(product_id) %>% 
  summarize(countp = n()) %>% 
  left_join(select(products,product_id,product_name,department_id,aisle_id),by="product_id") %>%
  left_join(departments,by="department_id") %>%
  left_join(aisles,by="aisle_id") %>%
  arrange(desc(countp))

bestsellers.products <- bestsellers %>% top_n(10, wt = countp)
kable(bestsellers.products)

bestsellers.departments <- bestsellers %>% 
  group_by(department_id) %>% 
  summarize(countd = sum(countp)) %>%
  top_n(10, wt = countd)  %>%
  arrange(desc(countd)) %>%
  left_join(departments,by="department_id")
  
kable(bestsellers.departments)

bestsellers.aisles <- bestsellers %>% 
  group_by(aisle_id) %>% 
  summarize(counta = sum(countp)) %>%
  top_n(10, wt = counta)  %>%
  arrange(desc(counta)) %>%
  left_join(aisles,by="aisle_id")

kable(bestsellers.aisles)


test <- products %>% inner_join(departments,by="department_id")

p <- plot_ly(x = ~rnorm(50), type = "histogram")

p2 <- orders %>% ggplot(aes(x=order_dow)) + geom_bar() + theme_gdocs()
print(p2)

pl <- bestsellers.products %>% ggplot(aes(x=reorder(product_name,countp), y=countp)) + 
  geom_bar(stat = "identity", color='red', fill='pink', alpha=0.7,position = position_stack(reverse = TRUE)) +
xlab('Product') + ylab('Count') + ggtitle('What are the products bestsellers?') + coord_flip()
print(pl)

bestsellers.products %>% ggplot() + geom_bar(aes(x=product_name, y=countp),stat = "identity",color='red',fill='pink',alpha=0.7) + 
  xlab('Hour of the Day') + ylab('Number of Orders') + ggtitle('What Time People Buy?') + theme_bw()



orders %>% ggplot(aes(order_hour_of_day)) + geom_bar(stat="count",color='red',fill='pink',alpha=0.7) + xlab('Hour of the Day')+ ylab('Number of Orders') + ggtitle('What Time People Buy?') + theme_bw()


tmp <- orders %>% 
  group_by(user_id) %>% 
  summarize(count = n()) %>%
  arrange(count) 

tmp <- tmp %>% filter(count == 1)
head(tmp)

length(tmp)


# Departamentos com mais compras

# Corredores com mais compras

# Produtos mais comprados

# Produtos mais recomprados
bestsellers.reordered <- order_products %>% filter(reordered == 1) %>% group_by(product_id) %>% 
  summarize(countp = n()) %>% 
  left_join(select(products,product_id,product_name,department_id,aisle_id),by="product_id") %>%
  left_join(departments,by="department_id") %>%
  left_join(aisles,by="aisle_id") %>%
  arrange(desc(countp)) %>%
  top_n(10, wt = countp)

# Produtos mais comprados na primeira compra

# Produtos mais comprados de manh?, de tarde e de noite

# Produtos mais comprados por dia da semana

# Produtos mais comprados em conjunto - Unsupervised?



n <- 145


orders.test <- orders %>% filter(eval_set == 'test')

tmp <- orders %>% filter(order_number > 1 & eval_set != 'test') %>% select(order_id,user_id)
same.order <- order_products %>% inner_join(tmp,by = 'order_id') %>% group_by(user_id) %>%
  summarise(m = mean(reordered),c = n()) %>% arrange(desc(m),desc(c)) %>% filter(m == 1)

same.order.2 <- order_products_prior %>% inner_join(tmp,by = 'order_id') %>% group_by(user_id) %>%
  summarise(m = mean(reordered),c = n()) %>% arrange(desc(m),desc(c)) %>% filter(m == 1)

tmp2 <- orders %>% filter(eval_set != 'test') %>% select(order_id,user_id)
all.different.order <- order_products %>% inner_join(tmp2,by = 'order_id') %>% group_by(user_id) %>%
  summarise(m = mean(reordered),c = n()) %>% arrange(desc(m),desc(c)) %>% filter(m == 0)

tmp3 <- all.different.order %>% filter(user_id %in% orders$user_id[orders$eval_set == 'test']) %>% top_n(50, wt = c)

none <- orders %>% filter(user_id %in% tmp3$user_id, eval_set == 'test') %>% select(order_id)

allorders.join <- order_products %>% filter(reordered == 1) %>% inner_join(tmp2,by = 'order_id')
basket_size <- allorders.join %>% filter(reordered == 1) %>% group_by(order_id) %>% summarise(basket_size = n()) %>% arrange(desc(basket_size))

common_products <- allorders.join %>% group_by(product_id) %>% summarise(times = n()) %>% arrange(desc(times)) %>%
  left_join(select(products,product_id,product_name),by="product_id")

common_products.user <- allorders.join %>% group_by(user_id,product_id) %>% 
  summarise(times = n()) %>% arrange(user_id,desc(times)) %>% top_n(n, product_id)

common_products <- data.frame(common_products)
common_products.user <- data.frame(common_products.user)

spread <- unpivot_users(common_products.user,n)



dfunite <- unite(data = df, col = x, 
                 eval(parse(text=paste(selectedcols))), sep = sep, remove = FALSE)

pred.100 <- orders %>% filter(eval_set == 'test') %>% inner_join(same.order,by='user_id')

size_user <- function(dataset,u_id) {
  return(nrow(dataset %>% filter(user_id == u_id)))
}


unpivot_users <- function(ds,num) {
  aux <- ds %>% group_by(user_id) %>% summarise(t = n())
  ds$order <- 1
  
  for(i in 2:num) {
    ds$order[ds$user_id %in% aux$user_id[aux$t == i]] <- rep(1:i, nrow(distinct(filter(aux,t == i),user_id)))
  }
  
  ds.spread <- ds %>% select(user_id,product_id,order) %>%  spread(order,product_id,fill="")
  names(ds.spread)[2:(num+1)] <- sapply(names(ds.spread)[2:(num+1)], FUN=function(x) {x = paste('Item',x,sep="_")})
  nomes <- c(names(ds.spread)[-c(1)])
  ds.spread$product_set <- ds.spread %>% unite_("product_set",nomes,sep=" ")
  return(ds.spread)
}

dt_submission <- fread('Data/sample_submission.csv')
dt_sub <- data.table(order_id = submit$order_id,
                     products = submit$products)

write.csv(dt_sub, "sub_previousrepeat4.csv", row.names=FALSE)



dt_sub$products <- aux$products



submit <- orders %>% filter(eval_set == 'test') %>% select(order_id,user_id)
submit <- submit %>% left_join(spread,by="user_id")
submit <- submit %>% select(order_id,product_set)
submit$products <- "None"
submit$products <- submit$product_set$product_set
submit$products <- trimws(submit$products)
submit$products[is.na(submit$products)] <- "None"
submit <- data.frame(submit)

dt_sub %>% inner_join(orders,by="order_id") %>% head() %>% select(order_id,user_id,products)

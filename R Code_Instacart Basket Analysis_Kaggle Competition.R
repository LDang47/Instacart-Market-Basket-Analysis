# Clear plots
if(!is.null(dev.list())) dev.off()

# Clean workspace
rm(list=ls())
gc()

memory.size(max = TRUE) # Setting memory to max
options(scipen = 99,digits = 10) 

# Check if you have universal installer package, install if not
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} 

# Check, and if needed install the necessary packages
pacman::p_load("tidyverse", "caret", "xgboost","DMwR", "ROCR", "lift", "glmnet", "MASS", "data.table","dplyr") 

# Read all .csv file in the directory specified
path <- "C:\\Users\\Alo-Ai day-Toi day\\Desktop\\MMA COURSE\\8. MMA 831 Marketing Analytics\\GRADED ASSIGNMENT\\Final Project_9Aug\\Data_Instacart\\"
files <- list.files(path=path, pattern="*.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

rm(file, files, path, perpos, sample_submission)
gc()

# Initial view of 7 imported files
str(aisles)
str(departments)
str(order_products__prior)
str(order_products__train)
str(orders)
str(products)
str(sample_submission)

# Change variables type to factor
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

## Address missing values - NAs
sapply(aisles, function(x) sum(is.na(x)))
sapply(departments, function(x) sum(is.na(x)))
sapply(order_products__prior, function(x) sum(is.na(x)))
sapply(order_products__train, function(x) sum(is.na(x)))
sapply(orders, function(x) sum(is.na(x)))
sapply(products, function(x) sum(is.na(x)))

# For "days_since_prior_order", NA is meaningful: that user_id has never bought anything from Instacart before
# hencing NA days since prior order

####### DATA MERGING ------------------------------------------------------------------------------------------------------
# Count the number of users in each set: prior, train and test
orders %>%
  group_by(eval_set) %>%
  summarise(users=n_distinct(user_id))
# 1 prior    206209
# 2 test      75000
# 3 train    131209


# Count the number of orders in each set: prior, train and test
orders %>%
  group_by(eval_set) %>%
  summarise(users=n_distinct(order_id))
# 1 prior    3214874
# 2 test       75000
# 3 train     131209


# products_info: Match products to aisles and departments types, including 3 types of id
# Combine: products, aisles and departments
products <- products %>% 
  inner_join(aisles, by="aisle_id") %>% inner_join(departments, by="department_id")

rm(aisles, departments)
gc()

str(products)

# Extract the prior data from "Orders" dataset
# Combine order_products__prior and orders(prior) by "order_id"
prior <- order_products__prior %>% 
  inner_join(orders %>% filter(eval_set == "prior"), by="order_id") %>%
  inner_join(products, by="product_id")

prior <- subset(prior, select = -c(aisle_id, department_id))

str(prior)

# Extract the train data from "Orders" dataset
# Combine order_products__train and orders(train) by "order_id"
train <- order_products__train %>% 
  inner_join(orders %>% filter(eval_set == "train"), by="order_id")

str(train)

####### DATA EXLPLORATION ------------------------------------------------------------------------------------------------

# Total orders frequency per user id
qplot(n, data = count(orders, user_id), bins = 97, 
      main = "Frequency of total orders",
      xlab = "total no. order",
      ylab = "count")

# Prior set: Total products per order id
qplot(count(prior, order_id)$n, bins = 51,
      xlim = c(0,50),
      main = "Frequency of total products in an order",
      xlab = "total no. prod",
      ylab = "count",
      na.rm = T)

# Train set: Total products per order id
qplot(count(train, order_id)$n, bins = 51,
      geom = "histogram", 
      xlim = c(0,50), 
      main = "Frequency of total products in an order",
      xlab = "total no. prod",
      ylab = "count",
      na.rm = T)

# Frequency of Number of days between orders
qplot(days_since_prior_order, data = orders, 
      main = "Frequency distribution by days since prior order",
      xlab = "Days since prior order", 
      ylab = "count",
      xlim = c(0,35),
      bins = 30, na.rm = T)

# Time in the day to order
qplot(as.factor(orders$order_hour_of_day), 
      main = "Frequency distribution by Time in day",
      xlab = "Time of day", 
      ylab = "count")

# Day of week to order
qplot(as.factor(orders$order_dow),
      main = "Frequency distribution by Day of week",
      xlab = "Day of week", 
      ylab = "count")


# Popular products: TOP 30
past_df <- rbind(order_products__prior, order_products__train)
product_list <- products %>% 
  inner_join(past_df, by="product_id") %>%
  select(order_id, product_name, aisle, department)

head(product_list)
rm(past_df)
gc()

product_list %>% 
  group_by(product_name) %>% 
  summarize(count = n()) %>% 
  top_n(30, wt = count) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x=reorder(product_name,count), y=count)) +
  geom_bar(stat="identity")+
  ggtitle('Top 30 Products') +
  coord_flip()+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.y = element_blank())


# Most visited departments: TOP 15
product_list %>%
  group_by(department) %>%
  summarize(count=n()) %>%
  top_n(15, wt = count) %>%
  arrange(desc(count)) %>%
  ggplot (aes(x=reorder(department,count), y=count)) +  
  geom_bar(stat="identity")+
  ggtitle('15 Most visited Departments') +
  coord_flip()+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.y = element_blank())


# Most visited aisles: TOP 15
product_list %>%
  group_by(aisle) %>%
  summarize(count=n()) %>%
  top_n(15, wt = count) %>%
  arrange(desc(count)) %>%
  ggplot (aes(x=reorder(aisle,count), y=count)) +  
  geom_bar(stat="identity")+
  ggtitle('15 Most visited Aisles') +
  coord_flip()+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.y = element_blank())


# Reorder ratio by Departments
prior %>%
  group_by(department) %>%
  summarize(reordered_ratio = sum(reordered)/n()) %>%
  ggplot(aes(reorder(department, -reordered_ratio), reordered_ratio)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=department, 
                   xend=department, 
                   y=min(reordered_ratio), 
                   yend=max(reordered_ratio)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  ggtitle("Department reorder ratio") +
  xlab("Department") +
  coord_flip()


# Reorder ratio by Aisles
prior %>%
  group_by(aisle) %>%
  summarize(reordered_ratio = sum(reordered)/n()) %>%
  top_n(20, reordered_ratio) %>%
  ggplot(aes(reorder(aisle, -reordered_ratio), reordered_ratio)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=aisle, 
                   xend=aisle, 
                   y=min(reordered_ratio), 
                   yend=max(reordered_ratio)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  ggtitle("Aisles reorder ratio") +
  xlab("Aisles") +
  coord_flip()


# Reorder ratio by Aisles
prior %>%
  group_by(product_name) %>%
  summarize(reordered_ratio = sum(reordered)/n()) %>%
  top_n(20, reordered_ratio) %>%
  ggplot(aes(reorder(product_name, -reordered_ratio), reordered_ratio)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=product_name, 
                   xend=product_name, 
                   y=min(reordered_ratio), 
                   yend=max(reordered_ratio)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  ggtitle("Aisles reorder ratio") +
  xlab("Aisles") +
  coord_flip()

prior$aisle <- NULL
prior$department <- NULL

rm(order_products__prior, order_products__train, products)
gc()

######## FEATURE ENGINEERING --------------------------------------------------------------------------------------------

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

total_prior_orders <- nrow(orders[orders$eval_set=="prior",])

# Products Metrics ----------------------------------------------------------------------------------------------------
product_metrics <- prior %>% 
  group_by(product_id) %>%
  summarise(pr_total_orders = n(), # total order time of all products
            pr_total_orders_ratio = n()/total_prior_orders, # prob of getting ordered in all transactions
            pr_mean_add_to_cart = mean(add_to_cart_order), # average order adding to cart of all products
            pr_reordered_times = sum(reordered), # number of reorder for all products
            pr_reordered_ratio = pr_reordered_times / pr_total_orders, # Out of all orders placed for each product, how many is reorder
            pr_distinct_user = n_distinct(user_id), # no. of users ordered each product
            avg_hr_order = mean(order_hour_of_day), # average order hour of day
            avg_dow_order = mean(order_dow), # average order day of week
            avg_add_to_cart_order = mean(add_to_cart_order), # average add to cart order
            days_since_last_order = last(days_since_prior_order), # days since last order,
            no_back_to_back_order = sum(diff(order_number)[diff(order_number)==1]), # no. of back to back order for each product
            mode_hr_order = getmode(order_hour_of_day), # most frequent order hour of day
            mode_dow_order = getmode(order_dow) # most frequent order day of week
)

# Create more features for product metrics
product_metrics$reorder_probab <- product_metrics$pr_reordered_times/product_metrics$pr_total_orders

str(product_metrics)
rm(total_prior_orders)
gc()

# User Metrics -------------------------------------------------------------------
orders$order_dow_hod <- orders$order_dow * 24 + orders$order_hour_of_day

user_metrics <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_total_orders = max(order_number),# the total number of orders each user id made
    user_period = sum(days_since_prior_order, na.rm = T), # the period of time a user id uses instacart
    user_order_frequency = mean(days_since_prior_order, na.rm = T), # mean of days between orders for each user id
    user_mean_dow = mean(order_dow), # mean day of week a user id usually make their orders
    user_mean_hour = mean(order_hour_of_day), # mean hour of day a user id usually make their orders
    user_mean_dow_hod = mean(order_dow_hod), # mean hour in week term a user id usually make their orders
  )

# Other user metrics: product related
temp <- prior %>% 
  group_by(user_id) %>%
  summarise(
    user_total_products = n(), # total products bought by each user id
    user_reorder_ratio = sum(reordered) / user_total_products, # the proportion of reordered in the total no. of products bought by each user id
    user_add_to_cart = mean(add_to_cart_order,na.rm = TRUE), # mean add to cart order
    user_distinct_products = n_distinct(product_id), # number of distinct products that each user id order
    user_total_pr_reorders = sum(reordered) # total number of reorder for each user id
  )


temp1 <- prior %>% 
  group_by(user_id, order_id) %>%
  summarise(
    user_basket_size = n(), # number of items in basket for each user
    user_reorder_size = sum(reordered)) %>%
  ungroup() %>%
  group_by(user_id) %>%
  summarise(
    user_mean_basket_size = mean(user_basket_size),
    user_mode_basket_size = getmode(user_basket_size),
    user_stdev_basket_size = sd(user_basket_size),
    user_mean_reorder_size = mean(user_reorder_size),
    user_stdev_reorder_size = sd(user_reorder_size)
    )


test_train_orders <-  orders %>% 
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set, days_since_prior_order)  


user_metrics <- user_metrics %>%
  inner_join(test_train_orders, by = "user_id") %>%
  inner_join(temp, by = "user_id") %>%
  inner_join(temp1, by = "user_id")


str(user_metrics)
rm(test_train_orders, temp, temp1)
gc()

# Create user and product combination metrics.
user_product_metrics <- prior %>%
  group_by(user_id, product_id) %>%
  summarise(up_total_orders = n(), # # total number of times a user ordered the product  
            up_mean_add_to_cart= mean(add_to_cart_order),
            up_first_order = min(order_number),# first time a user purchased the product
            up_last_order = max(order_number), # last time a user purchased the product
            up_reordered_times = sum(reordered), # number of reorder of the product by the user
            up_mean_days_since_last_order = mean(days_since_prior_order, na.rm = T) # mean days since last order of the product by the user
  ) 

rm(prior, orders)
gc()

# Combine user and product metrics using 3 file: user_metrics, product_metrics and user_product_metrics
user_product_metrics <- user_product_metrics %>% 
  inner_join(product_metrics, by= "product_id") %>%
  inner_join(user_metrics, by= "user_id")


# More user_products_metrics 
user_product_metrics$up_order_range <- user_product_metrics$user_total_orders - user_product_metrics$up_first_order + 1    # the order range
user_product_metrics$up_reorder_ratio <- user_product_metrics$up_total_orders / user_product_metrics$up_order_range # the reorder percentage of the user for the product 
user_product_metrics$up_order_ratio <- user_product_metrics$up_total_orders / user_product_metrics$user_total_orders  # percentage of total user orders
user_product_metrics$up_num_orders_since_last_purchase <- user_product_metrics$user_total_orders - user_product_metrics$up_last_order # number of orders made since last purchase of product was made by the user
user_product_metrics$up_in_last_cart <- ifelse(user_product_metrics$up_last_order == user_product_metrics$user_total_orders,1,0) # Whether the user id only make 1 purchase in their time using instacart

str(user_product_metrics)

rm(product_metrics, user_metrics)
gc()

### TRAINING DATA: Seperate data into Validation test using in training process ------------------------------------------------

# Left join to keep the NA reordered
user_product_metrics <- user_product_metrics %>% 
  left_join(train %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

str(user_product_metrics)
colnames(user_product_metrics)

insta_df <- user_product_metrics %>%
  filter(eval_set == "train") %>% 
  as.data.frame() %>%
  select(-c(eval_set, user_id, product_id, order_id))

# Products never been ordered before have reordered 0
insta_df$reordered[is.na(insta_df$reordered)] <- 0
insta_df$reordered <- as.factor(insta_df$reordered)


str(insta_df)

######## TESTING DATA: Seperate data into Test test
test <- user_product_metrics %>%
  filter(eval_set == "test") %>%
  as.data.frame() %>%
  select(-c(eval_set, user_id, reordered))

str(test)
gc()

# Sample 50% of the insta_df data to work with 
subtrain <- insta_df %>% sample_frac(0.5)

subtrain$up_mean_days_since_last_order <- NULL
subtrain$days_since_last_order <- NULL

test$up_mean_days_since_last_order <- NULL
test$days_since_last_order <- NULL

set.seed(1000) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = subtrain$reordered,
                               p = 0.8, list = FALSE)
train <- subtrain[ inTrain,]
valid <- subtrain[ -inTrain,]

rm(subtrain, inTrain, insta_df)
gc()

######## ####### NON-REGULARISED LOGISTIC REGRESSION MODEL ----------------------------------------------------------------------------------

model_logistic <- glm(reordered ~ ., data=train, family="binomial"(link="logit"))
summary(model_logistic)

## Stepwise regressions
logistic_stepwiseAIC <- stepAIC(model_logistic, direction = c("both"),trace = 1) #AIC stepwise
summary(logistic_stepwiseAIC) 

# Looking at the Variable Importance table
varImp(model_logistic, scale = TRUE)

# Make predictions on the test data
logistic_probabilities <- predict(model_logistic, newdata= valid, type="response")

# Translate probabilities to predictions
mean(valid$reordered == "1")
logistic_classification <- ifelse(logistic_probabilities > 0.09749654, "1", "0")
logistic_classification <- as.factor(logistic_classification)
valid$reordered <- as.factor(valid$reordered)

# Model Accuracy
observed_classes <- valid$reordered
mean(logistic_classification == observed_classes) # 0.7479132

###Confusion matrix 
confusionMatrix(logistic_classification, valid$reordered, positive = "1")

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, valid$reordered)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp.logit <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp.logit@y.values) #Calculate AUC
logistic_auc_testing # 0.8243984

#### Lift chart
plotLift(logistic_probabilities, valid$reordered, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### Test dataset
test$up_mean_days_since_last_order <- NULL
test$days_since_last_order <- NULL

str(test)

# Make predictions on the test data
logistic_probabilities_test <- predict(model_logistic, newdata= test, type="response")

# Translate probabilities to predictions
mean(valid$reordered == "1")
logistic_classification_test <- ifelse(logistic_probabilities_test > 0.09749654, "1", "0")
logistic_classification_test <- data.frame(logistic_classification_test)

# SUBMISSION FILE ON KAGGLE -------------------------------------------------------------------------------

submission <- test %>% cbind(logistic_classification_test) %>%
  select(c(order_id, product_id,logistic_classification_test))
colnames(submission)[3] = "reordered"

str(submission)

submission <- submission %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submissionfile.csv", row.names = F)


########## REGULARISED REGRESSION MODEL
# Initiallizing H2O with 2 threads and 10 gigs of memory
h2o.shutdown()
h2o.init(nthreads = 2,max_mem_size = "10g")

df_train_h2o <- as.h2o(as.data.frame(train))
df_valid_h2o <- as.h2o(as.data.frame(valid))
df_test_h2o <- as.h2o(as.data.frame(test))

# Making list of all the predictors
colnames(df_train_h2o)

predictors <- c(colnames(df_train_h2o)[1:42])
predictors

response <- "reordered"

###### RIDGE ALPHA = 0 ----------------------------------------------------------------------------------------------------
glm_ridge <- h2o.glm(x = predictors, y = response, training_frame = df_train_h2o,validation_frame = df_valid_h2o
                            ,family = 'binomial',link='logit',lambda_search = T,seed = 1,alpha=0,nfolds = 5)

glm_ridge
summary(glm_ridge)
print(h2o.auc(glm_ridge)) # 0.823443
print(h2o.auc(glm_ridge,valid=T)) # 0.8240014

# Checking Accuracy on the validation frame
h2o.confusionMatrix(glm_ridge)
# Confusion Matrix (vertical: actual; across: predicted)  for max f1 @ threshold = 0.203112004572619:
#  0      1    Error             Rate
#0      2790191 267469 0.087475  =267469/3057660
#1       170400 161804 0.512938   =170400/332204
#Totals 2960591 429273 0.129170  =437869/3389864

h2o.confusionMatrix(glm_ridge,valid=T)
#Confusion Matrix (vertical: actual; across: predicted)  for max f1 @ threshold = 0.207422355411865:
#  0      1    Error            Rate
#0      700660  64194 0.083930   =64194/764854
#1       42973  39639 0.520179    =42973/82612
#Totals 743633 103833 0.126456  =107167/847466

# Plotting the coefficients of variables
x <- h2o.varimp(glm_ridge)

h2o.performance(glm_ridge)

reordered <- h2o.predict(glm_ridge, df_test_h2o,thresholds= 0.20311)
reordered <- as.data.frame(reordered)

# SUBMISSION FILE ON KAGGLE ------------------------------------------------------------
submission <- test %>% cbind(reordered) %>% 
  subset(select = c("order_id", "product_id", "predict"))

colnames(submission)[3] = "reordered"

submission <- submission %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submissionfile_glm ridge.csv", row.names = F)


######## LASSO Alpha = 1 ----------------------------------------------------------------------------------------------------
glm_lasso <- h2o.glm(x = predictors, y = response, training_frame = df_train_h2o,validation_frame = df_valid_h2o
                            ,family = 'binomial',link='logit',lambda_search = T,seed = 1,alpha=1,nfolds = 5)

glm_lasso
summary(glm_lasso)
print(h2o.auc(glm_lasso)) # 0.8233008
print(h2o.auc(glm_lasso,valid=T)) # 0.8238995

# Checking Accuracy on the validation frame
h2o.confusionMatrix(glm_lasso)
# Confusion Matrix (vertical: actual; across: predicted)  for max f1 @ threshold = 0.204808544698963:
#  0      1    Error             Rate
# 0      2799850 257810 0.084316  =257810/3057660
# 1       173051 159153 0.520918   =173051/332204
# Totals 2972901 416963 0.127103  =430861/3389864

h2o.confusionMatrix(glm_lasso,valid=T)
# Confusion Matrix (vertical: actual; across: predicted)  for max f1 @ threshold = 0.200885271351624:
#  0      1    Error            Rate
# 0      697791  67063 0.087681   =67063/764854
# 1       42166  40446 0.510410    =42166/82612
# Totals 739957 107509 0.128889  =109229/847466

# Plotting the coefficients of variables
x <- h2o.varimp(glm_lasso)
h2o.performance(glm_lasso)

reordered <- h2o.predict(glm_lasso, df_test_h2o,thresholds= 0.2048)
reordered <- as.data.frame(reordered)

# SUBMISSION FILE ON KAGGLE ------------------------------------------------------------
submission <- test %>% cbind(reordered) %>% 
  subset(select = c("order_id", "product_id", "predict"))

colnames(submission)[3] = "reordered"

submission <- submission %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submissionfile_glm lasso.csv", row.names = F)


####### ELASTIC NET ALPHA = 0.5 --------------------------------------------------------------------
glm_elasticnet <- h2o.glm(x = predictors, y = response, training_frame = df_train_h2o,
                                 validation_frame = df_valid_h2o,family = 'binomial',link='logit',
                                 lambda_search = T,seed = 1,alpha=0.5,nfolds = 5)

glm_elasticnet
summary(glm_elasticnet)
print(h2o.auc(glm_elasticnet)) # 0.8232432
print(h2o.auc(glm_elasticnet,valid=T)) # 0.8238271

# Checking Accuracy on the validation frame
h2o.confusionMatrix(glm_elasticnet)
# Confusion Matrix (vertical: actual; across: predicted)  for max f1 @ threshold = 0.204092077105879:
#  0      1    Error             Rate
# 0      2798400 259260 0.084790  =259260/3057660
# 1       172669 159535 0.519768   =172669/332204
# Totals 2971069 418795 0.127418  =431929/3389864

h2o.confusionMatrix(glm_elasticnet,valid=T)
# Confusion Matrix (vertical: actual; across: predicted)  for max f1 @ threshold = 0.207464755869873:
#  0      1    Error            Rate
# 0      701863  62991 0.082357   =62991/764854
# 1       43311  39301 0.524270    =43311/82612
# Totals 745174 102292 0.125435  =106302/847466

# Plotting the coefficients of variables
x <- h2o.varimp(glm_elasticnet)
h2o.performance(glm_elasticnet)

reordered <- h2o.predict(glm_elasticnet, df_test_h2o,thresholds= 0.20409)
reordered <- as.data.frame(reordered)

# SUBMISSION FILE ON KAGGLE ------------------------------------------------------------
submission <- test %>% cbind(reordered) %>% 
  subset(select = c("order_id", "product_id", "predict"))

colnames(submission)[3] = "reordered"

submission <- submission %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submissionfile_glm elasticnet.csv", row.names = F)


########## GRADIENT BOOSTING MACHINE (GBMs) ---------------------------------------------------------------------------------------

# Data Cleaning
insta_df$up_mean_days_since_last_order[is.na(df$up_mean_days_since_last_order)] <- 999
insta_df$days_since_last_order[is.na(df$days_since_last_order)] <- 999

subtrain <- insta_df
set.seed(1000) #set a random number generation seed to ensure that the split is the same everytime

# Detaching the H2O for CLEAN LOAD
detach("package:h2o", unload = TRUE)
library(h2o)

# Initiallizing H2O with 2 threads and 10 gigs of memory
h2o.shutdown()
h2o.init(nthreads = 2,max_mem_size = "10g")

# Loading data frame to H2o frame
df_train_h20 <- as.h2o(as.data.frame(subtrain))
rm(insta_df,subtrain)
gc()

# df_test_h20 <- as.h2o(as.data.frame(test))

# Spliting the the dataset in 80 TRAIN - 20 VALIDATION
df.splits <- h2o.splitFrame(data =  df_train_h20, ratios = .8)
train <- df.splits[[1]]
valid <- df.splits[[2]]

# Making list of all the predictors
colnames(df_train_h20)
predictors <- colnames(df_train_h20)[1:44]
predictors

# Setting Y variable
response <- "reordered"


# interact_list <- c("SEX","MARRIAGE")
insta_gbm <- h2o.gbm(x = predictors, y = response, training_frame = train,
                     validation_frame = valid,seed = 1,nfolds=3,distribution = 'multinomial',
                     max_depth = 7
)
#ntrees = 10000, learn_rate = 0.001,
#stopping_rounds = 5,stopping_tolerance = 1e-4,stopping_metric = "AUC")
# ,interaction_pairs =list(
# c("SEX","MARRIAGE") )
# ,interactions = interact_list
# ignore_const_cols = FALSE

insta_gbm

## Checking AUC
print(h2o.auc(insta_gbm))
print(h2o.auc(insta_gbm, valid = TRUE))

pred <- as.data.frame(h2o.predict(insta_gbm, as.h2o(as.data.frame(test))))

test$reordered <- ifelse(pred$p0 > 0.80,0,1)
print(paste(Sys.time()," - - - - -7 "))
submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )
print(paste(Sys.time()," - - - - -8 "))
missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)
print(paste(Sys.time()," - - - - -9 "))
submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
print(paste(Sys.time()," - - - - -10 "))

# SUBMISSION FILE ON KAGGLE
write.csv(submission, file = "C:/Users/Vikram Dhingra/Desktop/submit21.csv", row.names = F)

saveRDS(insta_gbm,"C:/Users/Vikram Dhingra/Desktop/insta_GBM.rds")

########## Extreme Gradient Boosting (XGBoost) ---------------------------------------------------------------------------------------

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


subtrain <- train %>% sample_frac(0.5)
X <- xgb.DMatrix(as.matrix(subtrain %>% dplyr::select(-reordered)), label = subtrain$reordered)


#Model 1 
#nrounds=80,
#"objective"           = "reg:logistic",
#"eval_metric"         = "logloss",
#"eta"                 = 0.1,
#"max_depth"           = 6,
#"min_child_weight"    = 10,
#"gamma"               = 0.70,
#"subsample"           = 0.76,
#"colsample_bytree"    = 0.95,
#"alpha"               = 2e-05,
#"lambda"              = 10


#Model 2 
#nrounds=100
#"objective"           = "reg:linear",
#"eval_metric"         = "logloss",
#"eta"                 = 0.1,
#"max_depth"           = 7,
#"min_child_weight"    = 10,
#"gamma"               = 0.80,
#"subsample"           = 0.76,
#"colsample_bytree"    = 0.9,
#"alpha"               = 2e-05,
#"lambda"              = 10


#Model 3 
#nrounds=100
#"objective"           = "reg:logistic",
#"eval_metric"         = "logloss",
#"eta"                 = 0.1,
#"max_depth"           = 6,
#"min_child_weight"    = 10,
#"gamma"               = 0.70,
#"subsample"           = 0.76,
#"colsample_bytree"    = 0.95,
#"alpha"               = 2e-05,
#"lambda"              = 10


#Model 4
#"booster"             ="gbtree",
#"objective"           = "reg:logistic",
#"eval_metric"         = "logloss",
#"eta"                 = 0.1,
#"max_depth"           = 5,
#"min_child_weight"    = 10,
#"gamma"               = 0,
#"subsample"           = 0.76,
#"colsample_bytree"    = 0.9,
#"alpha"               = 2e-05,
#"lambda"              = 10



model <- xgboost(data = X, params = params, nrounds = 100)


#Feature Importance Plot
importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

rm(X, importance, subtrain)
gc()


#Apply model 

X <- xgb.DMatrix(as.matrix(test %>% dplyr::select(-order_id, -product_id)))
test$reordered <- predict(model, X)

test$reordered <- (test$reordered > 0.21) * 1


#####Crossvalidation

X1 <- xgb.DMatrix(as.matrix(valid))
X1 <- xgb.DMatrix(as.matrix(valid %>% dplyr::select(-reordered)))


valid$reordered_pred <- predict(model, X1)


#Confusion Matrix

pred<-ifelse(valid$reordered_pred >0.21,1,0)
table(pred,valid$reordered)
unique(pred)
unique(valid$reordered)
caret::confusionMatrix(as.factor(valid$reordered), as.factor(pred))


#AUC
aucc <- ROCR::performance(prediction(predict(model,X1,type="response"),valid$reordered),measure = "auc")
as.numeric(aucc@y.values)

# SUBMISSION FILE ON KAGGLE 

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

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submit.csv", row.names = F)




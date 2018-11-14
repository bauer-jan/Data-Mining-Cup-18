library(h2o)
h2o.init(nthreads = -1,
         max_mem_size = "32g")

source("scripts/00_Modeling_Helper_Functions.R")
source("scripts/4_Generate_fold_specific_features.R")

data <- get_train_data(imputed_data=T)
train_org <- delete_outlier_days(data)
test_feb <- get_test_data()

#train <- train_org %>% filter(date <= as.Date("2017-12-31"))
#validation <- train_org %>% filter(date > as.Date("2017-12-31"))

train_val <- get_features_fold_specific(train_org, test_feb, final_training = T)
#train_val <- get_features_fold_specific(train, validation, final_training = F)

train <- train_val$train
test <- train_val$validation

train <- train %>% mutate(cluster=ifelse(number_of_sales_per_month > 1.2, 1,  0))
test <- test %>% mutate(cluster=ifelse(number_of_sales_per_month > 1.2, 1,  0))

train <- train %>% filter(cluster==1)
test <- test %>% filter(cluster==1)

#Convert datatypes
train$pid <- as.factor(train$pid)
test$pid <- as.factor(test$pid)

train$is_bundesliga_game <- as.factor(train$is_bundesliga_game)
train$is_holiday <- as.factor(train$is_holiday)
train$is_champions_league_game <- as.factor(train$is_champions_league_game)
train$is_dfb_game <- as.factor(train$is_dfb_game)
train$is_dfb_pokal_game <- as.factor(train$is_dfb_pokal_game)

test$is_bundesliga_game <- as.factor(test$is_bundesliga_game)
test$is_holiday <- as.factor(test$is_holiday)
test$is_champions_league_game <- as.factor(test$is_champions_league_game)
test$is_dfb_game <- as.factor(test$is_dfb_game)
test$is_dfb_pokal_game <- as.factor(test$is_dfb_pokal_game)

train$week_of_month <- as.factor(train$week_of_month)
train$price_category_of_item <- as.factor(train$price_category_of_item)

test$week_of_month <- as.factor(test$week_of_month)
test$price_category_of_item <- as.factor(test$price_category_of_item)

#loand into h2o
h2o_train <- as.h2o(train)
h2o_test <- as.h2o(test)

#RANDOM FOREST
attr_rf <-c("weekday",
            "days_since_release",
            "mean_temp",
            "mean_sunshine_duration",
            "day_in_month",
            "mean_rainfall_height",
            "google_trends_11teamsports",
            "color",
            "subCategory",
            "mean_no_of_sales",
            "number_of_sales_per_month",
            "ratio_sales_to_stock",
            "week_of_month",
            "standardized_size",
            "price_ratio_actualPrice_rrp",
            "mean_days_between_sales",
            "price_ratio_actualPrice_priceSubCategory",
            "brand",
            "is_bundesliga_game",
            "target_group",
            "is_holiday",
            "price_category_of_item",
            "is_champions_league_game",
            "category", "pid",
            "var_sales", "ma_actualPrice", "last_month_sum",
            "diff_last_month_sum")

mod_rf <- h2o.randomForest(x=attr_rf,
                           y="unitsSold",
                           ntrees = 750,
                           max_depth = 10,
                           min_rows  = 17,
                           histogram_type = "AUTO",
                           training_frame = h2o_train)

y_hat_rf_1 <- as.numeric(as.data.frame(h2o.predict(mod_rf, h2o_test))$predict)


#MAE/MSE--TRAIN
y_hat_rf_train <- as.numeric(as.data.frame(h2o.predict(mod_rf, h2o_train))$predict)
res_train_rf = train$unitsSold - y_hat_rf_train
mae_train_rf = mean(abs(res_train_rf))
mse_train_rf = mean((res_train_rf^2))
print(round(mae_train_rf,4))
print(round(mse_train_rf,4))

#XGBOOST
attr_xg <- c("weekday",
             "days_since_release",
             "mean_temp",
             "mean_sunshine_duration",
             "day_in_month",
             "mean_rainfall_height",
             "google_trends_11teamsports",
             "color",
             "subCategory",
             "mean_no_of_sales",
             "number_of_sales_per_month",
             "ratio_sales_to_stock",
             "week_of_month",
             "standardized_size",
             "price_ratio_actualPrice_rrp",
             "mean_days_between_sales",
             "price_ratio_actualPrice_priceSubCategory",
             "brand",
             "is_bundesliga_game",
             "target_group",
             "is_holiday",
             "price_category_of_item",
             "is_champions_league_game",
             "category", "pid",
             "var_sales", "ma_actualPrice", "last_month_sum",
             "diff_last_month_sum")


mod_xg <- h2o.xgboost(x=attr_xg,
                      y="unitsSold",
                      ntrees = 100,
                      max_depth = 6,
                      min_rows  = 6,
                      learn_rate = 0.1,
                      training_frame = h2o_train)


y_hat_xg_1<- as.numeric(as.data.frame(h2o.predict(mod_xg, h2o_test))$predict)


#MAE/MSE--TRAIN
y_hat_xg_train <- as.numeric(as.data.frame(h2o.predict(mod_xg, h2o_train))$predict)
res_train_xg = train$unitsSold - y_hat_xg_train
mae_train_xg = mean(abs(res_train_xg))
mse_train_xg = mean((res_train_xg^2))
print(round(mae_train_xg,4))
print(round(mse_train_xg,4))

#GLM
attr_glm  =c("weekday",
             "days_since_release",
             "mean_temp",
             "mean_sunshine_duration",
             "day_in_month",
             "mean_rainfall_height",
             "google_trends_11teamsports",
             "color",
             "subCategory",
             "mean_no_of_sales",
             "number_of_sales_per_month",
             "ratio_sales_to_stock",
             "week_of_month",
             "standardized_size",
             "price_ratio_actualPrice_rrp",
             "mean_days_between_sales",
             "price_ratio_actualPrice_priceSubCategory",
             "brand",
             "is_bundesliga_game",
             "target_group",
             "is_holiday",
             "price_category_of_item",
             "is_champions_league_game",
             "category", "pid",
             "var_sales", "ma_actualPrice", "last_month_sum",
             "diff_last_month_sum")

mod_glm <- h2o.glm(x=attr_glm,
                   y="unitsSold",
                   family = "poisson",
                   training_frame = h2o_train )

y_hat_glm_1 <- as.numeric(as.data.frame(h2o.predict(mod_glm, h2o_test))$predict)


#MAE/MSE--TRAIN
y_hat_glm_train <- as.numeric(as.data.frame(h2o.predict(mod_glm, h2o_train))$predict)
res_train_glm = train$unitsSold - y_hat_glm_train
mae_train_glm = mean(abs(res_train_glm))
mse_train_glm = mean((res_train_glm^2))
print(round(mae_train_glm,4))
print(round(mse_train_glm,4))

#BAGGING
cluster_1 <- (y_hat_xg_1+y_hat_rf_1+y_hat_glm_1)/3


#MAE/MSE--TRAIN
y_hat_bag_train <- (y_hat_rf_train+y_hat_xg_train+y_hat_glm_train)/3
res_train_bag = train$unitsSold - y_hat_bag_train
mae_train_bag = mean(abs(res_train_bag))
mse_train_bag = mean((res_train_bag^2))
print(round(mae_train_bag,4))
print(round(mse_train_bag,4))

#SAVE THE DATA
items <- as.data.frame(train_val$validation %>% group_by(pid,size,date) %>% summarise(y=NA))
items$y <- NULL

items$pid <- as.character(items$pid)
test$pid <- as.character(test$pid)

#final <- cbind(test[,c("pid","size","date","unitsSold")],cluster_1)
final <- cbind(test[,c("pid","size","date")],cluster_1)
final <- cbind(final,y_hat_rf_1)
final <- cbind(final,y_hat_xg_1)
final <- cbind(final,y_hat_glm_1)
items <- left_join(items,final,by=c("pid","size","date"))

#write.table(items,"final_submission/cluster_1_validation.csv",row.names = F,quote = F,sep = "|")
write.table(items,"final_submission/cluster_1.csv",row.names = F,quote = F,sep = "|")

# list_stats <- list(mae_val_rf=mae_val_rf,
#                    mse_val_rf=mse_val_rf,
#
#                    mae_val_xg=mae_val_xg,
#                    mse_val_xg=mse_val_xg,
#
#                    mae_val_glm=mae_val_glm,
#                    mse_val_glm=mse_val_glm,
#
#                    mae_val_bag=mae_val_bag,
#                    mse_val_bag=mse_val_bag)
#
# write.table(list_stats,"final_submission/cluster_1_statistics_validation_data.txt",row.names = F,quote = F,sep = "|")

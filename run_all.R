source("scripts/00_Modeling_Helper_Functions.R")
source("scripts/4_Generate_fold_specific_features.R")
source("scripts/0_Load_dataset_functions.R")

data <- get_train_data(imputed_data = T)
train_org <- delete_outlier_days(data)
test_feb <- get_test_data()
for (i in unique(data[data$price_is_dynamic == TRUE, "unique_id"])){
  plot_product_time_series(data, id = "17955:152",series_to_plot="prices", remove_leading_zeros=TRUE)
  
}
plot_product_time_series(data[data$price_is_dynamic == TRUE,],series_to_plot="prices", remove_leading_zeros=TRUE)

train_val <-
  get_features_fold_specific(train_org, test_feb, final_training = T)

test_final = train_val$validation
train <- train_val$train

source("scripts/cluster_0.R")
source("scripts/cluster_1.R")

#load predictions
cluster_0 = read.csv(file = "final_submission/cluster_0.csv", header =
                       TRUE, sep = "|")
cluster_0 <- cluster_0 %>% mutate(cluster = ifelse(!is.na(cluster_0),0,NA))


cluster_1 = read.csv(file = "final_submission/cluster_1.csv", header =
                       TRUE, sep = "|")
cluster_1 <- cluster_1 %>% mutate(cluster = ifelse(!is.na(cluster_1),1,NA))


#combine models
all_preds = inner_join(cluster_0,
                        cluster_1,
                        by = c("pid", "size", "date"))

all_preds$date <- as.Date(all_preds$date)

all_preds = inner_join(all_preds, test_final[, c("pid", "size", "date", "estimated_stock", "stock")], by = c("pid", "size", "date"))
all_preds <- all_preds %>% mutate(cluster_train = ifelse(!is.na(cluster.x) & is.na(cluster.y),"cluster_0","cluster_1"))
all_preds$cluster_train <- as.factor(all_preds$cluster_train)

all_preds$y_hat_final <-
  rowMeans(all_preds[, c(
    "y_hat_rf_0",
    "y_hat_xg_0",
    "cluster_1"
  )], na.rm = TRUE)


#predict soldout dates for the combined model
predicted_soldOutDates <-
  calc_soldOutDate_from_predictions(all_preds, all_preds$y_hat_final)
df <-
  as.data.frame(test_final %>% group_by(pid, size) %>% summarise(y = NA))
soldOutDates <-
  left_join(df, predicted_soldOutDates, by = c("pid", "size"))
soldOutDates$date_yhat <-
  replace_na(soldOutDates$date_yhat, as.Date("2018-02-28"))#


soldOutDates$y <- NULL
names(soldOutDates)[names(soldOutDates) == "date_yhat"] <-
  "soldOutDate"
soldOutDates$size = as.character(soldOutDates$size)
soldOutDates[soldOutDates$size == "OneSize", "size"] = ""

soldOutDates$soldOutDate <- as.Date(soldOutDates$soldOutDate)

#write final submission
write.table(df, "final_submission/TU_Muenchen_2.csv", sep="|",row.names = F, quote=FALSE)

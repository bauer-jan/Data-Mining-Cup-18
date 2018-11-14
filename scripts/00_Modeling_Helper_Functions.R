if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

#get all train data with all features
get_train_data <- function(imputed_data=T) {
  source("scripts/99_Generate_all_features.R")
  
  if (!file.exists("data/train_imputed_with_features.csv") & imputed_data) {
    print("File train_imputed_with_features.csv doesnt exists, run all features script...")
    extract_all_features(use_imputed_data = T)
  }
  
  if (!file.exists("data/train_with_features.csv") & !imputed_data) {
    print("File train_with_features.csv doesnt exists, run all features script...")
    extract_all_features(use_imputed_data = F)
  }
  
  if(imputed_data){
    train <- load_train_data()
    return(train)
  }else{
    train <- load_train_data(path="data/train_with_features.csv")
    return(train)
  }
  
}

#get test data (february) with all features
get_test_data <- function() {
  source("scripts/99_Generate_all_features.R")
  
  if (!file.exists("data/test_imputed_with_features.csv")) {
    extract_all_features(use_imputed_data = T)
  }
  
  return(load_test_data())
}

#calculates the mse for two vectors
mean_absolut_error <- function(data, y_hat,final_prediction=F){
  if(!final_prediction){
    res <- data$validation$unitsSold - y_hat
    res_null_modl <- data$validation$unitsSold - calc_mean_no_of_sales_per_day_predictions(data)
    
    mse <- round(mean(abs(res)), digits = 4)
    
    mse_null_model <- round(mean(abs(res_null_modl)), digits = 4)
    
    print(paste("MAE-modle:",mse,",MAE-Mean-Sales-modle:",mse_null_model))
    print("Quantiles of predictions:")
    print(quantile(y_hat,na.rm = T))
    
    return(mse)
  }else{
    print("Quantiles of predictions:")
    print(quantile(y_hat,na.rm = T))
    return(NULL)
  }
  
}

#delete days like christmas, end of december and black friday
delete_outlier_days <- function(data){
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  
  data <- data %>% filter((date != as.Date("2017-11-24")) | (date != as.Date("2017-12-24") | (date != as.Date("2018-01-01"))))
  
  return(data)
}

calc_soldOutDate_from_predictions <- function(data, estimated_unitsSold){
  
  data <- data[,c("pid","size","date","estimated_stock")]
  data$estimated_unitsSold <- estimated_unitsSold
  
  
  data <- data %>% arrange(pid,size,date)   %>% group_by(pid,size)  %>% arrange(pid,size,date)  %>% mutate(sumUnitsSold=cumsum(estimated_unitsSold))%>% ungroup
  
  data <- data %>% filter(estimated_stock <= sumUnitsSold)
  
  val <- data %>% group_by(pid, size) %>% summarise(min_value = min(sumUnitsSold))
  
  data <- inner_join(data,val[,c("pid","size","min_value")],
                     by=c("pid"="pid","size"="size","sumUnitsSold"="min_value"))
  
  data <- data[,c("pid","size","date")]
  
  names(data)[names(data) == 'date'] <- 'date_yhat'
  return(data)
}
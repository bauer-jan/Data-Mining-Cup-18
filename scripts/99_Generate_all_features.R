# ======================================================================
# Script for generating all features and for changing the datatypes of them
# provides a load function to load the train and test data, if the function extract_all_features()
# was executed once so the corresponding files are already stored 
# ======================================================================

#=======================================================================
# Output
# data - data set with all features (prices,external,general), fold specific
# features are not generated, the dataframe is ordered asc by date

# It saves the train and test file in the folder data/test_with_features.csv and
# data/train_with_features.csv
#=======================================================================

extract_all_features <- function(save_data=T,
                                 convert_into_right_datatypes=T,
                                 use_imputed_data=T){
  
  source("scripts/0_Load_dataset_functions.R")
  source("scripts/00_Data_cleaning.R")
  source("scripts/1_Generate_price_features.R")
  source("scripts/2_Generate_general_features.R")
  source("scripts/3_Generate_external_features.R")
  source("scripts/5_Generate_lag_diff_features.R")
  
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  
  #load data, merge and impute data set
  if(use_imputed_data){
    data <- get_data_imputed_missing_sell_dates_actual_prices()
  }else{
    data <- get_complete_merged_dataset_default(remove_old_index=F)
  }
  
  #replace missing values
  imputed_clean_data <- clean_data(data)
  
  # add lag, diff, moving average features
  imputed_clean_data_with_lag_diff_features <- add_product_lag_diff_ma_features(imputed_clean_data)
  
  #genereate some general features related to date, color and sizes/brand/categories
  data_with_genereal_features <- extract_general_features(data = imputed_clean_data_with_lag_diff_features)
  #generate features related to dynamic pricing
  data_with_price_features <- extract_price_features(train = data_with_genereal_features)
  #genereate/integrate external features such as weather data, google trends or football events
  data_with_external_features <- extract_external_features(train=data_with_price_features)
  
  #convert datatypes into the the probably best ones (e.g. categories into factor)
  if(convert_into_right_datatypes){
    data_with_external_features <- convert_into_right_datatypes(data_with_external_features)
  }
  
  #save train and test frame
  if(save_data){
    train_with_external_features = data_with_external_features %>% filter(date < as.Date("2018-02-01"))
    test_with_external_features = data_with_external_features %>% filter(date >= as.Date("2018-02-01"))
    
    if(use_imputed_data){
      write.table(train_with_external_features,file = "data/train_imputed_with_features.csv",sep = "|",quote = F, row.names = F)
      write.table(test_with_external_features,file = "data/test_imputed_with_features.csv",sep = "|",quote = F, row.names = F)
    }else{
      write.table(train_with_external_features,file = "data/train_with_features.csv",sep = "|",quote = F, row.names = F)
      
    }

  }
  data <- data %>% arrange(data_with_external_features)
  return(data_with_external_features)
}

#=======================================================================
# Input
# path to train data

# Output
# data - train data with columns converted incorresponding datatypes, the dataframe is ordered asc by date
#=======================================================================
load_train_data <- function(path="data/train_imputed_with_features.csv",
                                convert_into_right_datatypes=T){
  
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  
  data <- read.csv(path, sep ="|")
  
  if(convert_into_right_datatypes){
    data <- convert_into_right_datatypes(data)
  }
  
  data <- data %>% arrange(date)
  return(data)
  
}

#=======================================================================
# Input
# path to test data

# Output
# data - test data with columns converted incorresponding datatypes, the dataframe is ordered asc by date
#=======================================================================
load_test_data <- function(path="data/test_imputed_with_features.csv",
                               convert_into_right_datatypes=T){
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  
  data <- read.csv(path, sep ="|")
  
  if(convert_into_right_datatypes){
    data <- convert_into_right_datatypes(data)
  }
  
  #data <- data %>% arrange(date)
  return(data)
}

#=======================================================================
# Input
# data - whole data with all attributes, output of function: extract_all_features()

# Output
# data - data set with corresponding datatypes, 
# the function is only internal used
#=======================================================================

convert_into_right_datatypes <- function(data){
  data$unique_id <- as.character(data$unique_id)
  data$pid <- as.numeric(data$pid)
  data$date <- as.Date(data$date)
  data$size <- as.factor(data$size)
  
  data$unitsSold <- as.numeric(data$unitsSold)
  data$actualPrice <- as.numeric(data$actualPrice)
  data$color <- as.factor(data$color)
  data$brand <- as.factor(data$brand)
  data$rrp <- as.numeric(data$rrp)
  
  data$mainCategory <- as.factor(data$mainCategory)
  data$category <- as.factor(data$category)
  data$subCategory <- as.factor(data$subCategory)
  data$releaseDate <- as.Date(data$releaseDate)
  data$weekday <- as.factor(data$weekday)
  data$weekend <- as.numeric(data$weekend)
  data$day_in_month <- as.numeric(data$day_in_month)
  data$week_of_month <- as.numeric(data$week_of_month)
  
  data$days_since_release <- as.numeric(data$days_since_release)
  data$red <- as.numeric(data$red)
  data$green <- as.numeric(data$green)
  data$blue <- as.numeric(data$blue)
  data$itemCategory <- as.factor(data$itemCategory)
  data$standardized_size <- as.factor(data$standardized_size)
  data$target_group <- as.factor(data$target_group)
  data$price_is_dynamic <- as.numeric(data$price_is_dynamic)
  
  
  data$price_ratio_actualPrice_rrp <- as.numeric(data$price_ratio_actualPrice_rrp)
  data$price_ratio_actualPrice_meanPrice <- as.numeric(data$price_ratio_actualPrice_meanPrice)
  data$price_ratio_actualPrice_medianPrice <- as.numeric(data$price_ratio_actualPrice_medianPrice)
  data$price_ratio_actualPrice_priceSubCategory <- as.numeric(data$price_ratio_actualPrice_priceSubCategory)
  data$price_ratio_actualPrice_priceCategory <- as.numeric(data$price_ratio_actualPrice_priceCategory)
  data$price_ratio_actualPrice_priceSubCategory_size <- as.numeric(data$price_ratio_actualPrice_priceSubCategory_size)
  data$price_ratio_actualPrice_priceMainCategory_size <- as.numeric(data$price_ratio_actualPrice_priceMainCategory_size)
  data$price_is_below_median_subCategory <- as.numeric(data$price_is_below_median_subCategory)
  data$price_is_below_median_category <- as.numeric(data$price_is_below_median_category)
  data$price_is_below_median_subCategory_size <- as.numeric(data$price_is_below_median_subCategory_size)
  data$price_is_below_median_category_size <- as.numeric(data$price_is_below_median_category_size)
  data$price_is_below_median  <- as.numeric(data$price_is_below_median )
  data$price_trend <- as.numeric(data$price_trend)
  data$price_trend_in_last_7_days <- as.numeric(data$price_trend_in_last_7_days)
  data$price_category_of_item <- as.numeric(data$price_category_of_item)
  data$is_sale <- as.numeric(data$is_sale)
  data$mean_sunshine_duration <- as.numeric(data$mean_sunshine_duration)
  data$mean_temp <- as.numeric(data$mean_temp)
  data$mean_rainfall_height <- as.numeric(data$mean_rainfall_height)
  data$is_bundesliga_game <- as.numeric(data$is_bundesliga_game)
  data$is_champions_league_game <- as.numeric(data$is_champions_league_game)
  data$is_dfb_game <- as.numeric(data$is_dfb_game)
  data$is_dfb_pokal_game <- as.numeric(data$is_dfb_pokal_game)
  data$is_holiday <- as.numeric(data$is_holiday)
  data$google_trends_11teamsports <- as.numeric(data$google_trends_11teamsports)
  
  return(data)
  }
# =====================================================================================
# This Script is generating price features and returns them in a dataframe, in addition
# it saves them in data/features/price_featuers.csv.
# =====================================================================================

# =====================================================================================
# 0. Functions to generate the price features,
#    it returns the train dataframe with the additional features
# =====================================================================================
extract_price_features <- function(deleteCorrelatedFeatures = T,
                                   prices = get_converted_dynamic_prices_as_long_dataset(),
                                   train = get_complete_merged_dataset_default(remove_old_index =F),
                                   items_org = read.csv("data/items.csv", sep ="|")) {
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require(tidyr)) {
    install.packages("tidyr")
    library(tidyr)
  }
  
  #preprare dataframes by joins and chance of some column types
  items_org <- items_org %>% mutate(size=as.factor(size))
  levels(items_org$size) <- c(levels(items_org$size), "OneSize")
  items_org$size[items_org$size == ''] <- 'OneSize'
  
  train <- train %>% mutate(pid = as.numeric(pid),
                     size = as.factor(size),
                     date = as.Date(date))
  
  prices <- separate(prices,
             unique_id,
             c("pid", "size"),
             sep = ":",
             remove = F) %>% mutate(pid = as.numeric(pid),
                                    size = as.factor(size),
                                    date = as.Date(date))
  
  levels(prices$size) <- c(levels(prices$size), "OneSize")
  prices$size[prices$size == ''] <- 'OneSize'
  
  prices <- prices %>% left_join(., items_org, by = c("pid", "size"))
  prices <- prices %>% mutate(releaseDate = as.Date(releaseDate))
  
  prices <- mutate(prices, subCategory = replace_na(subCategory,-1))
  train <- mutate(train, subCategory = replace_na(subCategory,-1))
  
  #create the mean and median for each item over the whole timeperiod (oct-feb)
  train <- prices %>% group_by(unique_id) %>% summarise(
      price_mean = mean(actualPrice, na.rm = T),
      price_median = median(actualPrice, na.rm = T)) %>% left_join(train, ., by = "unique_id")
  
  #create median per subCategory/Category/MainCategory for every size in that specific category
  train <- prices %>% group_by(date, subCategory) %>% summarise(price_median_per_subCategory_date =
                                                           median(actualPrice, na.rm = T)) %>% left_join(train, ., by = c("date", "subCategory"))
  train <- prices %>% group_by(date, category) %>% summarise(price_median_per_category_date =
                                                        median(actualPrice, na.rm = T)) %>% left_join(train, ., by = c("date", "category"))
  train <- prices %>% group_by(date, mainCategory) %>% summarise(price_median_per_mainCategory_date =
                                                            median(actualPrice, na.rm = T)) %>% left_join(train, ., by = c("date", "mainCategory"))
  
  train <- prices %>% group_by(date, subCategory, size) %>% summarise(price_median_per_subCategory_date_size =
                                                                 median(actualPrice, na.rm = T)) %>% left_join(train, ., by = c("date", "subCategory", "size"))
  train <-  prices %>% group_by(date, category, size) %>% summarise(price_median_per_category_date_size =
                                                              median(actualPrice, na.rm = T)) %>% left_join(train, ., by = c("date", "category", "size"))
  train <-  prices %>% group_by(date, mainCategory, size) %>% summarise(price_median_per_mainCategory_date_size =
                                                                  median(actualPrice, na.rm = T)) %>% left_join(train, ., by = c("date", "mainCategory", "size"))
  
  #Min/Max price-values per item and size
  train <-  prices %>% group_by(pid, size) %>% summarise(
      price_min = min(actualPrice, na.rm = T),
      price_max = max(actualPrice, na.rm =T)) %>% mutate(price_is_dynamic = ifelse(price_min != price_max, 1, 0)) %>% left_join(train, ., by =
                                                                                          c("pid", "size"))
  
  #generate features out of the new columns, mainly features with some ratios between the actual price and the price of the categories/same sizes and same days
  train <- train %>% mutate(
      price_ratio_actualPrice_rrp = actualPrice / rrp,
      price_ratio_actualPrice_meanPrice = actualPrice / price_mean,
      price_ratio_actualPrice_medianPrice = actualPrice / price_median,
      price_ratio_actualPrice_priceSubCategory = actualPrice / price_median_per_subCategory_date,
      price_ratio_actualPrice_priceCategory = actualPrice / price_median_per_category_date,
      price_ratio_actualPrice_priceMainCategory =  actualPrice / price_median_per_mainCategory_date,
      price_ratio_actualPrice_priceSubCategory_size = actualPrice / price_median_per_subCategory_date_size,
      price_ratio_actualPrice_priceCategory_size = actualPrice / price_median_per_category_date_size,
      price_ratio_actualPrice_priceMainCategory_size =  actualPrice / price_median_per_mainCategory_date_size,
      price_is_below_median_subCategory = ifelse(actualPrice < price_median_per_subCategory_date, 1, 0),
      price_is_below_median_category = ifelse(actualPrice < price_median_per_category_date, 1, 0),
      price_is_below_median_subCategory_size = ifelse(actualPrice < price_median_per_subCategory_date_size, 1, 0),
      price_is_below_median_category_size = ifelse(actualPrice < price_median_per_category_date_size, 1, 0),
      price_is_below_median = ifelse(actualPrice < price_median, 1, 0)
    )
  
  #Price difference the days before/after teh actual date
  prices_trends <-  prices %>% group_by(pid, size) %>% .[order(prices$date),] %>% mutate(
      price_direction_lag_1 = actualPrice - lag(actualPrice, n = 1),
      price_direction_lag_2 = lag(actualPrice, n = 1) - lag(actualPrice, n = 2),
      price_direction_lag_3 = lag(actualPrice, n = 2) - lag(actualPrice, n = 3),
      price_direction_lag_4 = lag(actualPrice, n = 3) - lag(actualPrice, n = 4),
      price_direction_lag_5 = lag(actualPrice, n = 4) - lag(actualPrice, n = 5),
      price_direction_lag_6 = lag(actualPrice, n = 5) - lag(actualPrice, n = 6),
      price_direction_lag_7 = lag(actualPrice, n = 6) - lag(actualPrice, n = 7),
      price_direction_lead_1 = lead(actualPrice, n = 1) - actualPrice,
      price_direction_lead_2 = lead(actualPrice, n = 2) - lead(actualPrice, n = 1),
      price_direction_lead_3 = lead(actualPrice, n = 3) - lead(actualPrice, n = 2),
      price_direction_lead_4 = lead(actualPrice, n = 4) - lead(actualPrice, n = 3),
      price_direction_lead_5 = lead(actualPrice, n = 5) - lead(actualPrice, n = 4),
      price_direction_lead_6 = lead(actualPrice, n = 6) - lead(actualPrice, n = 5),
      price_direction_lead_7 = lead(actualPrice, n = 7) - lead(actualPrice, n = 6)
    )
  
  prices <- left_join(prices, prices_trends[, c(
      "pid",
      "size",
      "date",
      "price_direction_lag_1",
      "price_direction_lag_2",
      "price_direction_lag_3",
      "price_direction_lag_4",
      "price_direction_lag_5",
      "price_direction_lag_6",
      "price_direction_lag_7",
      "price_direction_lead_1",
      "price_direction_lead_2",
      "price_direction_lead_3",
      "price_direction_lead_4",
      "price_direction_lead_5",
      "price_direction_lead_6",
      "price_direction_lead_7"
    )], by = c("pid", "size", "date"))
  
  prices$price_trend <- rowSums(prices[, c(
      "price_direction_lag_1",
      "price_direction_lag_2",
      "price_direction_lag_3",
      "price_direction_lag_4",
      "price_direction_lag_5",
      "price_direction_lag_6",
      "price_direction_lag_7"
    )], na.rm = T)
  
  train <- left_join(train, prices[c("pid", "size", "date", "price_trend")], by = c("pid", "size", "date"))
  train$price_trend_in_last_7_days <- NA
  train$price_trend_in_last_7_days <- ifelse(train$price_trend < 0,-1, train$price_trend_in_last_7_days)
  train$price_trend_in_last_7_days <- ifelse(train$price_trend == 0, 0, train$price_trend_in_last_7_days)
  train$price_trend_in_last_7_days <- ifelse(train$price_trend > 0, 1, train$price_trend_in_last_7_days)
  
  #get quantiles for price for every sub category and then decide if item is cheap
  train <- prices %>% group_by(date, subCategory) %>% summarise("price_quantile_25" = quantile(actualPrice, probs = 0.25, na.rm = T),
      "price_quantile_75" = quantile(actualPrice, probs = 0.75, na.rm = T), n = n()) %>% left_join(train, ., by = c("date", "subCategory"))
  train$price_category_of_item <- NA
  train$price_category_of_item <- ifelse(train$actualPrice <= train$price_quantile_25, 0, 1)
  train$price_category_of_item <- ifelse(train$actualPrice >= train$price_quantile_75, 2, train$price_category_of_item)
  
  #discount for each product comparing to the initial price at the day 1 and to rrp
  prices <- prices %>% mutate(price_discount_to_rrp = actualPrice / rrp)
  df <- prices %>% group_by(pid, size) %>% arrange(date) %>% mutate(absoluteDiscount = cumsum(price_trend))
  train <- left_join(train, df[, c("pid", "size", "absoluteDiscount", "date")], by = c("pid", "size", "date"))
  
  #get quantiles for rrp for every sub category and then decide if item is cheap
  train <- prices %>% group_by(date, subCategory) %>% summarise( "rrp_quantile_25" = quantile(rrp, probs = 0.25, na.rm = T), "rrp_quantile_75" = quantile(rrp, probs = 0.75, na.rm = T), n = n()) %>% left_join(train, ., by = c("date", "subCategory"))
  train$price_rrp_category_of_item <- NA
  train$price_rrp_category_of_item <- ifelse(train$actualPrice <= train$rrp_quantile_25, 0, 1)
  train$price_rrp_category_of_item <- ifelse(train$actualPrice >= train$rrp_quantile_75, 2, train$price_rrp_category_of_item)
  
  #Identify items which are on sale
  prices$price_item_is_1_days_on_sale <- ifelse((( prices$price_direction_lag_1 == prices$price_direction_lead_1) & prices$actualPrice < prices$actualPrice - 1 * prices$price_direction_lag_1),1,0)
  prices$price_item_is_3_days_on_sale <- ifelse(( prices$price_direction_lag_3 == prices$price_direction_lead_3 & prices$actualPrice < prices$actualPrice - 1 * prices$price_direction_lag_1 - 1 * prices$price_direction_lag_2 - 1 * prices$price_direction_lag_3),1,0)
  prices$price_item_is_5_days_on_sale <- ifelse((prices$price_direction_lag_5 == prices$price_direction_lead_5 &prices$actualPrice < prices$actualPrice - 1 * prices$price_direction_lag_1 -1 * prices$price_direction_lag_2 - 1 * prices$price_direction_lag_3 - 1 *prices$price_direction_lag_4 - 1 * prices$price_direction_lag_5),1,0)
  
  train <- left_join(train, prices[, c(
      "pid",
      "size",
      "date",
      "price_item_is_1_days_on_sale",
      "price_item_is_3_days_on_sale",
      "price_item_is_5_days_on_sale"
    )], by = c("pid", "size", "date"))
  
  train$is_sale <- rowSums(train[, c(
      "price_item_is_1_days_on_sale",
      "price_item_is_3_days_on_sale",
      "price_item_is_5_days_on_sale"
    )], na.rm = T)
  
  train$is_sale <- ifelse(train$is_sale > 0, 1, 0)
  
  if (deleteCorrelatedFeatures) {
    drop.cols <- c(
      "price_median_per_subCategory_date",
      "price_median_per_category_date",
      "price_median_per_mainCategory_date",
      "price_median_per_subCategory_date_size",
      "price_median_per_category_date_size",
      "price_median_per_mainCategory_date_size",
      "price_min",
      "price_max",
      "price_quantile_25",
      "price_quantile_75",
      "rrp_quantile_25",
      "rrp_quantile_75",
      "price_ratio_actualPrice_priceMainCategory",
      "price_ratio_actualPrice_priceCategory_size",
      "price_mean",
      "price_median",
      "price_item_is_3_days_on_sale",
      "price_item_is_5_days_on_sale",
      "price_item_is_1_days_on_sale"
    )
    
    train <- train %>% select(-one_of(drop.cols))
  }
  train <- train %>% select(-one_of(c("n.x", "n.y")))
  
  return(train)
}

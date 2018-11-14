# ========================================================================================
# Script for feature engineering that is fold specific, such as certain averages or ratios
# ========================================================================================

#Helper function

#Function to calculate the mode
getmode <- function(x, na.rm = FALSE) {
  if (na.rm)
  {
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#=========================================================================================================

# Input
# train - data to train model with on a specific fold
# validation - data to validatie model on a specific fold


# Output
# preprocessed train data,containg new columns
# preprocessed validation data, computations from the train data are appended with respect to pid and size

#==========================================================================================================

get_features_fold_specific <-
  function(train, validation, final_training = FALSE) {
    if (!require(dplyr)) {
      install.packages("dplyr")
      library(dplyr)
    }

    train$mean_no_of_sales = NA
    train$mean_no_of_sales_per_day = NA
    train$median_no_of_sales = NA
    train$mode_no_of_sales = NA
    train$mean_no_of_sales_brand = NA
    train$mean_no_of_sales_subCategory = NA
    train$mean_no_of_sales_itemCategory = NA
    train$ratio_sales_to_stock = NA
    train$mean_days_between_sales = NA
    train$median_days_between_sales = NA
    #train$mode_days_between_sales = NA


    data_nonZero = filter(train, unitsSold > 0)


    #mean, median and mode of number of sales per item
    data_nonZero <- data_nonZero %>%
      group_by(unique_id) %>%
      mutate(
        mean_no_of_sales = mean(unitsSold),
        median_no_of_sales = median(unitsSold),
        mode_no_of_sales = getmode(unitsSold)
      ) %>%
      ungroup

    #performance index for brand
    data_nonZero <- data_nonZero %>%
      group_by(brand) %>%
      mutate(mean_no_of_sales_brand = mean(unitsSold)) %>%
      ungroup


    #performance index for subCategory
    data_nonZero <- data_nonZero %>%
      group_by(subCategory) %>%
      mutate(mean_no_of_sales_subCategory = mean(unitsSold)) %>%
      ungroup

    #performance index for itemCategory
    data_nonZero <- data_nonZero %>%
      group_by(itemCategory) %>%
      mutate(mean_no_of_sales_itemCategory = mean(unitsSold)) %>%
      ungroup


    # average/median/most frequent number of days beetween sales of a certain item
    data_nonZero <- data_nonZero %>%
      arrange(pid, size, date) %>%
      group_by(pid, size) %>%
      mutate(date_lag = lag(date),
             days_diff = as.integer(difftime(date, date_lag, units = "days"))) %>%
      mutate(
        mean_days_between_sales = mean(days_diff, na.rm = TRUE),
        median_days_between_sales = median(days_diff, na.rm = TRUE)
      ) %>%
      ungroup()


    #delete temporary columns
    data_nonZero$date_lag <- NULL
    data_nonZero$days_diff <- NULL


    data_nonZero <- data_nonZero %>%
      mutate(mean_days_between_sales = ifelse(
        is.na(mean_days_between_sales),
        days_since_release,
        mean_days_between_sales
      )) %>%
      mutate(
        median_days_between_sales = ifelse(
          is.na(median_days_between_sales),
          days_since_release,
          median_days_between_sales
        )
      )


    train <- rbind(data_nonZero, filter(train, unitsSold == 0))


    train <- train %>%
      group_by(pid, size) %>%
      mutate(mean_no_of_sales_per_day = mean(unitsSold, na.rm = TRUE)) %>%
      ungroup

    if (!final_training) {

      stock_per_day_val <- validation %>%
        group_by(unique_id) %>%
        summarise(estimated_stock_per_day = mean(unitsSold,na.rm = TRUE)) %>%
        mutate(estimated_stock_per_day = ifelse(estimated_stock_per_day == 0, 1/60, estimated_stock_per_day))

      stock_val <- validation %>%
        group_by(unique_id) %>%
        summarise(estimated_stock = sum(unitsSold,na.rm = TRUE)) %>%
        mutate(estimated_stock = ifelse(estimated_stock==0 ,1,estimated_stock))

      stock_train <- train %>%
        group_by(unique_id) %>%
        summarise(estimated_stock = sum(unitsSold,na.rm = TRUE)) %>%
        mutate(estimated_stock = ifelse(estimated_stock ==0 ,1,estimated_stock))

    } else{

      stock_per_day_val <- validation %>%
        group_by(unique_id) %>%
        summarise(estimated_stock_per_day = (min(stock,na.rm = T) / 28))

      stock_train <- train %>%
        group_by(unique_id) %>%
        summarise(estimated_stock = sum(unitsSold,na.rm = TRUE)) %>%
        mutate(estimated_stock = ifelse(estimated_stock ==0 ,1,estimated_stock))


    }

    if (!final_training) {

      sold_out <- validation %>% group_by(unique_id) %>% summarise(z = NA)
      sold_out_overZero <- validation %>% filter(unitsSold > 0) %>% group_by(unique_id) %>% summarise(sold_out_date = as.character(max(as.Date(date))))
      sold_out <- sold_out %>% left_join(sold_out_overZero, by = "unique_id")
      max_date = as.character(max(as.Date(validation$date)))
      sold_out <- sold_out %>% mutate(sold_out_date = ifelse(is.na(sold_out_date),max_date,sold_out_date))
      sold_out$z <- NULL
      sold_out$sold_out_date <- as.Date(sold_out$sold_out_date)

      validation <- validation %>% left_join(sold_out, by = "unique_id")

    }

    train <- train %>% left_join(stock_per_day_val, by = "unique_id")
    validation <- validation %>% left_join(stock_per_day_val, by = "unique_id")

    train <- train %>% left_join(stock_train, by = "unique_id")
    if(!final_training){
      validation <- validation %>% left_join(stock_val, by = "unique_id")
    }else{
      validation <- validation %>% mutate(estimated_stock = stock)
    }


    train$stock <-NULL

    train <- train %>%
      group_by(pid, size) %>%
      mutate(ratio_sales_to_stock = mean_no_of_sales_per_day / estimated_stock_per_day) %>%
    ungroup

    train$estimated_stock_per_day <- NULL
    validation$estimated_stock_per_day <- NULL


    train <- train %>%
      group_by(pid, size) %>%
      mutate(
        mean_no_of_sales_brand = min(mean_no_of_sales_brand, na.rm = TRUE),
        mean_no_of_sales_subCategory = min(mean_no_of_sales_subCategory, na.rm = TRUE),
        mean_no_of_sales_itemCategory = min(mean_no_of_sales_itemCategory, na.rm = TRUE),
        mean_no_of_sales = min(mean_no_of_sales, na.rm = TRUE),
        median_no_of_sales = min(median_no_of_sales, na.rm = TRUE),
        mode_no_of_sales = min(mode_no_of_sales, na.rm = TRUE),
        mean_days_between_sales = min(mean_days_between_sales, na.rm = TRUE),
        median_days_between_sales = min(median_days_between_sales, na.rm = TRUE),
        ratio_sales_to_stock = ifelse(is.na(ratio_sales_to_stock), 0, ratio_sales_to_stock)
      ) %>%
      ungroup


    train <- train %>%
      group_by(pid, size) %>%
      mutate(
        mean_no_of_sales_brand = ifelse(is.infinite(mean_no_of_sales_brand),0,mean_no_of_sales_brand),
        mean_no_of_sales_subCategory = ifelse(is.infinite(mean_no_of_sales_subCategory),0,mean_no_of_sales_subCategory),
        mean_no_of_sales_itemCategory = ifelse(is.infinite(mean_no_of_sales_itemCategory),0,mean_no_of_sales_itemCategory),
        mean_no_of_sales = ifelse(!is.finite(mean_no_of_sales),0,mean_no_of_sales),
        median_no_of_sales = ifelse(is.infinite(median_no_of_sales),0,median_no_of_sales),
        mode_no_of_sales = ifelse(is.infinite(mode_no_of_sales),0,mode_no_of_sales),
        mean_days_between_sales = ifelse(is.infinite(mean_days_between_sales),days_since_release,mean_days_between_sales),
        median_days_between_sales = ifelse(is.infinite(median_days_between_sales),days_since_release,median_days_between_sales),
        ratio_sales_to_stock = ifelse(is.infinite(ratio_sales_to_stock), 0, ratio_sales_to_stock)
      ) %>%
      ungroup

    z <- train %>%
      group_by(unique_id) %>%
      summarise(
        mean_no_of_sales_per_day = min(mean_no_of_sales_per_day),
        mean_no_of_sales_brand = min(mean_no_of_sales_brand),
        mean_no_of_sales_subCategory = min(mean_no_of_sales_subCategory),
        mean_no_of_sales_itemCategory = min(mean_no_of_sales_itemCategory),
        mean_no_of_sales = min(mean_no_of_sales),
        median_no_of_sales = min(median_no_of_sales),
        mode_no_of_sales = min(mode_no_of_sales),
        ratio_sales_to_stock = min(ratio_sales_to_stock),
        mean_days_between_sales = min(mean_days_between_sales),
        median_days_between_sales = min(median_days_between_sales)
      )

    validation <- validation %>% left_join(z, by = "unique_id")

    validation <- validation %>%
      mutate(
        mean_no_of_sales_per_day = ifelse(
          is.na(mean_no_of_sales_per_day),
          0,
          mean_no_of_sales_per_day
        ),
        mean_no_of_sales = ifelse(is.na(mean_no_of_sales), 0, mean_no_of_sales),
        median_no_of_sales = ifelse(is.na(median_no_of_sales), 0, median_no_of_sales),
        mode_no_of_sales = ifelse(is.na(mode_no_of_sales), 0, mode_no_of_sales),
        ratio_sales_to_stock = ifelse(is.na(ratio_sales_to_stock), 0, ratio_sales_to_stock),
        mean_days_between_sales = ifelse(
          is.na(ratio_sales_to_stock),
          days_since_release,
          ratio_sales_to_stock
        ),
        median_days_between_sales = ifelse(
          is.na(ratio_sales_to_stock),
          days_since_release,
          ratio_sales_to_stock
        )
      )



    validation <- validation %>%
      group_by(brand) %>%
      mutate(mean_no_of_sales_brand = min(mean_no_of_sales_brand, na.rm = TRUE))%>%
      ungroup

    validation <- validation %>%
      group_by(subCategory) %>%
      mutate(mean_no_of_sales_subCategory = min(mean_no_of_sales_subCategory, na.rm = TRUE))%>%
      ungroup

    validation <- validation %>%
      group_by(itemCategory) %>%
      mutate(mean_no_of_sales_itemCategory = min(mean_no_of_sales_itemCategory, na.rm = TRUE))%>%
      ungroup

    #new features

    train$stock <- train$mean_no_of_sales_per_day*30

    if(!final_training){
      validation$stock <- validation$estimated_stock
    }

    max_date <- max(train$date)

    df <- train %>% filter(unitsSold > 0 & releaseDate <= max_date) %>% group_by(unique_id) %>% summarise(number_of_sales_per_month=(n()/(as.integer(max_date-min(releaseDate)))*30))

    train <- left_join(train,df,by="unique_id")
    validation <- left_join(validation,df,by="unique_id")

    train <- train %>% mutate(number_of_sales_per_month=ifelse(is.na(number_of_sales_per_month),0,number_of_sales_per_month))

    validation <- validation %>% mutate(number_of_sales_per_month=ifelse(is.na(number_of_sales_per_month),0,number_of_sales_per_month))

    return(list(train = train, validation = validation))
    
  }

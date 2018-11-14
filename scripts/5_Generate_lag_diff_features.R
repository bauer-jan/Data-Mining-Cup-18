# ========================================================================================
# Script for feature engineering: lag and diff features, moving average of price
# ========================================================================================

# variance of sales is added as well


add_product_lag_diff_ma_features <- function(df) {
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  f <- df %>% dplyr::group_by(unique_id) %>% dplyr::summarise(var_sales = var(unitsSold, na.rm = T))
  df <- left_join(df, f)
  rm(f)
  
  df <- add_time_lag_diff_columns(df, "actualPrice", 0.7)
  row.names(df) <- c()
  df
}

add_time_lag_diff_columns <- function(df, column, beta=0.9){
  sp <- split.data.frame(df, df$unique_id)
  
  for (i in seq_along(sp)) {
    sp[[i]] <- compute_product_series_moving_average(sp[[i]], column, beta)
    sp[[i]] <- compute_sales_features_last_month(sp[[i]])
  }
  sp <- do.call("rbind", sp)
  sp
}


compute_product_series_moving_average <- function(p_df, column, beta=0.9) {
  ts <- as.data.frame(p_df %>% select(date, paste(column)))
  ma_col <- vector(mode = "numeric", length = length(ts[, column]))
  
  if (min(ts[, column]) == max(ts[, column])) {
    ma_col <- ts[, column]
  }
  else {
    ma_col[1] <- ts[1, column]
    for (i in 2:length(ts[, column])) {
      ma_col[i] <- beta * ma_col[i-1] + (1 - beta) * ts[i, column]
    }
  }
  
  newname <- paste("ma_", column, sep = "")
  p_df <- p_df %>% mutate(nc=ma_col)
  names(p_df)[names(p_df) == "nc"] <- newname
  p_df
}

compute_sales_features_last_month <- function(p_df) {
  p_df <- p_df %>% mutate(current_month=lubridate::month(date))
  feb <- as.double(unique(p_df$stock))
  
  g <- p_df %>% dplyr::group_by(current_month) %>% dplyr::summarise(unique_id = unique(unique_id),
                                                                    month_sum = sum(unitsSold),
                                                                    month_mean = mean(unitsSold))
  
  g <- g %>% tidyr::replace_na(list(month_sum=as.double(feb)))
  
  g <- g %>% mutate(current_month = if_else(current_month < 10, current_month+12, current_month)) %>% arrange(current_month)
  
  g <- g %>% dplyr::arrange(current_month) %>% mutate(last_month_sum=lag(month_sum, 1),
                                                      last_month_mean=lag(month_mean, 1))
  
  g <- g %>% dplyr::arrange(current_month) %>% mutate(diff_last_month_sum=c(0, diff(last_month_sum, lag = 1, 1)))
  g <- g %>% dplyr::arrange(current_month) %>% mutate(diff_last_month_mean=c(0, diff(last_month_mean, lag = 1, 1)))
  
  g <- g %>% replace_na(list(last_month_sum=0, diff_last_month_sum=0, last_month_mean=0, diff_last_month_mean=0))
  
  g <- g %>% mutate(current_month = if_else(current_month > 12, current_month-12, current_month))
  g$month_mean <- NULL
  left_join(p_df, g, by=c("unique_id", "current_month"))
}
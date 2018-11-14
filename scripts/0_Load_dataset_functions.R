
# =============================================================================
# 0. Functions to read the data
# =============================================================================

# Function 0.1

# a function to convert the dynamic prices dataset to a long dataset and return it as dataframe
# Input arguments:
#   - filename of prices.csv
#   - remove_pid_size: binary whether to remove pid and size or keep them

# Return dataframe has Columns:
#   - pid
#   - size
#   - date: date for the value of the dynamic price given by the next column (dynamicPrice)
#   - actualPrice: value of the price set for the day in the previous column (priceDate)

get_converted_dynamic_prices_as_long_dataset <- function(prices_file="data/prices.csv", remove_pid_size=TRUE) {
    if (!require(tidyr)) {
        install.packages("tidyr")
        library(tidyr)
    }
    if (!require(stringr)) {
        install.packages("stringr")
        library(stringr)
    }
    
    prices_long <- gather(read.csv(prices_file, sep="|"), date, actualPrice, -c(pid, size))
    
    prices_long$date <- str_replace_all(string = prices_long$date, pattern = "X", replacement = "")
    prices_long$date <- str_replace_all(string = prices_long$date, pattern = "\\.", replacement = "-")
    prices_long <- unite(prices_long, unique_id, pid, size, remove = T, sep = ":")
    prices_long
}


# ____________________________________________________________________________________

# Function 0.2

# a function to read train.csv and items.csv, merge them based on pid and size, return a dataframe with the 
# same indices or with one combined index

# Input arguments:
#   - filename of items.csv
#   - filename of train.csv 
#   - one_index: a boolean to indicate whether to return a df with one index only (pid+size)
#   - remove_old_index: If TRUE, old indices (pid, size) are removed, only in case of one_index = TRUE

# Returned dataframe has all the columns in items and train, but with the following renamed:
#   - unitsSold --> number of units sold on that date

get_merged_train_items <- function(train="data/train.csv", items="data/items.csv", one_index=TRUE, remove_old_index=TRUE) {
    if (!require(tidyr)) {
        install.packages("tidyr")
        library(tidyr)
    }
    
    items <- read.csv(items, sep = "|", stringsAsFactors=F)
    train <- read.csv(train, sep="|", stringsAsFactors=F)
    
    m <- merge(items, train, by=c("pid", "size"))
    
    names(m)[names(m) == "units"] <- "unitsSold"
    m$date <- as.character(m$date)
    
    if (one_index) {
        unite(m, unique_id, pid, size, remove = remove_old_index, sep = ":")
    }
    else {
        m
    }
}

# ____________________________________________________________________________________

# Function 0.3

# a function to merge all the data into a single dataframe

# Input arguments:
#   - dataframe train_items (train + items, outcome from function 0.2)
#   - dataframe prices (outcome from function 0.1)


# Returned dataframe has all the columns in items, train, prices, 
#   Note: merge is done on pid+size+date


get_complete_merged_dataset <- function(train_items, prices) {
    
    train_items$date <- as.character(train_items$date)
    
    merge(x = train_items, y = prices, by= c("unique_id", "date"))
}

# ____________________________________________________________________________________

# ____________________________________________________________________________________

# Function 0.4

# a function to merge all the data into a single dataframe without the need for input arguments

# Input arguments (all optional):
#   - dataframe train_items (train + items, outcome from function 0.2)
#   - dataframe prices (outcome from function 0.1)
#   - remove_old_index: If TRUE, old indices (pid, size) are removed, only in case of one_index = TRUE

# Returned dataframe has all the columns in items, train, prices, 
#   Note: merge is done on pid+size+date[sellDate in train, priceDate in prices]


get_complete_merged_dataset_default <- function(train_items=NULL, prices=NULL, remove_old_index=T) {
    if (is.null(train_items)) {
        train_items <- get_merged_train_items(one_index=TRUE, remove_old_index=remove_old_index)
    }
    
    if (is.null(prices)) {
        prices <- get_converted_dynamic_prices_as_long_dataset()
    }
    
    merge(x = train_items, y = prices, by= c("unique_id", "date"))
}

# ____________________________________________________________________________________

# Function 0.5

# a function to:
#   * merge all the data into a single dataframe and expand all missing combinations
#   * impute all missing sales data (units)
#   * impute all missing dynamic prices (actualPrice)


# Returned dataframe has all the columns in items, train, prices, including all the missing row combinations

# Parameters:
#   * units_imputed_value: the value to impute for sale units
#   * dprice_imputed_value: the value to impute for actual price
#   * drop_dates_before_release: if TRUE, all rows containing NAs for actual price (releaseDate after date) are removed

get_data_imputed_missing_sell_dates_actual_prices <- function(units_imputed_value=0, dprice_imputed_value=NA, 
                                                              drop_dates_before_release=TRUE) {
    if (!require(lubridate)) {
        install.packages("lubridate")
        library(lubridate)
    }
    
    if (!require(tidyr)) {
        install.packages("tidyr")
        library(tidyr)
    }
    
    if (!require(dplyr)) {
        install.packages("dplyr")
        library(dplyr)
    }
    
    train <- read.csv("data/train.csv", sep="|")
    train <- unite(train, unique_id, pid, size, remove = T, sep = ":")
    
    
    
    # merge the expanded grid
    x <- left_join(expand(train, unique_id, date), train)
    
    # impute missing values of sales to (given parameter value, default is ZERO)
    x <- x %>% mutate(units = replace(units, is.na(units), units_imputed_value))
    x$date <- as.character(x$date)
    
    # merge result with prices
    prices <- get_converted_dynamic_prices_as_long_dataset()
    
    y <- full_join(x, prices)
    
    # impute missing values of actualPrice to (given parameter value, default is NA)
    y <- y %>% mutate(actualPrice = replace(actualPrice, is.na(actualPrice), dprice_imputed_value))
    
    # merge with items at the end
    
    items <- read.csv("data/items.csv", sep = "|")
    items <- unite(items, unique_id, pid, size, remove = T, sep = ":")
    items$releaseDate <- as.character(items$releaseDate)
    
    z <- left_join(y, items)
    
    # get the size and pid columns again
    z <- separate(z, unique_id, c("pid", "size"), sep=":", remove = F)
    
    z$releaseDate <- as.Date(z$releaseDate)
    z$date <- as.Date(z$date)
    
    if (drop_dates_before_release) {
        z <- z %>% filter(date >= releaseDate)
    }
    
    names(z)[names(z) == "units"] <- "unitsSold"
    as.data.frame(z)
} 

# ____________________________________________________________________________________

# Function 0.6 (might improve it a bit more in the future)

# a function to plot the time series of sales or actual prices of a certain product
# Arguments:
# 1. series_to_plot can have two values:
#   - sales --> "sales"
#   - actualPrice --> "prices"
# 2.remove_leading_zeros: binary to remove leading zeros in sales data

plot_product_time_series <- function(df, id=NULL, series_to_plot="sales", remove_leading_zeros=TRUE) {
    if (!require(ggplot2)) {
        install.packages("ggplot2")
        library(ggplot2)
    }
    
    if (is.null(id)) {
        id <- df[sample(nrow(df), 1), "unique_id"]
    }
    
    p_df <- df[df$unique_id == id, ]
    
    if (series_to_plot == "sales") {
        if (remove_leading_zeros) {
            p_df <- p_df %>% filter(cumsum(p_df$unitsSold) != 0)
        }
        
        g <- ggplot(data = p_df, mapping = aes(x = date, y = unitsSold, group = 1)) + 
            geom_line(color="darkblue", size=1)
    }
    
    if (series_to_plot == "prices") {
        g <- ggplot() + 
            geom_line(data = p_df, aes(x = date, y = actualPrice, group = 1), color="darkblue", size=1) +
            geom_line(data = p_df, aes(x = date, y = rrp, group = 1), color="darkred", size=1)
    }
    g
} 


# ____________________________________________________________________________________

# Function 0.7 Remove some unsueful rows from the full imputed dataset


remove_rows_leading_trailing_zeros <- function(df, remove_leading_zeros=T, remove_trailing_zeros=T) {
  if (!require(FSA)) {
    install.packages("FSA")
    library(FSA)
  }
  
  if (missing(df)) {
    df <- get_data_imputed_missing_sell_dates_actual_prices()
  }
  
  df_splited <- lapply(split.data.frame(df, df$unique_id), remove_rows, remove_leading_zeros, remove_trailing_zeros)
  do.call("rbind", df_splited)
}


# Helper function, no need to call separately

remove_rows <- function(df, remove_leading_zeros=T, remove_trailing_zeros=T) {
  
  if (remove_leading_zeros) {
    df <- df %>% filter(cumsum(unitsSold) != 0)
  }
  
  if (remove_trailing_zeros) {
    df <- df %>% filter(rcumsum(unitsSold) != 0)
  }
  
  df
}




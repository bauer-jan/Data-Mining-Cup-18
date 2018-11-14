# =============================================================================
# This Script is generating external features and returns them in a dataframe,
# in addition it saves them in data/features/external_featuers.csv.
# =============================================================================

#==============================================================================
# Functions to generate new features and helping functions

# Input
# data to preprocess - whole imputed data set

# Output
# preprocessed data set containg new columns
#==============================================================================
get_google_trends_data <- function(train) {
  goole_trends_11_teamsports <- list(
    c("2017-10-01", 29),
    c("2017-10-08", 22),
    c("2017-10-15", 21),
    c("2017-10-22", 17),
    c("2017-10-29", 27),
    c("2017-11-05", 17),
    c("2017-11-12", 22),
    c("2017-11-19", 26),
    c("2017-11-26", 26),
    c("2017-12-03", 19),
    c("2017-12-10", 12),
    c("2017-12-17", 14),
    c("2017-12-24", 13),
    c("2017-12-31", 14),
    c("2018-01-07", 9),
    c("2018-01-14", 17),
    c("2018-01-21", 16),
    c("2018-01-28", 19),
    c("2018-02-04", 11),
    c("2018-02-11", 13),
    c("2018-02-18", 8),
    c("2018-02-25", 16)
  )
  #create dataframes out of lists
  goole_trends_11_teamsports <-
    as.data.frame(matrix(
      unlist(goole_trends_11_teamsports),
      nrow = length(goole_trends_11_teamsports),
      byrow = T
    ))
  
  #convert V1 column into date tye
  goole_trends_11_teamsports$V1 <-
    as.Date(goole_trends_11_teamsports$V1)
  goole_trends_11_teamsports$V2 <-
    as.numeric(levels(goole_trends_11_teamsports$V2))[goole_trends_11_teamsports$V2]
  #rename columns
  names(goole_trends_11_teamsports)[names(goole_trends_11_teamsports) == "V1"] <-
    "date"
  names(goole_trends_11_teamsports)[names(goole_trends_11_teamsports) == "V2"] <-
    "google_trends_11teamsports"
  
  
  #join data to train
  train <-
    left_join(train, goole_trends_11_teamsports, by = c("date"))
  
  start_date <- as.Date("2017-10-01")
  end_date <- start_date + 6
  train <- arrange(train, date)
  
  for (i in 1:22) {
    train[train$date >= start_date &
            train$date <= end_date, "google_trends_11teamsports"] <-
      goole_trends_11_teamsports[goole_trends_11_teamsports$date == start_date, "google_trends_11teamsports"]
    start_date <- start_date + 7
    end_date <- start_date + 6
  }
  
  
  return(train)
  
}

get_weather_data <- function(train) {
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require(rdwd)) {
    install.packages("rdwd")
    library(rdwd)
  }
  
  
  #Weather data
  link <-
    selectDWD("Muenchen-Stadt",
              res = "daily",
              var = "kl",
              per = "recent")
  file <- dataDWD(link,
                  read = FALSE,
                  dir = tempdir(),
                  quiet = TRUE)
  # tempdir is only for CRAN vignette checks. In real life, use a real folder.
  clim <- readDWD(file)
  names(clim)[names(clim) == "MESS_DATUM"] <- "date"
  names(clim)[names(clim) == "SDK"] <- "munich_sunshine_duration"
  names(clim)[names(clim) == "TMK"] <- "munich_temp"
  names(clim)[names(clim) == "RSK"] <- "munich_rainfall_height"
  clim$date <- as.Date(clim$date)
  train <-
    left_join(train, clim[, c("date",
                              "munich_sunshine_duration",
                              "munich_rainfall_height",
                              "munich_temp")], by = c("date"))
  
  #Hamburg
  link <-
    selectDWD(
      "Hamburg-Fuhlsbuettel",
      res = "daily",
      var = "kl",
      per = "recent"
    )
  file <- dataDWD(link,
                  read = FALSE,
                  dir = tempdir(),
                  quiet = TRUE)
  # tempdir is only for CRAN vignette checks. In real life, use a real folder.
  clim <- readDWD(file)
  names(clim)[names(clim) == "MESS_DATUM"] <- "date"
  names(clim)[names(clim) == "SDK"] <- "hamburg_sunshine_duration"
  names(clim)[names(clim) == "TMK"] <- "hamburg_temp"
  names(clim)[names(clim) == "RSK"] <- "hamburg_rainfall_height"
  clim$date <- as.Date(clim$date)
  train <-
    left_join(train, clim[, c("date",
                              "hamburg_sunshine_duration",
                              "hamburg_rainfall_height",
                              "hamburg_temp")], by = c("date"))
  
  #Berlin
  link <-
    selectDWD("Berlin-Tegel",
              res = "daily",
              var = "kl",
              per = "recent")
  file <- dataDWD(link,
                  read = FALSE,
                  dir = tempdir(),
                  quiet = TRUE)
  # tempdir is only for CRAN vignette checks. In real life, use a real folder.
  clim <- readDWD(file)
  names(clim)[names(clim) == "MESS_DATUM"] <- "date"
  names(clim)[names(clim) == "SDK"] <- "berlin_sunshine_duration"
  names(clim)[names(clim) == "TMK"] <- "berlin_temp"
  names(clim)[names(clim) == "RSK"] <- "berlin_rainfall_height"
  clim$date <- as.Date(clim$date)
  train <-
    left_join(train, clim[, c("date",
                              "berlin_sunshine_duration",
                              "berlin_rainfall_height",
                              "berlin_temp")], by = c("date"))
  
  #Essen/Koeln/dortmund
  link <-
    selectDWD("Essen-Bredeney",
              res = "daily",
              var = "kl",
              per = "recent")
  file <- dataDWD(link,
                  read = FALSE,
                  dir = tempdir(),
                  quiet = TRUE)
  # tempdir is only for CRAN vignette checks. In real life, use a real folder.
  clim <- readDWD(file)
  names(clim)[names(clim) == "MESS_DATUM"] <- "date"
  names(clim)[names(clim) == "SDK"] <- "essen_sunshine_duration"
  names(clim)[names(clim) == "TMK"] <- "essen_temp"
  names(clim)[names(clim) == "RSK"] <- "essen_rainfall_height"
  clim$date <- as.Date(clim$date)
  train <-
    left_join(train, clim[, c("date",
                              "essen_sunshine_duration",
                              "essen_rainfall_height",
                              "essen_temp")], by = c("date"))
  
  #Leipzg
  link <-
    selectDWD("Leipzig/Halle",
              res = "daily",
              var = "kl",
              per = "recent")
  file <- dataDWD(link,
                  read = FALSE,
                  dir = tempdir(),
                  quiet = TRUE)
  # tempdir is only for CRAN vignette checks. In real life, use a real folder.
  clim <- readDWD(file)
  names(clim)[names(clim) == "MESS_DATUM"] <- "date"
  names(clim)[names(clim) == "SDK"] <- "leipzig_sunshine_duration"
  names(clim)[names(clim) == "TMK"] <- "leipzig_temp"
  names(clim)[names(clim) == "RSK"] <- "leipzig_rainfall_height"
  clim$date <- as.Date(clim$date)
  train <-
    left_join(train, clim[, c("date",
                              "leipzig_sunshine_duration",
                              "leipzig_rainfall_height",
                              "leipzig_temp")], by = c("date"))
  
  #stuttgart
  link <-
    selectDWD(
      "Renningen-Ihinger Hof",
      res = "daily",
      var = "kl",
      per = "recent"
    )
  file <- dataDWD(link,
                  read = FALSE,
                  dir = tempdir(),
                  quiet = TRUE)
  # tempdir is only for CRAN vignette checks. In real life, use a real folder.
  clim <- readDWD(file)
  names(clim)[names(clim) == "MESS_DATUM"] <- "date"
  names(clim)[names(clim) == "SDK"] <- "stuttgart_sunshine_duration"
  names(clim)[names(clim) == "TMK"] <- "stuttgart_temp"
  names(clim)[names(clim) == "RSK"] <- "stuttgart_rainfall_height"
  clim$date <- as.Date(clim$date)
  train <-
    left_join(train, clim[, c(
      "date",
      "stuttgart_sunshine_duration",
      "stuttgart_rainfall_height",
      "stuttgart_temp"
    )], by = c("date"))
  
  train$mean_sunshine_duration <-
    rowMeans(train[, c(
      "stuttgart_sunshine_duration",
      "leipzig_sunshine_duration",
      "essen_sunshine_duration",
      "berlin_sunshine_duration",
      "hamburg_sunshine_duration",
      "munich_sunshine_duration"
    )])
  
  
  train$mean_temp <- rowMeans(train[, c(
    "stuttgart_temp",
    "leipzig_temp",
    "essen_temp",
    "berlin_temp",
    "hamburg_temp",
    "munich_temp"
  )])
  
  train$mean_rainfall_height <-
    rowMeans(train[, c(
      "stuttgart_rainfall_height",
      "leipzig_rainfall_height",
      "essen_rainfall_height",
      "berlin_rainfall_height",
      "hamburg_rainfall_height",
      "munich_rainfall_height"
    )])
  
  
  #delete columns with weather data of the cities
  train <- select(train, -contains("stuttgart"))
  train <- select(train, -contains("leipzig"))
  train <- select(train, -contains("essen"))
  train <- select(train, -contains("berlin"))
  train <- select(train, -contains("hamburg"))
  train <- select(train, -contains("munich"))
  return(train)
}

get_football_data <- function(train) {
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  #Football data
  football_dates_dfb <- list(c("2017-10-5", 1),
                             c("2017-10-8", 1),
                             c("2017-11-10", 1),
                             c("2017-11-14", 1))
  
  football_dates_dfb_pokal <- list(
    c("2017-10-24", 1),
    c("2017-10-25", 1),
    c("2017-12-19", 1),
    c("2017-12-20", 1),
    c("2018-2-6", 1),
    c("2018-2-7", 1)
  )
  
  football_dates_champions_league <- list(
    c("2017-10-17", 1),
    c("2017-10-18", 1),
    c("2017-10-31", 1),
    c("2017-11-1", 1),
    c("2017-11-21", 1),
    c("2017-11-22", 1),
    c("2017-12-5", 1),
    c("2017-12-6", 1),
    c("2017-12-11", 1),
    c("2018-2-13", 1),
    c("2018-2-14", 1),
    c("2018-2-20", 1),
    c("2018-2-21", 1)
  )
  
  football_dates_bundesliga <- list(
    c("2017-10-1", 1),
    c("2017-10-13", 1),
    c("2017-10-14", 1),
    c("2017-10-15", 1),
    c("2017-10-20", 1),
    c("2017-10-21", 1),
    c("2017-10-22", 1),
    c("2017-10-27", 1),
    c("2018-10-28", 1),
    c("2018-10-29", 1),
    
    c("2017-11-3", 1),
    c("2017-11-4", 1),
    c("2017-11-5", 1),
    c("2017-11-17", 1),
    c("2017-11-18", 1),
    c("2017-11-19", 1),
    c("2017-11-24", 1),
    c("2017-11-25", 1),
    c("2018-11-26", 1),
    
    c("2017-12-1", 1),
    c("2017-12-2", 1),
    c("2017-12-3", 1),
    c("2017-12-8", 1),
    c("2017-12-9", 1),
    c("2017-12-10", 1),
    c("2017-12-12", 1),
    c("2017-12-13", 1),
    c("2018-12-15", 1),
    c("2018-12-16", 1),
    c("2018-12-17", 1),
    
    c("2018-1-12", 1),
    c("2018-1-13", 1),
    c("2018-1-14", 1),
    c("2018-1-19", 1),
    c("2018-1-20", 1),
    c("2018-1-21", 1),
    c("2018-1-26", 1),
    c("2018-1-27", 1),
    c("2018-1-28", 1),
    
    c("2018-2-2", 1),
    c("2018-2-3", 1),
    c("2018-2-4", 1),
    c("2018-2-9", 1),
    c("2018-2-10", 1),
    c("2018-2-11", 1),
    c("2018-2-16", 1),
    c("2018-2-17", 1),
    c("2018-2-18", 1),
    c("2018-2-19", 1),
    c("2018-2-23", 1),
    c("2018-2-24", 1),
    c("2018-2-25", 1),
    c("2018-2-26", 1)
  )
  
  #create dataframes out of lists
  bundesliga <-
    as.data.frame(matrix(
      unlist(football_dates_bundesliga),
      nrow = length(football_dates_bundesliga),
      byrow = T
    ))
  champions_league <-
    as.data.frame(matrix(
      unlist(football_dates_champions_league),
      nrow = length(football_dates_champions_league),
      byrow = T
    ))
  dfb <-
    as.data.frame(matrix(
      unlist(football_dates_dfb),
      nrow = length(football_dates_dfb),
      byrow = T
    ))
  dfb_pokal <-
    as.data.frame(matrix(
      unlist(football_dates_dfb_pokal),
      nrow = length(football_dates_dfb_pokal),
      byrow = T
    ))
  
  #convert V1 column into date tye
  bundesliga$V1 <- as.Date(bundesliga$V1)
  champions_league$V1 <- as.Date(champions_league$V1)
  dfb$V1 <- as.Date(dfb$V1)
  dfb_pokal$V1 <- as.Date(dfb_pokal$V1)
  
  #rename columns
  names(bundesliga)[names(bundesliga) == "V1"] <- "date"
  names(bundesliga)[names(bundesliga) == "V2"] <-
    "is_bundesliga_game"
  names(champions_league)[names(champions_league) == "V1"] <- "date"
  names(champions_league)[names(champions_league) == "V2"] <-
    "is_champions_league_game"
  names(dfb)[names(dfb) == "V1"] <- "date"
  names(dfb)[names(dfb) == "V2"] <- "is_dfb_game"
  names(dfb_pokal)[names(dfb_pokal) == "V1"] <- "date"
  names(dfb_pokal)[names(dfb_pokal) == "V2"] <- "is_dfb_pokal_game"
  
  #join data to train
  train <-
    left_join(train, bundesliga, by = c("date")) %>% left_join(., champions_league, by =
                                                                 c("date")) %>% left_join(., dfb, by = c("date")) %>% left_join(., dfb_pokal, by =
                                                                                                                                  c("date")) %>% mutate(
                                                                                                                                    is_bundesliga_game = ifelse(is.na(is_bundesliga_game), 0, is_bundesliga_game),
                                                                                                                                    is_champions_league_game = ifelse(
                                                                                                                                      is.na(is_champions_league_game),
                                                                                                                                      0,
                                                                                                                                      is_champions_league_game
                                                                                                                                    ),
                                                                                                                                    is_dfb_game = ifelse(is.na(is_dfb_game), 0, is_dfb_game),
                                                                                                                                    is_dfb_pokal_game = ifelse(is.na(is_dfb_pokal_game), 0, is_dfb_pokal_game)
                                                                                                                                  )
  return(train)
  
  
  
}

get_holiday_data <- function(train) {
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  #Holiday data
  holidays <- list(
    c("2017-10-1", 1),
    c("2017-10-3", 1),
    c("2017-10-29", 1),
    c("2017-10-31", 1),
    
    c("2017-11-1", 1),
    c("2017-11-2", 1),
    c("2017-11-11", 1),
    c("2017-11-19", 1),
    c("2017-11-22", 1),
    c("2017-11-26", 1),
    
    c("2017-12-1", 1),
    c("2017-12-3", 1),
    c("2017-12-4", 1),
    c("2017-12-6", 1),
    c("2017-12-10", 1),
    c("2017-12-17", 1),
    c("2017-12-21", 1),
    c("2017-12-25", 1),
    c("2017-12-24", 1),
    c("2018-12-26", 1),
    c("2018-12-31", 1),
    
    c("2018-1-6", 1),
    
    c("2018-2-8", 1),
    c("2018-2-10", 1),
    c("2018-2-11", 1),
    c("2018-2-12", 1),
    c("2018-2-13", 1),
    c("2018-2-14", 1)
  )
  
  holidays <-
    as.data.frame(matrix(
      unlist(holidays),
      nrow = length(holidays),
      byrow = T
    ))
  names(holidays)[names(holidays) == "V1"] <- "date"
  names(holidays)[names(holidays) == "V2"] <- "is_holiday"
  holidays$date <- as.Date(holidays$date)
  
  
  train <-
    left_join(train, holidays, by = c("date")) %>% mutate(is_holiday = ifelse(is.na(is_holiday), 0, is_holiday))
  
  return(train)
}

#==============================================================================
# Function to combine the different feature generating functions

# Input
# data to preprocess - whole merged dataset, WITH IMPUTED VALUES

# Output
# preprocessed data set containg new columns
#==============================================================================
extract_external_features <-
  function(train = get_complete_merged_dataset_default(remove_old_index = F)) {
    if (!require(dplyr)) {
      install.packages("dplyr")
      library(dplyr)
    }
    
    #prepare data
    train <- train %>% mutate(date = as.Date(date))
    
    train <- get_weather_data(train)
    train <- get_football_data(train)
    train <- get_holiday_data(train)
    train <- get_google_trends_data(train)
    
    return(train)
  }

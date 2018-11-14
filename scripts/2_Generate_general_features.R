# ==================================================================
# Script for data preprocessing and generating general features related
# to different topics such as date, sales, sizes or categories
# ==================================================================
#===================================================================
# Helper functions
#===================================================================

#Function for calculatinhthe mode
getmode <- function(x, na.rm = FALSE)
{
  if (na.rm)
  {
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#====================================================================
# Functions to generate new features
# Input
# data to preprocess - whole imputed dataset

# Output
# preprocessed data set containg new columns
#====================================================================
#function to perform data preprocessing steps that are related to the date of a sale
get_date_features <- function(data) {
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }

  Sys.setlocale("LC_TIME", "C")

  #weekday
  data$weekday <- as.factor(weekdays(as.Date(data$date)))
  #binary feature to indicate if it is weekend or not
  data <- data %>% mutate(weekend = ifelse(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 0, 1))
  #day in month
  data$day_in_month = as.integer(format(as.Date(data$date, format = "%Y-%m-%d"), "%d"))
  # week of month
  data$week_of_month[data$day_in_month <= 7] = 1
  data$week_of_month[data$day_in_month <= 14 &
                       data$day_in_month > 7] = 2
  data$week_of_month[data$day_in_month <= 21 &
                       data$day_in_month > 14] = 3
  data$week_of_month[data$day_in_month <= 28 &
                       data$day_in_month > 21] = 4
  data$week_of_month[data$day_in_month > 28] = 5
  data$week_of_month <- sapply(data$week_of_month, as.numeric)
  # days between sale date and release date
  data$days_since_release = as.integer(as.Date(data$date) - as.Date(data$releaseDate))

  return(data)
}

get_color_features <- function(data) {
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require(stringr)) {
    install.packages("stringr")
    library(stringr)
  }
  # extracting RGB values
  #change to english names
  data$color[data$color == "gruen"] <- "green"
  data$color[data$color == "schwarz"] <- "black"
  data$color[data$color == "weiss"] <- "white"
  data$color[data$color == "blau"] <- "blue"
  data$color[data$color == "rot"] <- "red"
  data$color[data$color == "gelb"] <- "yellow"
  data$color[data$color == "lila"] <- "purple"
  data$color[data$color == "grau"] <- "grey"
  data$color[data$color == "rosa"] <- "pink"
  data$color[data$color == "pink"] <- "deeppink"
  data$color[data$color == "silber"] <- "grey75"
  data$color[data$color == "braun"] <- "brown"
  data$color[data$color == "tuerkis"] <- "turquoise"
  data$color[data$color == "orange"] <- "orange"
  data$color[data$color == "gold"] <- "gold"
  data$color[data$color == "khaki"] <- "khaki"
  data$color[data$color == "beige"] <- "beige"

  data[, c("red", "green", "blue")] <- NA

  for (color in unique(data$color)) {
    data$red[data$color == color] <- col2rgb(color)[1]
    data$green[data$color == color] <- col2rgb(color)[2]
    data$blue[data$color == color] <- col2rgb(color)[3]
  }

  return (data)

}

#function for standardizing sizes, assign item categories and target groups
get_sizes_categories_features <- function(data) {
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require(stringr)) {
    install.packages("stringr")
    library(stringr)
  }


  data$itemCategory <- NA
  #create category shoes/socks/clothes on the basis of SubCategory and the detected sizes there
  data$itemCategory[data$subCategory %in% c(11, 12, 13, 43)] = "socks"
  data$itemCategory[data$subCategory %in% c(3, 4, 5, 6, 19, 32, 38, 39, 44)] = "shoes"
  data$itemCategory[data$subCategory %in% c(8, 14, 16, 17, 20, 21, 22, 23, 25, 26, 28, 29, 31, 34, 35, 40, 41, 42, -1)] = "clothes"
  #there are items that do not have a subCategory
  # -> investigating them shows that there are only clothing sizes -> assign them to the category clothes
  data[is.na(data$itemCategory), ]$itemCategory = "clothes"

  data$itemCategory <- as.factor(data$itemCategory)


  # generate standardized size across different item categories and target groups
  # at the same time infer target group if possible

  data$standardized_size = NA
  data$target_group <- NA

  #Standardization for shoe sizes
  shoes = data[data$itemCategory == "shoes", ]

  shoes$size_2 = shoes$size
  shoes$size_2 = lapply(
    shoes$size_2,
    gsub,
    pattern = ",",
    replacement = ".",
    fixed = TRUE
  )
  shoes$size_2 = lapply(
    shoes$size_2,
    gsub,
    pattern = " 2/3",
    replacement = ".66",
    fixed = TRUE
  )
  shoes$size_2 = lapply(
    shoes$size_2,
    gsub,
    pattern = " 1/3",
    replacement = ".33",
    fixed = TRUE
  )
  shoes$size_2 = as.double(shoes$size_2)
  shoes$standardized_size[shoes$size_2 <= 35] = "XS"
  shoes$standardized_size[shoes$size_2 > 35 &
                            shoes$size_2 <= 38] = "S"
  shoes$standardized_size[shoes$size_2 > 38 &
                            shoes$size_2 <= 42] = "M"
  shoes$standardized_size[shoes$size_2 > 42 &
                            shoes$size_2 <= 46] = "L"
  shoes$standardized_size[shoes$size_2 > 46 &
                            shoes$size_2 <= 50] = "XL"
  shoes$standardized_size[shoes$size_2 > 50] = "XXL"

  shoes$size_2 <- NULL

  data[data$itemCategory == "shoes", ] = shoes

  #general size standardization
  data$standardized_size[grep("^XS", data$size)] = "XS"
  data$standardized_size[grep("^S", data$size)] = "S"
  data$standardized_size[grep("^M", data$size)] = "M"
  data$standardized_size[grep("^L", data$size)] = "L"
  data$standardized_size[grep("^XL", data$size)] = "XL"
  data$standardized_size[grep("^2XL", data$size)] = "XXL"
  data$standardized_size[grep("^3XL", data$size)] = "XXL"
  data$standardized_size[grep("^4XL", data$size)] = "XXL"

  data$standardized_size[data$size == "116"] = "XS"
  data$standardized_size[data$size == "128" |
                           data$size == "134"] = "S"
  data$standardized_size[data$size == "140" |
                           data$size == "152"] = "M"
  data$standardized_size[data$size == "158"] = "L"
  data$standardized_size[data$size == "164" |
                           data$size == "170"] = "XL"
  data$standardized_size[data$size == "176"] = "XXL"


  #Brand specific
  nike = data[data$brand == "Nike", ]
  nike$target_group[grep("\\( [0-9]{2}/", nike$size)] = "women"
  nike$target_group[grep("\\( [0-9]{3}-", nike$size)] = "children"
  nike$target_group[grep("^[X,S,M,L]{1,2}$", nike$size)] = "men"
  nike$target_group[grep("^2XL$", nike$size)] = "men"
  nike$target_group[is.na(nike$target_group)] = "unisex"

  data[data$brand == "Nike", ] = nike

  #adidas
  adidas = data[data$brand == "adidas" &
                  data$itemCategory %in% c("socks", "clothes"), ]

  adidas$target_group[grep("^[0-9]{3}$", adidas$size)] = "children"
  adidas$target_group[grep("^[X,S,M,L]{1,2} \\(", adidas$size)] = "women"
  adidas$target_group[grep("^(34|36|38|40|42)$", adidas$size)] = "women"
  adidas$target_group[grep("^[X,S,M,L]{1,2}", adidas$size)] = "men"
  adidas$target_group[grep("^[0-4]{1}XL", adidas$size)] = "men"
  adidas$target_group[grep("^[X,L]{1,2}/T", adidas$size)] = "men"
  adidas$target_group[is.na(adidas$target_group)] = "unisex"

  adidas$standardized_size[grep("^(0)* \\(", adidas$size)] = "XS"
  adidas$standardized_size[grep("^1 \\(", adidas$size)] = "S"
  adidas$standardized_size[grep("^35 - 38$", adidas$size)] = "S"
  adidas$standardized_size[grep("^(2|3) \\(", adidas$size)] = "M"
  adidas$standardized_size[grep("^39 - 42$", adidas$size)] = "M"
  adidas$standardized_size[grep("^4 \\(", adidas$size)] = "L"
  adidas$standardized_size[grep("^43 - 46$", adidas$size)] = "L"
  adidas$standardized_size[grep("^5 \\(", adidas$size)] = "XL"
  adidas$standardized_size[grep("^47 - 50$", adidas$size)] = "XL"


  adidas$standardized_size[adidas$size == "34"] = "XS"
  adidas$standardized_size[adidas$size == "36"] = "S"
  adidas$standardized_size[adidas$size == "38"] = "M"
  adidas$standardized_size[adidas$size == "40"] = "L"
  adidas$standardized_size[adidas$size == "42"] = "L"

  adidas$standardized_size[adidas$size == "OneSize"] = "OneSize"

  data[data$brand == "adidas" &
         data$itemCategory %in% c("socks", "clothes"), ] = adidas


  #PUMA
  puma = data[data$brand == "PUMA", ]

  puma$standardized_size[grep("^1 ", puma$size)] = "XS"
  puma$standardized_size[grep("^2 ", puma$size)] = "S"
  puma$standardized_size[grep("^2$", puma$size)] = "S"
  puma$standardized_size[grep("^35", puma$size)] = "S"
  puma$standardized_size[grep("^3 ", puma$size)] = "M"
  puma$standardized_size[grep("^3$", puma$size)] = "M"
  puma$standardized_size[grep("^39", puma$size)] = "M"
  puma$standardized_size[grep("^4 ", puma$size)] = "L"
  puma$standardized_size[grep("^4$", puma$size)] = "L"
  puma$standardized_size[grep("^43", puma$size)] = "L"
  puma$standardized_size[grep("^5 ", puma$size)] = "XL"
  puma$standardized_size[grep("^47", puma$size)] = "XL"


  puma$target_group[grep("^[0-9]{3}$", puma$size)] = "children"
  puma$target_group[is.na(puma$target_group)] = "unisex"


  data[data$brand == "PUMA", ] = puma

  #Jako
  jako = data[data$brand == "Jako" &
                data$itemCategory %in% c("socks", "clothes"), ]

  #women
  jako$standardized_size[jako$size == "36"] = "S"
  jako$standardized_size[grep("^38", jako$size)] = "M"
  jako$standardized_size[jako$size == "40"] = "M"
  jako$standardized_size[grep("^19", jako$size)] = "M"
  jako$standardized_size[jako$size == "42"] = "L"
  jako$standardized_size[jako$size == "44"] = "XL"

  jako$target_group[grep("^(36|38|40|42|42|44)$", jako$size)] = "women"
  jako$target_group[grep("^38/40", jako$size)] = "women"

  #socks
  jako$standardized_size[grep("^1 \\( 25", jako$size)] = "XS"
  jako$standardized_size[grep("^2 \\( 31", jako$size)] = "XS"
  jako$standardized_size[grep("^3 \\(35", jako$size)] = "S"
  jako$standardized_size[grep("^4 \\( 39", jako$size)] = "M"
  jako$standardized_size[grep("^5 \\( 43", jako$size)] = "L"
  jako$standardized_size[grep("^6 \\( 47", jako$size)] = "XL"

  #children
  jako$standardized_size[jako$size == "116/128"] = "S"
  jako$standardized_size[jako$size == "140/152"] = "M"
  jako$standardized_size[jako$size == "164/176"] = "L"

  jako$standardized_size[jako$size == "116"] = "XS"
  jako$standardized_size[jako$size == "128"] = "S"
  jako$standardized_size[jako$size == "134"] = "S"
  jako$standardized_size[jako$size == "140"] = "M"
  jako$standardized_size[jako$size == "146"] = "M"
  jako$standardized_size[jako$size == "152"] = "M"
  jako$standardized_size[jako$size == "158"] = "L"
  jako$standardized_size[jako$size == "164"] = "L"
  jako$standardized_size[jako$size == "176"] = "XL"

  jako$standardized_size[jako$size == "2"] = "S"
  jako$standardized_size[jako$size == "3"] = "M"
  jako$standardized_size[jako$size == "4"] = "L"


  jako$target_group[grep("^[0-9]{3}$", jako$size)] = "children"
  jako$target_group[grep("^[0-9]{3}/", jako$size)] = "children"
  jako$target_group[grep("Junior", jako$size)] = "children"
  jako$target_group[jako$size %in% c("2", "3", "4")] = "children"

  jako$standardized_size[jako$size == "102 (M)" |
                           jako$size == "24 (M)"] = "M"
  jako$standardized_size[jako$size == "28 (3XL)" |
                           jako$size == "30 (5XL)"] = "XXL"

  jako$standardized_size[jako$size == "5"] = "S"
  jako$standardized_size[jako$size == "6"] = "M"
  jako$standardized_size[jako$size == "7"] = "L"
  jako$standardized_size[jako$size == "8"] = "XL"
  jako$standardized_size[jako$size %in% c("9", "10")] = "XL"

  jako$target_group[is.na(jako$target_group)] = "unisex"


  data[data$brand == "Jako" &
         data$itemCategory %in% c("socks", "clothes"), ] = jako

  #Jordan
  jordan = data[data$brand == "Jordan", ]

  # no more sizes to standardize
  jordan$target_group[is.na(jordan$target_group)] = "unisex"

  data[data$brand == "Jordan", ] = jordan


  #Reusch
  reusch = data[data$brand == "Reusch", ]

  # no more sizes to standardize
  reusch$target_group[is.na(reusch$target_group)] = "unisex"

  data[data$brand == "Reusch", ] = reusch

  #Cinquestelle
  #only shoes, no more sizes to standardize
  c = data[data$brand == "Cinquestelle", ]

  # no more sizes to standardize
  c$target_group[is.na(c$target_group)] = "unisex"

  data[data$brand == "Cinquestelle", ] = c

  #Sport2000
  sport = data[data$brand == "Sport2000", ]

  sport$target_group[grep("\\( [0-9]{2}/", sport$size)] = "women"
  sport$target_group[grep("^[X,S,M,L]{1,2}$", sport$size)] = "men"
  sport$target_group[is.na(sport$target_group)] = "unisex"

  data[data$brand == "Sport2000", ] = sport


  #Lotto
  #only shoes, no more sizes to standardize
  lotto = data[data$brand == "Lotto", ]

  # no more sizes to standardize
  lotto$target_group[is.na(lotto$target_group)] = "unisex"

  data[data$brand == "Lotto", ] = lotto


  #Stance
  stance = data[data$brand == "Stance", ]

  # no more sizes to standardize
  stance$target_group[is.na(stance$target_group)] = "unisex"

  data[data$brand == "Stance", ] = stance


  #KangaROOS
  #only shoes, no more sizes to standardize
  kanga = data[data$brand == "KangaROOS", ]

  # no more sizes to standardize
  kanga$target_group[is.na(kanga$target_group)] = "unisex"

  data[data$brand == "KangaROOS", ] = kanga


  #Sells - !! Only one item
  sells = data[data$brand == "Sells", ]

  # no more sizes to standardize
  sells$target_group[is.na(sells$target_group)] = "unisex"

  data[data$brand == "Sells", ] = sells


  #Kempa - !! Only one item
  kempa = data[data$brand == "Kempa", ]

  # no more sizes to standardize
  kempa$target_group[is.na(kempa$target_group)] = "unisex"

  data[data$brand == "Kempa", ] = kempa

  #Onitsuka - !! Only one item
  oni = data[data$brand == "Onitsuka", ]

  # no more sizes to standardize
  oni$target_group[is.na(oni$target_group)] = "unisex"

  data[data$brand == "Onitsuka", ] = oni

  #FREAM - !! Only one item
  fream = data[data$brand == "FREAM", ]

  # no more sizes to standardize
  fream$target_group[is.na(fream$target_group)] = "unisex"

  data[data$brand == "FREAM", ] = fream

  #Reebok
  reebok = data[data$brand == "Reebok", ]

  # no more sizes to standardize
  reebok$target_group[is.na(reebok$target_group)] = "unisex"

  data[data$brand == "Reebok", ] = reebok

  #Asics
  asics = data[data$brand == "Asics", ]

  # no more sizes to standardize
  asics$target_group[is.na(asics$target_group)] = "unisex"

  data[data$brand == "Asics", ] = asics

  #Mizuno
  miz = data[data$brand == "Mizuno", ]

  # no more sizes to standardize
  miz$target_group[is.na(miz$target_group)] = "unisex"

  data[data$brand == "Mizuno", ] = miz


  #Converse
  con = data[data$brand == "Converse", ]

  # no more sizes to standardize
  con$target_group[is.na(con$target_group)] = "unisex"

  data[data$brand == "Converse", ] = con


  #New Balance
  newB = data[data$brand == "New Balance", ]

  # no more sizes to standardize
  newB$target_group[is.na(newB$target_group)] = "unisex"

  data[data$brand == "New Balance", ] = newB

  #Uhlsport
  uhl = data[data$brand == "Uhlsport", ]

  uhl$standardized_size[uhl$size == "104"] = "XS"
  uhl$standardized_size[uhl$size == "116-122"] = "XS"

  uhl$standardized_size[uhl$size == "37 - 40"] = "M"
  uhl$standardized_size[uhl$size == "41 - 44"] = "L"
  uhl$standardized_size[uhl$size == "45 - 47"] = "XL"

  uhl$standardized_size[uhl$size == "OneSize"] = "OneSize"

  uhl$target_group[uhl$size == "104"] = "children"
  uhl$target_group[uhl$size == "116-122"] = "children"
  uhl$target_group[is.na(uhl$target_group)] = "unisex"


  data[data$brand == "Uhlsport", ] = uhl


  #Under Armour
  underArmour = data[data$brand == "Under Armour", ]

  underArmour$standardized_size[grep("^YLG", underArmour$size)] = "L"
  underArmour$standardized_size[grep("^YM", underArmour$size)] = "M"
  underArmour$standardized_size[grep("^YSM", underArmour$size)] = "S"
  underArmour$standardized_size[grep("^YXL", underArmour$size)] = "XL"

  underArmour$target_group[grep("^Y", underArmour$size)] = "children"
  underArmour$target_group[is.na(underArmour$target_group)] = "unisex"

  data[data$brand == "Under Armour",] = underArmour


  #Erima
  erima = data[data$brand == "Erima", ]

  erima$standardized_size[erima$size == "43-46"] = "L"
  erima$standardized_size[erima$size == "39-42"] = "M"
  erima$standardized_size[erima$size == "42"] = "L"
  erima$standardized_size[erima$size == "4 ( 44-46 )"] = "XL"
  erima$standardized_size[grep("^7 \\( L|44-46)", erima$size)] = "L"
  erima$standardized_size[grep("(^8 \\( XL|^46|^44)", erima$size)] = "XL"
  erima$standardized_size[grep("(^38|^40|41-43)", erima$size)] = "M"
  erima$standardized_size[grep("(^36|37-40|33-36|128)", erima$size)] = "S"
  erima$standardized_size[grep("(^34)", erima$size)] = "XS"
  erima$standardized_size[grep("(^11)", erima$size)] = "XXL"

  erima$standardized_size[grep("164", erima$size)] = "XL"
  erima$standardized_size[grep("(152|140)", erima$size)] = "M"

  erima$target_group[grep("(^34|^36|^38|^40|^44|^46)", erima$size)] = "women"
  erima$target_group[grep("[0-9]{3}", erima$size)] = "children"
  erima$target_group[is.na(erima$target_group)] = "unisex"


  data[data$brand == "Erima", ] = erima


  #Hummel
  hummel = data[data$brand == "Hummel", ]

  hummel$standardized_size[grep("(^12|45-48)", hummel$size)] = "L"
  hummel$standardized_size[grep("^14", hummel$size)] = "XL"
  hummel$standardized_size[grep("(^10)", hummel$size)] = "M"
  hummel$standardized_size[grep("(^2$|)", hummel$size)] = "S"
  hummel$standardized_size[grep("(^6/8)", hummel$size)] = "XS"
  hummel$standardized_size[grep("(^16)", hummel$size)] = "XXL"


  hummel$target_group[grep("[0-9]{3}", hummel$size)] = "children"
  hummel$target_group[is.na(hummel$target_group)] = "unisex"

  data[data$brand == "Hummel", ] = hummel


  #Diadora
  dia = data[data$brand == "Diadora", ]

  # no more sizes to standardize
  dia$target_group[is.na(dia$target_group)] = "unisex"

  data[data$brand == "Diadora", ] = dia


  data$standardized_size[grep("Junior", data$size)] = "S"
  data$standardized_size[grep("Senior", data$size)] = "L"
  data$standardized_size[grep("Bambini", data$size)] = "XS"

  data$target_group[grep("Junior", data$size)] = "children"
  data$target_group[grep("Senior", data$size)] = "unisex"
  data$target_group[grep("Bambini", data$size)] = "children"


  data$target_group[is.na(data$target_group)] = "unisex"


  return(data)

}


#=====================================================================
# Function to combine the different feature generating functions

# Input
# data to preprocess - whole merged dataset, WITH IMPUTED VALUES

# Output
# preprocessed data set containg new columns
#======================================================================

extract_general_features <- function(data) {
  #Feature engineering related to date
  data <- get_date_features(data)

  #Feature Engineering related to color
  data <- get_color_features(data)

  #Feature Engineering related to brands/sizes/categories
  data <- get_sizes_categories_features(data)


  #return the preprocessed data set
  return(data)

}

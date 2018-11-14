# ======================================================================
# Script for cleaning the data, more precisely replacing missing values
# ======================================================================

#=======================================================================
# Input
# data - whole imputed data set

# Output
# data - data set with replaced missing values
#=======================================================================

clean_data <- function(data) {
  data$subCategory[is.na(data$subCategory)] <- -1  
  data$size <- as.factor(data$size)

  levels(data$size) <- c(levels(data$size), "OneSize")
  data$size[data$size == ''] <- 'OneSize'

  return(data)
}

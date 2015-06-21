## R programming tutorial for assignment 3
## practice ordering data

sort_by_column <- function(data,column){
  orderdata <- data[order(data[,column]),]
  return(orderdata)
}

sort_by_columns <- function(data, col1, col2){
  orderdata <- data[order(data[,col1],data[,col2]),]
  return(orderdata)
  
}
## sort data by countries

sort_country <- function(data, country, column){
  countrydata <- data[grep(country,data$countries),]
  orderdata <- countrydata[order(countrydata[,column]),]
  return(orderdata)
}
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
# Sort  by column and remove NA

sort_by_column_NA <- function(data, column){
  for (i in 3:4){
    data[,i] <- suppressWarnings(as.numeric(levels(data[,i]),data[,i]))
  }
  orderdata <- data[order(data[,column]),]
  orderdata <- orderdata[complete.cases(orderdata),]
  return(orderdata)
}
# return speccific ranking after ordering
find_city_rank <- function(data, column, rank){
  orderdata <- data[order(decreasing = TRUE,data[,column]),]
  return(as.character(orderdata[rank,1]))
}
find_last_city <- function( data, column){
  orderdata <- data[order(decreasing = TRUE, data[,column]),]
  return(as.character(orderdata[nrow(orderdata),1]))
}
rank_by_country <- function(data,column,rank){
  ## We save the levels of column 2, the countries' names, in the countries vector
  countries <- levels(data[,2])
  
  ## We generate an empty vector that we will fill later, row by row, to generate our final output
  output <- vector()
  
  ## For loop to get the right data on each city. length(countries) is the number of different countries in our
  ## database. In our case we have 3 countries: China, UK, USA.
  for (i in 1:length(countries)) {
    
    ## countrydata subsets data by the considered country
    countrydata <- data [grep(countries[i],data$countries),]
    orderdata <- countrydata[order(decreasing = TRUE, countrydata[,column]),]
    
    ## append() adds elements at the end of a vector. We want to add the name of the city [rank,1],
    ## the areakm2 [rank,2] and the populationk [rank,3]. We don't add the name of the countries, because it
    ## will be the label of the rows.
    output <- append (output, as.character(orderdata[rank,1]))
    for (l in 3:4){
      output <- append (output, as.character(orderdata[rank,l]))
    }
  }
  ## Just because it's simpler to generate a matrix rather than a data frame, I generate it first and convert it
  ## to data frame immediatly after. 
  output <- as.data.frame(matrix(output,length(countries),3, byrow = TRUE))
  
  ## Name of the columns will be "cities", "areakm2" and "populationk". Name of the rows are the countries.
  colnames(output) <- c("cities","areakm2","populationk")
  rownames(output) <- countries
  return(output)
}
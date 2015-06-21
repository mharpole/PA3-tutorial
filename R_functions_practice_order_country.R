## sort data by countries

sort_country <- function(data, country, column){
  countrydata <- data[grep(country,data$countries),]
  orderdata <- countrydata[order(countrydata[,column]),]
  return(orderdata)
}
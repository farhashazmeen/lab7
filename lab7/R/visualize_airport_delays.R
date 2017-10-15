library(nycflights13)
library(ggplot2)
library(dplyr)

#' Handling large datasets with dplyr
#'
#' This function creates a plot which visualizes a large dataset.
#' @return returns plot.
#' @export
#'
#' @examples visualize_airport_delays()
#' 
#' @references \href{https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf}{DataWrangling}
#' 
#' 
visualize_airport_delays<-function(){

  airports <- nycflights13::airports
  flights<-nycflights13::flights
  
  merged_results<- dplyr::inner_join(airports,flights, by = c("faa"="dest"))
  mean_delay <- merged_results %>%  group_by(faa) %>%  dplyr::summarise(Delay = mean(arr_delay,na.rm = TRUE))
  latlong <- merged_results %>% group_by(faa) %>%   dplyr::summarise(Lat =lat[1],Lon =lon[1])
  df <- data.frame(mean_delay, latlong[,2:3])
  plot_delay <- ggplot(df,aes(x=df$Lat, y =df$Lon))+
  geom_point(aes(color=df$Delay),size=4)+
  labs(title= "Visualising delay ", x= "Latitude",y="Longitude")
  return(plot_delay)
  } 

visualize_airport_delays()
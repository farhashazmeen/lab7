library(nycflights13)
library(ggplot2)
library(dplyr)


#' Title
#'
#' @return returns plot.
#' @export
#'
#' @examples
#' visualize_airport_delays()
visualize_airport_delays<-function(){
  
  airports <- nycflights13::airports
  flights<-nycflights13::flights
  
  merged_results<- inner_join(airports,flights, by = c("faa"="dest"))
  mean_delay <- merged_results %>%  group_by(faa) %>%  dplyr::summarise(Delay = mean(arr_delay,na.rm = TRUE))
  latlong <- merged_results %>% group_by(faa) %>%   dplyr::summarise(Lat =lat[1],Lon =lon[1])
  latlong
    df <- data.frame(mean_delay, latlong[,2:3])

  df


 plot_delay <- ggplot(df,aes(x=df$Lat, y =df$Lon))+
 geom_point(aes(color=df$Delay),size=4)+
 labs(title= "Visualising delay ", x= "Latitude",y="Longitude")


  plot_delay

  } 


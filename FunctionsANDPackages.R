airtemps<-c(212,30.3,78,32)
celsius1<-(airtemps[1]-32)*5/9
celsius1
celsius4<-(airtemps[4]-32)*5/9
celsius4


fahr_to_celsius <-function(fahr){
  celsius<-(fahr-32)*5/9
  return(celsius)
}
celsius_new<-fahr_to_celsius(airtemps)
celsius_new

fahr_to_celsius(airtemps)


#' Convert temperatures from C to F
#' 
#' Longer description of the function
#'
#' @param celsius Numeric temperature value in C
#'
#' @return Numeric value in Fahrenheit
#' @export
#'
#' @examples
celsius_to_fahr <-function(celsius){
  fahr<-(celsius*9/5)+32
  return(fahr)
}
fahr_new<-celsius_to_fahr(celsius_new)
fahr_new
fahr_new == airtemps


convert_temps<-function(fahr){
  celsius<-(fahr-32)*5/9
  kelvin<-celsius+273.15
  return(data.frame(fahr=fahr,
                     celsius=celsius,
                     kelvin=kelvin))
}
df<-convert_temps(airtemps)
df





convert_temps <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  kelvin <- celsius + 273.15
  return(list(fahr = fahr, celsius = celsius, kelvin = kelvin))
}
temps_df <- data.frame(convert_temps(seq(-100,100,10)))


custom_theme <- function(base_size = 9) {
  ggplot2::theme(
    text             = ggplot2::element_text(family = 'Helvetica', 
                                             color = 'gray30', 
                                             size = base_size),
    plot.title       = ggplot2::element_text(size = ggplot2::rel(1.25), 
                                             hjust = 0.5, 
                                             face = 'bold'),
    panel.background = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = 'grey90', 
                                             linewidth = 0.25),
    legend.position  = 'right',
    legend.key       = ggplot2::element_rect(colour = NA, 
                                             fill = NA),
    axis.ticks       = ggplot2::element_blank(),
    axis.line        = ggplot2::element_blank()
  )
}

library(ggplot2)


ggplot(temps_df, mapping = aes(x = fahr, y = celsius, color = kelvin)) +
  geom_point() +
  custom_theme(50)









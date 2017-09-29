#without dataset
#mapply
x = 1:4
b = 4:1
mapply(rep,x,b)
similar_to_mapply = function(a,b){
  vec = c()
  for (i in 1:length(a)){
    vec[[i]] = c(rep(a[i],b[i]))
  }
  return(vec)
}
similar_to_mapply(x,b)
plot_mapply = microbenchmark(
  mapply(rep,x,b),
  similar_to_mapply(x,b)
)
plot(plot_mapply)
#tapply
x <- c(1, 2, 3, 10, 20, 30, 100, 200, 300)
groups <- c("a", "a", "a", "b", "b", "b", "c", "c", "c")
tapply(x, groups, mean,simplify = TRUE)
tapply(x,groups,mean,simplify = FALSE)
similar_to_tapply = function(x,groups) {
  fact = levels(factor(groups))
  vec = c()
  i = 1
  for (group in fact){
    vec[[i]] = (mean(x[groups == group]))
    i = i+1
  }
  return(vec)
}
similar_to_tapply(x,groups)
plot_tapply = microbenchmark(tapply(x, groups, mean),similar_to_tapply(x,groups))
plot(plot_tapply)
#rapply
x = list(1:5)
rapply(x,function(x){
  return (max(unlist(x)))
})
similar_to_raaply = function(x) {
  return(max(unlist(x)))
}
similar_to_raaply(x)
plot_rapply = microbenchmark(rapply(x,function(x){
  return (max(unlist(x)))
}),similar_to_raaply(x)
)
plot(plot_rapply)
#eapply
env <- new.env(hash = FALSE) 
env$a <- c(1,2,3,4,5,6,7,8,9,10)
env$beta <-  c(0.04978707,0.13533528,0.36787944,1.00000000,2.71828183,7.38905610,20.08553692)
env$logic <- c(TRUE, FALSE, FALSE, TRUE)
eapply(env, sum)
microbenchmark(eapply(env, sum))
Unit: microseconds
expr   min     lq    mean median    uq    max neval
eapply(env, sum) 2.848 3.2565 3.58781 3.3645 3.582 16.199   100

#Using dataset
#load dataset
library(readr)
weather_data_nyc_centralpark_2016_1_ <- read_csv("~/Downloads/weather_data_nyc_centralpark_2016(1).csv")
View(weather_data_nyc_centralpark_2016_1_)
#tapply
tapply(weather_data_nyc_centralpark_2016_1_$`maximum temperature`,weather_data_nyc_centralpark_2016_1_$`minimum temperature`,mean,simplify = FALSE)
tapply(weather_data_nyc_centralpark_2016_1_$`maximum temperature`,weather_data_nyc_centralpark_2016_1_$`minimum temperature`,mean,simplify = TRUE)
similar_to_tapply(weather_data_nyc_centralpark_2016_1_$`maximum temperature`,weather_data_nyc_centralpark_2016_1_$`minimum temperature`)
plot_tapply = microbenchmark(tapply(weather_data_nyc_centralpark_2016_1_$`maximum temperature`,weather_data_nyc_centralpark_2016_1_$`minimum temperature`,mean,simplify = TRUE),
                             similar_to_tapply(weather_data_nyc_centralpark_2016_1_$`maximum temperature`,weather_data_nyc_centralpark_2016_1_$`minimum temperature`))
plot(plot_tapply)
#rapply
y = list(weather_data_nyc_centralpark_2016_1_$`maximum temperature`)
rapply(y,function(y){
  return (max(unlist(y)))
})
similar_to_raaply = function(y) {
  return(max(unlist(y)))
}
similar_to_raaply(y)
plot_rapply = microbenchmark(rapply(y,function(y){
  return (max(unlist(y)))
}),
similar_to_raaply = function(y) {
  return(max(unlist(y)))
})
plot(plot_rapply)








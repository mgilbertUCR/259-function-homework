library(tidyverse)

limit_replace = function(dataA, lowerb = 1, upperb = 1) {
  stopifnot(is.numeric(dataA))
  stopifnot(is.numeric(lowerb)&is.numeric(upperb))
  #if(lowerb == 1) {lowerb = (mean(dataA)-sd(dataA))}
  lowerb = ifelse(lowerb == 1, (mean(dataA)-sd(dataA)), lowerb)
  #if(upperb == 1) {lowerb = (mean(dataA)+sd(dataA))}
  upperb = ifelse(upperb == 1, (mean(dataA)+sd(dataA)), upperb)
  #print(c(lowerb, upperb))
  
  ifelse(dataA < lowerb | dataA > upperb, NA, dataA)
}

plus_minus_sd = function(dataArg, num_of_SDs = 1){
  dmean = mean(dataArg)
  dsd = (sd(dataArg))*num_of_SDs
  c((paste0 ("Values within ",num_of_SDs," std.dev.:")),(dmean-dsd), dmean, (dmean+dsd))
}
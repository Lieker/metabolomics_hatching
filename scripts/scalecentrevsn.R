scalecentrevsn <- function(x){
  x_v <- justvsn(x)
  x_ftrcsv <- prep.autoscale(x_v, center = T, scale = T)
  return(x_ftrcsv)
}
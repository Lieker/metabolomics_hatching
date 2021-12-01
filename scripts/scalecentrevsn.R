scalecentrevsn <- function(x){
  x_ftrcs <- prep.autoscale(x, center = TRUE, scale = TRUE)
  x_ftrcsv <- justvsn(x_ftrcs)
  return(x_ftrcsv)
}
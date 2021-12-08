#x is a dataframe containing RF results, y is a dataframe containing correlation results. Both have a column called 'feature'.

overlap_RF_corr <- function(x, y){
  z <- left_join(x, y, by = "feature")
  names(z)[names(z) == "rtmed"] <- "Rt_r"
  names(z)[names(z) == "mzmed"] <- "m.z.meas."
  return(z)
}

overlap_RF_corr2 <- function(x, y, yis){
  if(yis == "p"){
  y$m <- round((y$mzmed - 2), digits = 1)
  x$m <- round(x$mz_r, digits = 1)
  y1 <- y[,names(y) %in% c("corr.coef","m")]
  names(y1) <- c("corr.coef2", "m")
  x <- left_join(x, y1, by = "m")
  x <- x %>% dplyr::select(-(contains("m.z.meas.")))
  return(x)
  } else if(yis == "n"){
    y$m <- round((y$m.z.meas. + 2), digits = 1)
    x$m <- round(as.numeric(x$mz_r), digits = 1)
    y1 <- y[,names(y) %in% c("corr.coef","m")]
    names(y1) <- c("corr.coef2", "m")
    x <- left_join(x, y1, by = "m")
    x <- x %>% dplyr::select(-(contains("mzmed")))
    return(x)
  }
}

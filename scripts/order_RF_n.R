order_RF_n1 <- function(x){
  names <- t(as.data.frame(str_split(x$name, "T")))
  x <- cbind(x, names)
  x$'1' <- gsub("M","",x$'1')
  x$'mz_r' <- round(as.numeric(x$'1'), digit = 2)
  return(x)
}

order_RF_n2 <- function(x, y, n){
  x$Rt_r <- round(as.numeric(x$'2'), digit = 2)
  z <- y %>% rownames_to_column("feature") %>% dplyr::select(feature, RT..min., m.z.meas., M.meas.) 
  z$m.z.meas. <- round(z$m.z.meas., digits = 2)
  z$RT..min. <- round(z$RT..min., digits = 2)
  names(z) <- c("feature","Rt_r", "mz_r", "M_r")
  a <- cbind(y, z)
  x <- left_join(x, a, by = c("Rt_r", "mz_r"))
  write.csv(x, n, row.names = FALSE)
  return(x)
}
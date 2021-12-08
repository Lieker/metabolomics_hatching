order_RF_p <- function(x, y){
  y2 <- y %>% rownames_to_column("name")
  y2$name <- paste0("FT",y2$name)
  x <- left_join(x, y2, by = "name")
  x$mz_r <- round(as.numeric(x$mzmed), digits = 2)
  x$feature <- as.character(as.numeric(gsub("FT","",x$name)))
  return(x)
}
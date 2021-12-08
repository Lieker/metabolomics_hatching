add_mz <- function(x, c){
  c1 <- c %>% rownames_to_column("feature")
  c1$feature <- as.character(as.numeric(c1$feature))
  x1 <- left_join(x, c1[,names(c1) %in% c("feature", "m.z.meas.", "mzmed")], by = "feature")
  names(x1) <- c(names(x), "mz")
  return(x1)
}

# compounds should be a dataframe with the columns "Rt", "m/z" and "Name"

ordermatrixforcorplot_fr <- function(x, compounds, hatching = hatch_s_subset$mean_hatch){
  x$hatch <- hatching
  c <- as.data.frame(cor(x))
  c <- c[order(-c$hatch),]
  
  #get only top and bottom 10
  c1 <- c[c(1:11, (length(c) - 9):(length(c))),]
  rn <- row.names(c1)
  c1 <- c1[, which(names(c1) %in% rn)] %>% as.matrix()
  
  #order the matrices
  c1 <- as.data.frame(c1[rn, rn])
  
  #change names to compoundnames
  rn <- as.data.frame(rn)
  names(rn) <- "rn"
  rownames(compounds) <- NULL
  compounds <- compounds %>% rownames_to_column("rn")
  c3 <- left_join(rn, compounds[,c("rn","Name","m.z.meas.", "RT..min.")], by = "rn")
  c3$mz <- substr(c3$m.z.meas., 0, 6)
  c3$Rt <- substr(c3$RT..min., 0, 5)
    
  while(length(ind <- which(c3$Name == "")) > 0){
    c3$Name[ind] <- paste0("m/z ",c3$m.z.meas.[ind]," Rt ",c3$RT..min.[ind])
  }
  c3$Name[c3$rn == "hatch"] <- "hatch"
  row.names(c1) <- c3$Name
  c1 <- t(c1)
  row.names(c1) <- c3$Name
  
  return(c1)
}
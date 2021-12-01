# compounds should be a dataframe with the columns "Rt", "m/z" and "Name"

ordermatrixforcorplot <- function(x, compounds, interest = "solA", y = "input/23gt_rootweightsolA.csv"){
  xp_design <- read.csv(y, header = TRUE, fileEncoding = "UTF-8-BOM")
  c0 <- as.data.frame(x)
  
  if(interest == "solA"){
    c0$solA <- xp_design$Concentration.per.g.FW[1:100] 
    c <- as.data.frame(cor(c0))
    c <- c[order(-c$solA),]
    } else if(interest == "hatching"){
    c0$hatching <- xp_design$Hatching[1:100]
    c <- as.data.frame(cor(c0))
    c <- c[order(-c$hatching),]
    }
  
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
    c3 <- left_join(rn, compounds[,c("rn","Name","mz", "Rt")], by = "rn")
    c3$mz <- substr(c3$mz, 0, 6)
    c3$Rt <- substr(c3$Rt, 0, 5)
    
    while(length(ind <- which(c3$Name == "")) > 0){
      c3$Name[ind] <- paste0("m/z ",c3$mz[ind]," Rt ",c3$Rt[ind])
    }
    c3$Name[c3$rn == interest] <- interest
    row.names(c1) <- c3$Name
    c1 <- t(c1)
    row.names(c1) <- c3$Name
  
    return(c1)
  
  
}
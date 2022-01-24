# compounds should be a dataframe with the columns "Rt", "m/z" and "Name"

ordermatrixforcorplot <- function(x, compounds, interest = "solA_UHPLC", y = "input/23gt_rootweightsolA.csv"){
  xp_design <- read.csv(y, header = TRUE, fileEncoding = "UTF-8-BOM")
  c0 <- as.data.frame(x)
  f <- rownames(x)
  f <- gsub("X","", f)
  xp_design <- xp_design[xp_design$Number %in% f,]
  if(interest == "solA_UHPLC"){
    c0$solA_UHPLC <- xp_design$Concentration.per.g.FW 
    c <- as.data.frame(cor(c0))
    c <- c[order(-c$solA_UHPLC),]
  } else if(interest == "hatching"){
    c0$hatching <- xp_design$Hatching
    c <- as.data.frame(cor(c0))
    c <- c[order(-c$hatching),]
  }
  #get only top and bottom 10
  c1 <- c[c(1:11),]
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

ordermatrixforcorplot2 <- function(x, compounds, interest = "solA_UHPLC", y = "input/23gt_rootweightsolA.csv"){
  xp_design <- read.csv(y, header = TRUE, fileEncoding = "UTF-8-BOM")
  c0 <- as.data.frame(x)
  f <- rownames(x)
  f <- gsub("X","", f)
  xp_design <- xp_design[xp_design$Number %in% f,]
  if(interest == "solA_UHPLC"){
    c0$solA_UHPLC <- xp_design$Concentration.per.g.FW 
    c <- as.data.frame(cor(c0))
    c <- c[order(-c$solA_UHPLC),]
  } else if(interest == "hatching"){
    c0$hatching <- xp_design$Hatching
    c <- as.data.frame(cor(c0))
    c <- c[order(-c$hatching),]
  }
  return(c0)
}

get_testRes <- function(x, compounds, interest = "solA_UHPLC", y = "input/23gt_rootweightsolA.csv", t = testRes){
  xp_design <- read.csv(y, header = TRUE, fileEncoding = "UTF-8-BOM")
  c0 <- as.data.frame(x)
  f <- rownames(x)
  f <- gsub("X","", f)
  xp_design <- xp_design[xp_design$Number %in% f,]
  if(interest == "solA_UHPLC"){
    c0$solA_UHPLC <- xp_design$Concentration.per.g.FW 
    c <- as.data.frame(cor(c0))
    c <- c[order(-c$solA_UHPLC),]
  } else if(interest == "hatching"){
    c0$hatching <- xp_design$Hatching
    c <- as.data.frame(cor(c0))
    c <- c[order(-c$hatching),]
  }
  
  
  #get only top and bottom 10
  c1 <- c[c(1:11),]
  rn <- row.names(c1)
  c1 <- c1[, which(names(c1) %in% rn)] %>% as.matrix()
  
  #order the matrices
  c1 <- as.data.frame(c1[rn, rn])
  
  #order also the testRes matrix
  testRes <- as.data.frame(t)
  testRes <- testRes[which(names(testRes) %in% names(c1)), which(names(testRes) %in% names(c1))]
  testRes <- testRes[rn, rn]
  
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
  row.names(testRes) <- c3$Name
  testRes <- t(testRes)
  row.names(testRes) <- c3$Name
  
  return(testRes)
}

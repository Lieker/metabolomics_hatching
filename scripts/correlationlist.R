correlationlist <- function(x, interest, y = "input/23gt_rootweightsolA.csv"){
  xp_design <- read.csv(y, header = TRUE, fileEncoding = "UTF-8-BOM")
  c0 <- as.data.frame(x)
  f <- rownames(x)
  f <- gsub("X","", f)
  xp_design <- xp_design[xp_design$Number %in% f,]
  if(interest == "solA"){
    c0$solA <- xp_design$Concentration.per.g.FW 
    c <- as.data.frame(cor(c0))
    c <- c[order(-c$solA),]
    c <- c[c$solA > 0.1,]
    c <- c[,row.names(c)]
    testRes <- cor.mtest(c, conf.level = 0.95)
    p <- as.data.frame(testRes$p)
    p <- p[, names(p) == "solA"]
    c$pvalue <- p
    c <- c[c$pvalue < 0.05, names(c) %in% c("pvalue", "solA")]
  } else if(interest == "hatching"){
    c0$hatching <- xp_design$Hatching
    c <- as.data.frame(cor(c0))
    c <- c[order(-c$hatching),]
    c <- c[c$hatching > 0.1,]
    c <- c[,row.names(c)]
    testRes <- cor.mtest(c, conf.level = 0.95)
    p <- as.data.frame(testRes$p)
    p <- p[, names(p) == "hatching"]
    c$pvalue <- p
    c <- c[c$pvalue < 0.05, names(c) %in% c("pvalue", "hatching")]
  }
  
  c <- c %>% rownames_to_column("feature")
  return(c)
}

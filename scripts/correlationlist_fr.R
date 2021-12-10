correlationlist_fr <- function(x, hatching = hatch_s_subset$mean_hatch, compounds){
  x$hatch <- hatching
  c0 <- as.data.frame(x)
  c <- as.data.frame(cor(c0))
  c <- c[order(-c$hatch),]
  c <- c[c$hatch > 0.5,]
  c <- c[,row.names(c)]
  testRes <- cor.mtest(c, conf.level = 0.95)
  p <- as.data.frame(testRes$p)
  p <- p[, names(p) == "hatch"]
  c$pvalue <- p
  c <- c[c$pvalue < 0.05, names(c) %in% c("pvalue", "hatch")]
  c <- c %>% rownames_to_column("feature")
  
  c1 <- compounds %>% rownames_to_column("feature")
  c1$feature <- as.character(as.numeric(c1$feature))
  x1 <- left_join(c, c1[,names(c1) %in% c("feature", "m.z.meas.", "mzmed")], by = "feature")
  return(x1)
}

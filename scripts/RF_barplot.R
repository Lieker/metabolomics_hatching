# if correlation coefficient needs to be after bar instead of inside, use hj = -0.2
RF_barplot <- function(x, hj = 0.6) {
  x <- x[!duplicated(x[ , c("rank")]),]
  x$mz <- paste0("m/z ",x$mz_r)
  x$rr <- as.numeric(1/x$rank)
  x$corr.coef <- round(x$corr.coef, digits = 2)
  x$corr.coef2 <- round(x$corr.coef2, digits = 2)
  x$f <- ifelse(is.na(x$corr.coef2), ifelse(is.na(x$corr.coef), 'a', 'b'), ifelse(is.na(x$corr.coef), 'c', 'd'))
  x$stars <- ifelse(is.na(x$corr.coef),
                    ifelse(is.na(x$corr.coef2),
                           "",
                           ifelse(x$pvalue2 < 0.001,
                                  "***",
                                  ifelse(x$pvalue2 < 0.01,
                                         " **",
                                         "  *"))),
                    ifelse(x$pvalue < 0.001,
                           "***",
                           ifelse(x$pvalue < 0.01,
                                  " **",
                                  "  *")))
  
    r1 <- ggplot(data = x, 
               aes(x = reorder(name, rr, FUN = sum), 
                   y = rr,
                   fill = f)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values=c("a" = "grey50", "b" = "firebrick", "c" = "dodgerblue", "d" = "green4")) +
    coord_flip() +
    theme_minimal() +
    labs(y = "feature importance", x = "") +
    geom_text(aes(label=ifelse(is.na(corr.coef), 
                               ifelse(is.na(corr.coef2), 
                                      "", 
                                      paste(round(corr.coef2, digits = 2), stars)), 
                               paste(corr.coef, stars)), 
              hjust = hj)) +
      scale_x_discrete(breaks=x$name, label=x$mz)
      
  r1
  return(r1)
}


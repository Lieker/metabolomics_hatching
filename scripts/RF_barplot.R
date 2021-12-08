# if correlation coefficient needs to be after bar instead of inside, use hj = -0.2
RF_barplot <- function(x, hj = 1.1) {
  x <- x[!duplicated(x[ , c("rank")]),]
  if("mz_r.x" %in% names(x)){
    x$mz <- paste0("m/z ",x$mz_r.x)
  } else if("mz_r" %in% names(x)){
    x$mz <- paste0("m/z ",x$mz_r)
  }
  x$rr <- as.numeric(1/x$rank)
  x$corr.coef <- round(x$corr.coef, digits = 2)
  x$f <- ifelse(is.na(x$corr.coef2), ifelse(is.na(x$corr.coef), 'a', 'b'), ifelse(is.na(x$corr.coef), 'c', 'd'))
  
  r1 <- ggplot(data = x, 
               aes(x = reorder(mz, rr, FUN = sum), 
                   y = rr,
                   fill = f)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values=c("a" = "grey50", "b" = "firebrick", "c" = "dodgerblue", "d" = "green4")) +
    coord_flip() +
    theme_minimal() +
    labs(y = "feature importance", x = "") +
    geom_text(aes(label=ifelse(is.na(corr.coef), ifelse(is.na(corr.coef2), "", round(corr.coef2, digits = 2)), corr.coef)), hjust = hj)
  r1
  return(r1)
}


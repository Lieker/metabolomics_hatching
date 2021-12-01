correctforrootweight <- function(x, y = "input/23gt_rootweightsolA.csv"){
  x_t <- t(x)
  xp_design <- read.csv(y, header = TRUE, fileEncoding = "UTF-8-BOM")
  rootw <- as.vector(xp_design$root.fresh.weight..g.)
  x_tr <- t(t(x_t)) / rootw[1:100]
  return(x_tr)
}

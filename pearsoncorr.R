
library(tidyverse)
library(corrplot)

dat <- read.csv("trimed_23 genotype_pos.csv", header = T)
dat <- dat %>% column_to_rownames("X")
dattrim <- dat[,-c(1:7)] %>% t()

r <- round(cor(dattrim), digits = 2)
rtrim <- r[1:25, 1:25]
res1 <- cor.mtest(rtrim, conf.level = .95)
corrplot.mixed(rtrim,
               lower.col = "black",
               tl.cex = 0.5,
               tl.pos = "lt",
               number.cex = .5)

rs <- as.data.frame(r[,"FT00000"])
names(rs) <- "FT00000"
rs <- rs %>% filter(FT00000 >= 0.3 | FT00000 <= -0.3)
row.names(rs) -> "ms"


dattrim <- as.data.frame(t(dattrim)) %>% rownames_to_column("metabolites")
dattrim_ms <- dattrim[dattrim$metabolites %in% ms,] 
row.names(dattrim_ms) <- NULL
dattrim_ms <- dattrim_ms %>% column_to_rownames("metabolites")



rm <- round(cor(t(dattrim_ms)), digits = 2)
res2 <- cor.mtest(rm, conf.level = .95)
corrplot.mixed(rm,
         order = "hclust",
         tl.pos = "d",
         tl.cex = 0.5,
         number.cex = 0.7)
pvalues <- res2$p

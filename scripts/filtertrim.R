# input x contains 105 columns: 100 sample columns and 5 blanks OR contains only 100 sample columns
filtertrim <- function(x, ep_factor = 1){
  if(ncol(x) > 100){
    #make ep dataframe
    ep <- x[,101:105]
    ep$mean <- rowMeans(ep)
    ep$mean1 <- ep$mean * ep_factor
    epv <- as.vector(ep$mean1)
    
    #make dataframe showing average of replicates per genotype
    pergenotype <- function(y){
      y <- y %>% t() %>% as.data.frame()
      y <- y %>% rownames_to_column("sample")
      y$genotype <- substr(y$sample, 1, 3)
      y <- y[1:100,]
      y <- y %>% column_to_rownames("sample") %>% as.data.frame()
      x1 <- y %>% group_by(genotype) %>% summarise(across(everything(), mean))
      x1 <- x1 %>% column_to_rownames("genotype") %>% t() %>% as.data.frame()
      return(x1)
    }
    ensum <- pergenotype(y = x)
    ensum[ensum < epv] <- 0
    ensum <- ensum[as.logical(rowSums(ensum) != 0),]
    
    #remove the trimmed compounds from ensum in the main df
    nc <- row.names(ensum)
    nc <- gsub("V","",nc)
    x <- x %>% rownames_to_column("compound")
    x <- x[x$compound %in% nc,]
    x <- x %>% subset(., select = -c(compound))
    x <- x[,1:100]
  }
  
  #remove if feature does not occur in 3 or more samples per genotype
  xp_design <- read.csv("input/23gt_rootweightsolA.csv", header = TRUE, fileEncoding = "UTF-8-BOM")
  xp_design <- xp_design[xp_design$Number %in% gsub("X","",names(x)),]
  x_binary <- apply(x, c(1,2), function(z) {ifelse(any(z > 0), 1, 0)}) %>% t() %>% as.data.frame()
  x_binary$genotype <- xp_design$Genotype.name
  x_binary2 <- x_binary %>% group_by(genotype) %>% summarise(across(everything(), sum)) %>% column_to_rownames("genotype") %>% t()
  keep <- x_binary2[apply(x_binary2, 1, function(x) any(x > 2)),]
  x_f <- x[row.names(x) %in% gsub("V","",row.names(keep)),]
  
  #filter compounds that do not reach above 200 in any sample (column)
  x_f2 <- x_f[!apply(x_f<200,1,all),]
  return(x_f2)
}
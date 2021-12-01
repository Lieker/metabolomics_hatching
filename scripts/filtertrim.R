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
    x_f <- x %>% rownames_to_column("compound")
    x_f <- x_f[x_f$compound %in% nc,]
    x_f <- x_f %>% subset(., select = -c(compound))
    
    #remove if compound is present in less than 4 samples
    x_binary <- x_f
    x_binary <- apply(x_binary, c(1,2), function(z) {ifelse(any(z > 0), 1, 0)})
    keep <- as.data.frame(x_binary[rowSums(x_binary) >3,])
    x_f2 <- x_f[row.names(x_f) %in% row.names(keep),1:100]
  }
  
  #filter compounds that do not reach above 200 in any sample (column)
  x_f3 <- x_f2[!apply(x_f2<200,1,all),]
  return(x_f3)
}
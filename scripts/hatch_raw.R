hatch_raw <- function(input = "input/hatching_raw.csv") {
  raw <- read.csv(input, sep=',', header = TRUE)
  names(raw)[1] <- "well"
  
  #only take data points at t=0 for normalization
  raw0 <- filter(raw, t == 0) 
  
  #combine the t=0 timepoints with all other timepoints
  raw <- left_join(raw, raw0, by = c("well", "treatment"), copy = FALSE, suffix=c("", "_t0")) %>% dplyr::filter(t!=0) 
  raw_h <- subset(raw, 
                select = -c(eggs,
                            t_t0))
  raw_h <- raw_h %>% drop_na()
  #remove unnecessary lines
  return(raw_h)
}

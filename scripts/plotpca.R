plotpca <- function(x, y = "input/23gt_rootweightsolA.csv"){
  
  c25 <- c(
    "dodgerblue2", "#E31A1C", # red
    "green4",
    "#6A3D9A", # purple
    "#FF7F00", # orange
    "black", "gold1",
    "skyblue2", "#FB9A99", # lt pink
    "palegreen2",
    "#CAB2D6", # lt purple
    "#FDBF6F", # lt orange
    "gray70", "khaki2",
    "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
    "darkturquoise", "green1", "yellow4", "yellow3",
    "darkorange4", "brown"
  )
  
  xp_design <- read.csv(y, header = TRUE, fileEncoding = "UTF-8-BOM")
  
  y <- prcomp(x, scale. = F)
  x2 <- as.data.frame(cbind(x, xp_design$Genotype.name[1:100]))
  names(x2)[length(names(x2))]  <- "Genotype.name"
  x2$Genotype.name <- as.factor(x2$Genotype.name)
  
  p <- autoplot(y, 
           data = x2, 
           colour = 'Genotype.name', 
           label = TRUE, 
           shape = F, 
           label.size = 3) + 
    theme_minimal() + 
    scale_color_manual(values = c25) + 
    scale_size(guide = "none")
  
  return(p)
}
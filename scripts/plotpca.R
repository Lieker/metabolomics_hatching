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
           label = F, 
           shape = 'Genotype.name',
           size = 2) + 
    theme_minimal() + 
    scale_color_manual(values = c25) +
    scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
    scale_size(guide = "none")
  
  return(p)
}

plotpca2 <- function(x){
  
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
  
  samples <- as.data.frame(row.names(x))
  samples2 <- str_split_fixed(samples$`row.names(x)`, "_", 2)
  samples <- cbind(samples, samples2)
  names(samples) <- c("sample","genotype","fraction")
  samples$fraction <- factor(samples$fraction, levels = c("F8","F9","F10","F11"))
  y <- prcomp(x, scale. = F)
  
  p <- autoplot(y, 
                data = samples, 
                colour = 'genotype', 
                label = F, 
                shape = 'fraction',
                size = 2) + 
    theme_minimal() + 
    scale_color_manual(values = c25) +
    scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
  scale_size(guide = "none")
  
  return(p)
}

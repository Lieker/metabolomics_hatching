plotcor <- function(x, t = testRes){
  corrplot(x, 
             order = "hclust", 
             diag = F,
             addrect = 2, 
             method = "circle",
             type = "lower",
             number.cex = 0.8,
             cl.pos = 'b', 
             p.mat = t, 
             sig.level = c(0.001, 0.01, 0.05),
             insig = "label_sig",
             pch.cex = 1,
             tl.srt = 45,
             tl.col = "black",
             tl.cex = 0.8,
             col = brewer.pal(n = 10, name = 'RdYlGn'))
  recordPlot()
}

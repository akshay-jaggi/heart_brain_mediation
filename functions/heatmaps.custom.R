library(gplots)
library(ggplot2)
library(reshape2)
library(scales)

heatmap.with.labels = function(cor.data, min, max, label) {
  ggplot(melt(cor.data), 
         #aes(Var2,ordered(Var1,levels=rev(levels(Var1))),fill=value)) + 
         aes(Var2,Var1,fill=value)) +
    scale_fill_gradient2(low=muted("red"),mid="white",high=muted("blue"),
                         midpoint=0,
                         limits=c(min, max)) +
    geom_tile() + 
    theme_classic() +
    labs(fill = label) +
    guides(fill = guide_colourbar(barwidth = 0.5, barheight = 15))+
    coord_fixed() +
    theme(
      axis.line=element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}

heatmap.with.tree = function(data, test.names, test.palette) { 
  my_palette = colorRampPalette(c("darkred", "white", "darkblue"))(n = 299)
  heatmap.2(data,
            RowSideColors = test.palette,
            key = TRUE,
            key.title = NULL,
            key.xlab = "correlation",
            labRow = FALSE,
            labCol = FALSE,
            density.info = "none",
            trace="none",
            col=my_palette,
            dendrogram = "row",
            na.rm = TRUE,
            margins = c(0,15))
  legend("topright", legend = levels(test.names), col = unique(test.palette),
         lty= 1, lwd = 5, cex=.7)
}
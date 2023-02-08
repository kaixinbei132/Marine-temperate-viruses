library(pheatmap)
save_pheatmap_pdf <- function(x, filename, width=7, height=7) {
  stopifnot(!missing(x))
  stopifnot(!missing(filename))
  pdf(filename, width=width, height=height)
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  dev.off()
}


##### read in data #####
dAMG_sample <- read.table("4figure_heatmap_sample_AMG.txt",header = T,sep = "\t",row.names = 1)
dAMG_taxa <- read.table("4figure_heatmap_Superkindom_AMG.txt",header = T,sep = "\t",row.names = 1)


##### AMG heatmap #####
p1 <- pheatmap(dAMG_sample,
               #pheatmap(dAMG_sample,
               cluster_cols = F,
               cluster_rows = F,
               show_rownames = T,
               breaks = seq(0,1,0.02),
               colorRampPalette(colors= c("#336699","#FFFF99","#FFCC33","#FF8C00","red","#990000"))(50))
save_pheatmap_pdf(p1, "heatmap_AMG_sample.pdf")
p2 <- pheatmap(dAMG_taxa,
               #pheatmap(dAMG_taxa,
               cluster_cols = F,
               cluster_rows = F,
               show_rownames = F,
               breaks = seq(0,1,0.02),
               colorRampPalette(colors= c("#336699","#FFFF99","#FFCC33","#FF8C00","red","#990000"))(50))
save_pheatmap_pdf(p2, "heatmap_AMG_taxa.pdf")


dAMG_sample <- read.table("4figure_heatmap_sample_AMG_total.txt",header = T,sep = "\t",row.names = 1)
dAMG_taxa <- read.table("4figure_heatmap_Superkindom_AMG_total.txt",header = T,sep = "\t",row.names = 1)
p1 <- pheatmap(dAMG_sample,
               #pheatmap(dAMG_sample,
               cluster_cols = F,
               cluster_rows = F,
               show_rownames = T,
               breaks = seq(0,1,0.02),
               colorRampPalette(colors= c("#336699","#FFFF99","#FFCC33","#FF8C00","red","#990000"))(50))
save_pheatmap_pdf(p1, "heatmap_AMG_sample_total.pdf")
p2 <- pheatmap(dAMG_taxa,
               #pheatmap(dAMG_taxa,
               cluster_cols = F,
               cluster_rows = F,
               show_rownames = F,
               breaks = seq(0,1,0.02),
               colorRampPalette(colors= c("#336699","#FFFF99","#FFCC33","#FF8C00","red","#990000"))(50))
save_pheatmap_pdf(p2, "heatmap_AMG_taxa_total.pdf")

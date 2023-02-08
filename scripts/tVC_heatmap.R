library(pheatmap)
save_pheatmap_pdf <- function(x, filename, width=7, height=7) {
  stopifnot(!missing(x))
  stopifnot(!missing(filename))
  pdf(filename, width=width, height=height)
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  dev.off()
}

##### prophage cluster seawater pheatmap #####
dphcluster_sample <- read.table("4figure_heatmap_cluster_sample_type_Seawater.txt",header = T,sep = "\t",row.names = 1)
p2_3 <- pheatmap(dphcluster_sample,
                 cluster_cols = F,treeheight_row = 10,
                 cellwidth = 20, cellheight = 15,
                 fontsize = 10,angle_col="45",
                 colorRampPalette(colors= c("#0F425CFF","#FFFF99","#FFCC66","#FF9933","#CC0C00FF","#800000FF"))(50))
#         annotation_row = anno_row,annotation_colors = anno_colos)
save_pheatmap_pdf(p2_3,"prophage_cluster_seawater_pheatmap.pdf",3,10)

##### prophage cluster seawater pheatmap #####
dphcluster_sample <- read.table("4figure_heatmap_cluster_sample_type_Sediment.txt",header = T,sep = "\t",row.names = 1)
p2_4 <- pheatmap(dphcluster_sample,
                 cluster_cols = F,treeheight_row = 10,
                 cellwidth = 20, cellheight = 15,
                 fontsize = 10,angle_col="45",
                 colorRampPalette(colors= c("#0F425CFF","#FFFF99","#FFCC66","#FF9933","#CC0C00FF","#800000FF"))(50))
#         annotation_row = anno_row,annotation_colors = anno_colos)
save_pheatmap_pdf(p2_4,"prophage_cluster_sediment_pheatmap.pdf",3,10)


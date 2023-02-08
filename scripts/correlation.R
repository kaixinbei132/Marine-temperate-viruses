library(ggplot2)
library(Rmisc)

dcov_pearsonr_sample <- read.table("4figure_prophage_cov_genomic_features_sample_genus_pearsonr.txt",
                                   header = T,sep = "\t")
dcov_pearsonr_sample$sample <- factor(dcov_pearsonr_sample$sample,
                                      levels = c("Epipelagic-Seawater","Mesopelagic-Seawater","Bathypelagic-Seawater",
                                                 "Epipelagic-Sediment","Mesopelagic-Sediment","Bathypelagic-Sediment"))
dcov_pearsonr_sample$data <- 
  factor(dcov_pearsonr_sample$data,
         levels = c('host_NARSC','host_CARSC','host_GC','host_protein_density','host_len',   
                    'vh_codon_distance','vh_d2star'))
dcov_pearsonr_sample_h <- subset(dcov_pearsonr_sample,data_type=="host")
dcov_pearsonr_sample_v <- subset(dcov_pearsonr_sample,data_type=="virus")
dcov_pearsonr_sample_vh <- subset(dcov_pearsonr_sample,data_type=="vh")

dcov_pearsonr_Class <- read.table("4figure_prophage_cov_genomic_features_Class_genus_pearsonr.txt",
                                  header = T,sep = "\t")
dcov_pearsonr_Class$taxa <- 
  factor(dcov_pearsonr_Class$taxa,
         levels = c("Bacteria","Archaea",
                    "Gammaproteobacteria","Alphaproteobacteria","Bacteroidia",
                    "Cyanobacteriia","Actinomycetia","Bacilli",
                    "Poseidoniia","Nitrososphaeria","Bathyarchaeia","Lokiarchaeia"))
dcov_pearsonr_Class$data <- 
  factor(dcov_pearsonr_Class$data,
         levels = c('host_NARSC','host_CARSC','host_GC','host_protein_density','host_len', 
                    'vh_codon_distance','vh_d2star'))
dcov_pearsonr_Class_h <- subset(dcov_pearsonr_Class,data_type=="host")
dcov_pearsonr_Class_vh <- subset(dcov_pearsonr_Class,data_type=="vh")


### host related pearsonr ###
p1 <- ggplot() +
  #    ggplot() + 
  geom_point(data=dcov_pearsonr_sample_h,aes(x=sample,y=data,fill=pearsonr),size=9,shape=22) +
  geom_point(data=dcov_pearsonr_sample_h,aes(x=sample,y=data,alpha=sig,shape=sig,size=sig),color="white") +
  scale_shape_manual(values = c("*","*","*",".")) +
  scale_fill_gradientn(colours = colorRampPalette(colors= c("#000066","#3333FF","#6699FF","white","#FF9933","#CC0C00FF","#800000FF"))(50),
                       limits=c(-1,1)) + 
  scale_alpha_manual(values = c(1,1,1,0)) +
  scale_size_manual(values = c(6,8,11,1)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),legend.position = "none",
        #axis.text.x = element_text(angle = 45,hjust=1,vjust=1),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

p2 <- ggplot() + 
  #ggplot() + 
  geom_point(data=dcov_pearsonr_Class_h,aes(x=taxa,y=data,fill=pearsonr),size=9,shape=22) +
  geom_point(data=dcov_pearsonr_Class_h,aes(x=taxa,y=data,alpha=sig,shape=sig,size=sig),color="white") +
  scale_shape_manual(values = c("*","*","*",".")) +
  scale_fill_gradientn(colours = colorRampPalette(colors= c("#000066","#3333FF","#6699FF","white","#FF9933","#CC0C00FF","#800000FF"))(50),
                       limits=c(-1,1)) + 
  scale_alpha_manual(values = c(1,1,1,0)) +
  scale_size_manual(values = c(6,8,11,1)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),legend.position = "none",
        #axis.text.x = element_text(angle = 45,hjust=1,vjust=1),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())


p3 <- ggplot() +
  #ggplot() + 
  geom_point(data=dcov_pearsonr_sample_vh,aes(x=sample,y=data,fill=pearsonr),size=7,shape=22) +
  geom_point(data=dcov_pearsonr_sample_vh,aes(x=sample,y=data,alpha=sig,shape=sig,size=sig),color="white") +
  scale_shape_manual(values = c("*","*","*",".")) +
  #scale_shape_manual(values = c("*","*","*",".")) +
  scale_fill_gradientn(colours = colorRampPalette(colors= c("#000066","#3333FF","#6699FF","white","#FF9933","#CC0C00FF","#800000FF"))(50),
                       limits=c(-1,1)) + 
  #scale_alpha_manual(values = c(1,0)) +
  scale_alpha_manual(values = c(1,1,1,0)) +
  scale_size_manual(values = c(6,8,11,1)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),legend.position = "none",
        #axis.text.x = element_text(angle = 45,hjust=1,vjust=1),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

p4 <- ggplot() +
  #ggplot() + 
  geom_point(data=dcov_pearsonr_Class_vh,aes(x=taxa,y=data,fill=pearsonr),size=7,shape=22) +
  geom_point(data=dcov_pearsonr_Class_vh,aes(x=taxa,y=data,alpha=sig,shape=sig,size=sig),color="white") +
  scale_shape_manual(values = c("*","*",'*',".")) +
  scale_fill_gradientn(colours = colorRampPalette(colors= c("#000066","#3333FF","#6699FF","white","#FF9933","#CC0C00FF","#800000FF"))(50),
                       limits=c(-1,1)) + 
  scale_alpha_manual(values = c(1,1,1,0)) +
  scale_size_manual(values = c(6,8,11,1)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = "none",
        #axis.text.x = element_text(angle = 45,hjust=1,vjust=1),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

pdf("merged_correlation.pdf",width = 6.5,height = 6.5)
p_total <- list(p1,p2,
                p3,p4,
                p5,p6)
lay <- rbind(c(1,2,2),
             c(1,2,2),
             c(3,4,4),
             c(3,4,4))
multiplot(plotlist = p_total[1:4], layout = lay)
dev.off()

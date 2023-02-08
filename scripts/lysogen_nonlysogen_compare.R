library(ggplot2)
library(Rmisc)

##### read in data #####
dph_Class_features <- read.table("4figure_with_without_prophage_host_genomic_features_Class_boxplot.txt",header = T)
dph_Class_features$Class <- 
  factor(dph_Class_features$Class,
         levels = c("Bacteria","Archaea",
                    "Gammaproteobacteria","Alphaproteobacteria","Bacteroidia",
                    "Cyanobacteriia","Actinomycetia","Bacilli",
                    "Poseidoniia","Nitrososphaeria",
                    "Bathyarchaeia","Lokiarchaeia"))
dph_Class_features_total <- subset(dph_Class_features,Phylum=="total")
dph_Class_features_split <- subset(dph_Class_features,Total=="total")


dph_sample_features <- read.table("4figure_with_without_prophage_host_genomic_features_sample_type_boxplot.txt",sep = "\t",header = T)
dph_sample_features$Sample_type <- 
  factor(dph_sample_features$Sample_type,
         levels=c("Epipelagic-Seawater","Mesopelagic-Seawater","Bathypelagic-Seawater",
                  "Epipelagic-Sediment","Mesopelagic-Sediment","Bathypelagic-Sediment"))
dph_seawater_features <- subset(dph_sample_features,sample=="Seawater")
dph_sediment_features <- subset(dph_sample_features,sample=="Sediment")


dph_Class_troph <- read.table("4figure_with_without_prophage_host_troph_type_Class_bar.txt",header = T)
dph_Class_troph$troph_type <- 
  factor(dph_Class_troph$troph_type,
         levels = c("others","copitroph"))
dph_Class_troph$Merged_type <- 
  factor(dph_Class_troph$Merged_type,
         levels = c("Bacteria__With_prophage","Bacteria__Without_prophage",
                    "Archaea__With_prophage","Archaea__Without_prophage",
                    "Gammaproteobacteria__With_prophage","Gammaproteobacteria__Without_prophage",
                    "Alphaproteobacteria__With_prophage","Alphaproteobacteria__Without_prophage",
                    "Bacteroidia__With_prophage","Bacteroidia__Without_prophage",
                    "Cyanobacteriia__With_prophage","Cyanobacteriia__Without_prophage",
                    "Actinomycetia__With_prophage","Actinomycetia__Without_prophage",
                    "Bacilli__With_prophage","Bacilli__Without_prophage",
                    "Poseidoniia__With_prophage","Poseidoniia__Without_prophage",
                    "Nitrososphaeria__With_prophage","Nitrososphaeria__Without_prophage",
                    "Bathyarchaeia__With_prophage","Bathyarchaeia__Without_prophage",
                    "Lokiarchaeia__With_prophage","Lokiarchaeia__Without_prophage"))
dph_Class_troph_total <- subset(dph_Class_troph,Phylum=="total")
dph_Class_troph_split <- subset(dph_Class_troph,Total=="total")


dph_sample_troph <- read.table("4figure_with_without_prophage_host_troph_type_sample_type_coverage.txt",header = T)
dph_sample_troph$Info <- 
  factor(dph_sample_troph$Info,
         levels = c("others","copitroph"))
dph_sample_troph$Merged_type <- 
  factor(dph_sample_troph$Merged_type,
         levels = c("Epipelagic-Seawater_With_prophage","Epipelagic-Seawater_Without_prophage",
                    "Mesopelagic-Seawater_With_prophage","Mesopelagic-Seawater_Without_prophage",
                    "Bathypelagic-Seawater_With_prophage","Bathypelagic-Seawater_Without_prophage",
                    "Epipelagic-Sediment_With_prophage","Epipelagic-Sediment_Without_prophage",
                    "Mesopelagic-Sediment_With_prophage","Mesopelagic-Sediment_Without_prophage",
                    "Bathypelagic-Sediment_With_prophage","Bathypelagic-Sediment_Without_prophage"))
dph_seawater_troph <- subset(dph_sample_troph,sample=="Seawater")
dph_sediment_troph <- subset(dph_sample_troph,sample=="Sediment")


##### carsc #####
p5_1 <- ggplot(dph_seawater_features) + 
  #ggplot(dph_seawater_features) +
  geom_boxplot(aes(x=Sample_type,y=host_CARSC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_CARSC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(2.64,3.35)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_2 <- ggplot(dph_sediment_features) + 
  #ggplot(dph_sediment_features) +
  geom_boxplot(aes(x=Sample_type,y=host_CARSC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_CARSC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(2.64,3.35)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_3 <- ggplot(dph_Class_features_total) + 
  #  ggplot(dph_Class_features_total) +
  geom_boxplot(aes(x=Class,y=host_CARSC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_CARSC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(2.64,3.35)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        #axis.text.y = element_blank(),
        axis.text.y = element_text(size=25),
        axis.title = element_blank())


p5_4 <- ggplot(dph_Class_features_split) + 
  #  ggplot(dph_Class_features_split) +
  geom_boxplot(aes(x=Class,y=host_CARSC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_CARSC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(2.64,3.35)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


##### narsc #####
p5_5 <- ggplot(dph_seawater_features) + 
  #ggplot(dph_seawater_features) +
  geom_boxplot(aes(x=Sample_type,y=host_NARSC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_NARSC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.28,0.45)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_6 <- ggplot(dph_sediment_features) + 
  #ggplot(dph_sediment_features) +
  geom_boxplot(aes(x=Sample_type,y=host_NARSC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_NARSC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.28,0.45)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_7 <- ggplot(dph_Class_features_total) + 
  #ggplot(dph_Class_features_total) +
  geom_boxplot(aes(x=Class,y=host_NARSC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_NARSC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.28,0.45)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        #axis.text.y = element_blank(),
        axis.text.y = element_text(size=17),
        axis.title = element_blank())


p5_8 <- ggplot(dph_Class_features_split) + 
  #ggplot(dph_Class_features_split) +
  geom_boxplot(aes(x=Class,y=host_NARSC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_NARSC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.28,0.45)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


##### troph type #####
p5_9 <- ggplot(dph_seawater_troph,
                #ggplot(dph_seawater_troph,
                aes(x=Merged_type,y=Percentage,fill=Info)) +
  geom_bar(stat="identity",width = 0.5) + 
  scale_fill_manual(values=c("#D3D3D3","#00A087FF")) +
  scale_y_continuous(expand = c(0,0)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.x = element_text(size=5,angle = 45),
        axis.text.y = element_blank())


p5_10 <- ggplot(dph_sediment_troph,
#                ggplot(dph_sediment_troph,
                aes(x=Merged_type,y=Percentage,fill=Info)) +
  geom_bar(stat="identity",width = 0.5) + 
  scale_fill_manual(values=c("#D3D3D3","#00A087FF")) +
  scale_y_continuous(expand = c(0,0)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.x = element_text(size=5,angle = 45),
        axis.text.y = element_blank())


p5_11 <- ggplot(dph_Class_troph_total,
#               ggplot(dph_Class_troph_total,
               aes(x=Merged_type,y=perc,fill=troph_type)) +
  geom_bar(stat="identity",width = 0.5) + 
  scale_fill_manual(values=c("#D3D3D3","#00A087FF")) +
  scale_y_continuous(expand = c(0,0)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.x = element_blank(),
        #axis.text.x = element_text(size=5,angle = 45),
        axis.text.y = element_text(size=17),
        #axis.text.y = element_blank(),
        axis.title = element_blank())


p5_12 <- ggplot(dph_Class_troph_split,
#ggplot(dph_Class_troph_split,
       aes(x=Merged_type,y=perc,fill=troph_type)) +
  geom_bar(stat="identity",width = 0.5) + 
  scale_fill_manual(values=c("#D3D3D3","#00A087FF")) +
  scale_y_continuous(expand = c(0,0)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.x = element_text(size=5,angle = 45),
        axis.text.y = element_blank())


pdf("merged_boxplot1.pdf",width = 21,height = 10)
p_total <- list(p5_1,p5_2,p5_3,p5_4,
                p5_5,p5_6,p5_7,p5_8,
                p5_9,p5_10,p5_11,p5_12)
lay <- rbind(c(1,2,3,4,4,4),
             c(5,6,7,8,8,8),
             c(9,10,11,12,12,12))
multiplot(plotlist = p_total[1:12], layout = lay)
dev.off()


##### genome size #####
p5_1 <- ggplot(dph_seawater_features) + 
  #ggplot(dph_seawater_features) +
  geom_boxplot(aes(x=Sample_type,y=host_nucl,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_nucl,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.5e+06,12e+06)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_2 <- ggplot(dph_sediment_features) + 
  #ggplot(dph_sediment_features) +
  geom_boxplot(aes(x=Sample_type,y=host_nucl,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_nucl,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.5e+06,12e+06)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_3 <- ggplot(dph_Class_features_total) + 
  #  ggplot(dph_Class_features_total) +
  geom_boxplot(aes(x=Class,y=host_nucl,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_nucl,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.5e+06,12e+06)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        #axis.text.y = element_blank(),
        axis.text.y = element_text(size=6.44),
        axis.title = element_blank())


p5_4 <- ggplot(dph_Class_features_split) + 
  #    ggplot(dph_Class_features_split) +
  geom_boxplot(aes(x=Class,y=host_nucl,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_nucl,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.5e+06,12e+06)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


##### GC #####
p5_5 <- ggplot(dph_seawater_features) + 
  #ggplot(dph_seawater_features) +
  geom_boxplot(aes(x=Sample_type,y=host_GC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_GC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.22,0.81)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_6 <- ggplot(dph_sediment_features) + 
  #ggplot(dph_sediment_features) +
  geom_boxplot(aes(x=Sample_type,y=host_GC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_GC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.22,0.81)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_7 <- ggplot(dph_Class_features_total) + 
  #ggplot(dph_Class_features_total) +
  geom_boxplot(aes(x=Class,y=host_GC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_GC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.22,0.81)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        #axis.text.y = element_blank(),
        axis.text.y = element_text(size=17),
        axis.title = element_blank())


p5_8 <- ggplot(dph_Class_features_split) + 
  #ggplot(dph_Class_features_split) +
  geom_boxplot(aes(x=Class,y=host_GC,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_GC,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.22,0.81)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


##### protein denstiy #####
p5_9 <- ggplot(dph_seawater_features) + 
  #ggplot(dph_seawater_features) +
  geom_boxplot(aes(x=Sample_type,y=host_protein_density,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_protein_density,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.7,1.5)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_10 <- ggplot(dph_sediment_features) + 
  #ggplot(dph_sediment_features) +
  geom_boxplot(aes(x=Sample_type,y=host_protein_density,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_protein_density,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.7,1.5)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_11 <- ggplot(dph_Class_features_total) + 
  #ggplot(dph_Class_features_total) +
  geom_boxplot(aes(x=Class,y=host_protein_density,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_protein_density,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.7,1.5)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        #axis.text.y = element_blank(),
        axis.text.y = element_text(size=17),
        axis.title = element_blank())


p5_12 <- ggplot(dph_Class_features_split) + 
  #ggplot(dph_Class_features_split) +
  geom_boxplot(aes(x=Class,y=host_protein_density,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_protein_density,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.7,1.5)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


##### growth (<5 h) #####
p5_13 <- ggplot(dph_seawater_features) + 
  #ggplot(dph_seawater_features) +
  geom_boxplot(aes(x=Sample_type,y=host_growth,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               na.rm = T,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_growth,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             na.rm = T,
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0,5.5)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_14 <- ggplot(dph_sediment_features) + 
  #ggplot(dph_sediment_features) +
  geom_boxplot(aes(x=Sample_type,y=host_growth,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               na.rm = T,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Sample_type,y=host_growth,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             na.rm = T,
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0,5.5)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


p5_15 <- ggplot(dph_Class_features_total) + 
  #ggplot(dph_Class_features_total) +
  geom_boxplot(aes(x=Class,y=host_growth,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               na.rm = T,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_growth,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             na.rm = T,
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0,5.5)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        #axis.text.y = element_blank(),
        axis.text.y = element_text(size=43),
        axis.title = element_blank())


p5_16 <- ggplot(dph_Class_features_split) + 
  #ggplot(dph_Class_features_split) +
  geom_boxplot(aes(x=Class,y=host_growth,fill=Host_group),
               outlier.shape = NA,alpha=0.85,width=0.6,
               na.rm = T,
               position = position_dodge(width=0.85)) + 
  geom_point(aes(x=Class,y=host_growth,fill=Host_group),
             position = position_jitterdodge(jitter.width = 0.05,dodge.width = 0.85),
             na.rm = T,
             size=0.06,alpha=0.3,color="#696969") + 
  scale_y_continuous(expand = c(0,0),limits = c(0,5.5)) + 
  scale_fill_manual(values=c("#00A087FF","#D3D3D3")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())


pdf("merged_boxplot2.pdf",width = 22,height = 15)
p_total <- list(p5_1,p5_2,p5_3,p5_4,
                p5_5,p5_6,p5_7,p5_8,
                p5_9,p5_10,p5_11,p5_12,
                p5_13,p5_14,p5_15,p5_16)
lay <- rbind(c(1,2,3,4,4,4),
             c(5,6,7,8,8,8),
             c(9,10,11,12,12,12),
             c(13,14,15,16,16,16))
multiplot(plotlist = p_total[1:16], layout = lay)
dev.off()
library(ggplot2)
library(Rmisc)

##### prophage host distance #####
### read in data ###
dph_sample_features <- read.table("4figure_prophage_host_distance_sample_type_boxplot.txt",header = T)
dph_sample_features$sample <-
  factor(dph_sample_features$sample,
         levels = c("Epipelagic-Seawater","Mesopelagic-Seawater","Bathypelagic-Seawater",
                    "Epipelagic-Sediment","Mesopelagic-Sediment","Bathypelagic-Sediment"))
dph_sample_codon_distance <- subset(dph_sample_features,distance=="codon_distance")
dph_sample_d2star <- subset(dph_sample_features,distance=="d2star")

dph_Class_features <- read.table("4figure_prophage_host_distance_Class_boxplot.txt",header = T)
dph_Class_features$Class <- 
  factor(dph_Class_features$Class,
         levels = c("Gammaproteobacteria","Alphaproteobacteria","Bacteroidia",
                    "Cyanobacteriia","Acidimicrobiia","Actinomycetia",
                    "Marinisomatia","Dehalococcoidia","Verrucomicrobiae",
                    "Bacilli","Poseidoniia","Nitrososphaeria",
                    "Thermoproteia","Nanoarchaeia","Bathyarchaeia","Lokiarchaeia"))
dph_Class_codon_distance <- subset(dph_Class_features,distance=="codon_distance")
dph_Class_d2star <- subset(dph_Class_features,distance=="d2star")


### nucl distance: d2star ###
p4_1 <- ggplot(dph_sample_d2star) + 
#  ggplot(dph_sample_d2star) +
  geom_boxplot(aes(x=sample,y=value,fill=sample),outlier.shape = NA,width=0.7) + 
  geom_point(aes(x=sample,y=value),
             position = position_jitter(width = 0.10),
             size=0.06,alpha=0.3,color="#333333") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.04,0.58)) + 
  scale_fill_manual(values=c("#4DBBD5FF","#3C5488FF","#8491B4FF",
                             "#F8CDA0","#F5AA6A","#DA5516")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y = element_blank(),
        legend.position = "none",
        axis.title = element_blank())

p4_2 <- ggplot(dph_Class_d2star) + 
  #ggplot(dph_Class_d2star) +
  geom_boxplot(aes(x=Class,y=value,fill=Class),outlier.shape = NA,width=0.7) + 
  geom_point(aes(x=Class,y=value),
             position = position_jitter(width = 0.10),
             size=0.06,alpha=0.3,color="#333333") + 
  scale_y_continuous(expand = c(0,0),limits = c(0.04,0.58)) + 
  scale_fill_manual(values=c("#00A087FF","#00A087FF","#00A087FF",
                             "#00A087FF","#00A087FF","#00A087FF",
                             "#00A087FF","#00A087FF","#00A087FF",
                             "#00A087FF","#DC0000FF","#DC0000FF",
                             "#DC0000FF","#DC0000FF","#DC0000FF",
                             "#DC0000FF")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank())

### codon distance ###
p4_3 <- ggplot(dph_sample_codon_distance) + 
  #ggplot(dph_sample_codon_distance) +
  geom_boxplot(aes(x=sample,y=value,fill=sample),outlier.shape = NA,width=0.7) + 
  geom_point(aes(x=sample,y=value),
             position = position_jitter(width = 0.10),
             size=0.06,alpha=0.3,color="#333333") + 
  scale_y_continuous(expand = c(0,0),limits = c(0,0.15)) + 
  scale_fill_manual(values=c("#4DBBD5FF","#3C5488FF","#8491B4FF",
                             "#F8CDA0","#F5AA6A","#DA5516")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        #axis.text.y = element_blank(),
        legend.position = "none",
        axis.title = element_blank())

p4_4 <- ggplot(dph_Class_codon_distance) + 
  #ggplot(dph_Class_codon_distance) +
  geom_boxplot(aes(x=Class,y=value,fill=Class),outlier.shape = NA,width=0.7) + 
  geom_point(aes(x=Class,y=value),
             position = position_jitter(width = 0.10),
             size=0.06,alpha=0.3,color="#333333") + 
  scale_y_continuous(expand = c(0,0),limits = c(0,0.15)) + 
  scale_fill_manual(values=c("#00A087FF","#00A087FF","#00A087FF",
                             "#00A087FF","#00A087FF","#00A087FF",
                             "#00A087FF","#00A087FF","#00A087FF",
                             "#00A087FF","#DC0000FF","#DC0000FF",
                             "#DC0000FF","#DC0000FF","#DC0000FF",
                             "#DC0000FF")) +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank())


p_total <- list(p4_1,p4_2,
                p4_3,p4_4)
lay <- rbind(c(1,2,2),
             c(3,4,4))
p_out <- multiplot(plotlist = p_total[1:4], layout = lay)

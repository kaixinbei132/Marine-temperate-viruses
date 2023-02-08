library(ggplot2)
library(Rmisc)

##### read in data #####
dsample_infection <- read.table("4figure_sample_type_infection_count.txt",header = T)
dsample_infection$sample_type <- factor(dsample_infection$sample_type,
                                        levels = c("Epipelagic-Seawater","Mesopelagic-Seawater","Bathypelagic-Seawater",
                                                   "Epipelagic-Sediment","Mesopelagic-Sediment","Bathypelagic-Sediment"))
dsample_infection_w <- subset(dsample_infection,sample=="Seawater")
dsample_infection_s <- subset(dsample_infection,sample=="Sediment")


##### total seawater prophage coverage #####
dsample_prophage_cov <- read.table("4figure_Seawater_prophage_coverage_sample_type.txt",header = T)
dsample_prophage_cov$Sample_type <- factor(dsample_prophage_cov$Sample_type,
                                           levels = c("Epipelagic-Seawater","Mesopelagic-Seawater","Bathypelagic-Seawater"))


#p1 <- ggplot(dsample_prophage_cov,aes(x=Sample_type,y=Coverage,fill=Sample_type)) + 
p1<-  ggplot(dsample_prophage_cov,aes(x=Sample_type,y=Coverage,fill=Sample_type)) + 
  geom_bar(stat = "identity",width = 0.65) + 
  scale_fill_manual(values=c("#4DBBD5FF","#8491B4FF","#3C5488FF")) +  
  #scale_fill_manual(values=c("#4DBBD5FF","#3C5488FF","#EEAD0E","#BB5500")) +
  theme_bw() + 
  scale_y_continuous(expand = c(0,0),limits = c(0,0.55)) +  
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.x = element_text(angle=45,hjust = 1,vjust = 1),
        legend.position = "none") 
  #coord_flip()
#ggsave(p1,filename = "total_Seawater_sample_type_prophage_coverage.pdf",height = 15,width = 10,units = "cm")


##### total sediment prophage coverage #####
dsample_prophage_cov <- read.table("4figure_Sediment_prophage_coverage_sample_type.txt",header = T)
dsample_prophage_cov$Sample_type <- factor(dsample_prophage_cov$Sample_type,
                                           levels = c("Epipelagic-Sediment","Mesopelagic-Sediment","Bathypelagic-Sediment"))
p2 <- ggplot(dsample_prophage_cov,aes(x=Sample_type,y=Coverage,fill=Sample_type)) + 
  #ggplot(dsample_prophage_cov,aes(x=Sample_type,y=Coverage,fill=Sample_type)) + 
  geom_bar(stat = "identity",width = 0.65) + 
  scale_fill_manual(values=c("#FDD0A2FF","#FDAE6BFF","#E6550DFF")) +  
  #scale_fill_manual(values=c("#4DBBD5FF","#3C5488FF","#EEAD0E","#BB5500")) +
  theme_bw() + scale_y_continuous(expand = c(0,0),limits = c(0,0.55)) +  
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.x = element_text(angle=45,hjust = 1,vjust = 1),
        legend.position = "none") 
  #coord_flip()
#ggsave(p1,filename = "total_Sediment_sample_type_prophage_coverage.pdf",height = 20,width = 10,units = "cm")


##### infection count #####
p3 <- ggplot(dsample_infection_w,aes(x=sample_type,y=count,fill=sample_type)) + 
  geom_bar(stat = "identity",width = 0.65) + 
  scale_fill_manual(values=c("#4DBBD5FF","#8491B4FF","#3C5488FF")) +  
  #scale_fill_manual(values=c("#4DBBD5FF","#3C5488FF","#EEAD0E","#BB5500")) +
  theme_bw() + 
  scale_y_continuous(expand = c(0,0),limits = c(0,3.2)) +  
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.x = element_text(angle=45,hjust = 1,vjust = 1),
        legend.position = "none") 


p4 <- ggplot(dsample_infection_s,aes(x=sample_type,y=count,fill=sample_type)) + 
  #ggplot(dsample_prophage_cov,aes(x=Sample_type,y=Coverage,fill=Sample_type)) + 
  geom_bar(stat = "identity",width = 0.65) + 
  scale_fill_manual(values=c("#FDD0A2FF","#FDAE6BFF","#E6550DFF")) +  
  #scale_fill_manual(values=c("#4DBBD5FF","#3C5488FF","#EEAD0E","#BB5500")) +
  theme_bw() + 
  scale_y_continuous(expand = c(0,0),limits = c(0,3.2)) +  
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.x = element_text(angle=45,hjust = 1,vjust = 1),
        legend.position = "none") 


pdf("merged_sample_coverage_20220409.pdf",width = 5,height = 5.5)
p_total <- list(p1,p2,
                p3,p4)
lay <- rbind(c(1,2),
             c(3,4))
multiplot(plotlist = p_total[1:4], layout = lay)
dev.off()

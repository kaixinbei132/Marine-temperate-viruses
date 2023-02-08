library(maps)
library(ggplot2)
library(Rmisc)

x = c(1:10)
y = c(11:20)
par(pin=c(20,10))


###### map #####
dsample_locus <- read.table("4Rfigure_map_locus.txt",header = T)
#dsample_locus <- read.table("4Rfigure_map_locus.txt",header = T)
dsample_locus$total_sample <- factor(dsample_locus$total_sample,
                                    levels = c("Seawater","Sediment",
                                               "Mixed","Ambiguous"))
mp<- ggplot() + 
#ggplot() +
  scale_y_continuous(expand = c(0,0),limits = c(-90,90)) +
  scale_x_continuous(expand = c(0,0),limits = c(-185,185)) +
  borders("world",size=0.5,colour ="white",fill="white") + 
  geom_point(aes(x=dsample_locus$lon,y=dsample_locus$lat,color=dsample_locus$total_sample),alpha=0.85,size=3.8) +
  #geom_text(aes(x=dsample_locus$lon,y=dsample_locus$lat,label=dsample_locus$sample_id),size=1)+
  scale_color_manual(values = c("#5599FF","#DAA520","#228B22","#999999")) + 
  #scale_size_continuous(range = c(2.5,8)) +
  theme(panel.background = element_rect(fill = alpha("SkyBlue3",alpha=0.2)),
        #panel.grid.major = element_line(linetype = "dashed",color = "#CCCCCC",size = 0.2),
        #panel.grid.minor = element_line(linetype = "dashed",color = "#CCCCCC",size = 0.2),
        panel.grid = element_blank(),
        legend.position = "none")
ggsave(mp,filename = "map.pdf",height = 22,width = 40,units = "cm")


###### seawater depth distribution #####
dsample_depth <- read.table("4Rfigure_Seawater_depth_distribution.txt",header = T)
dsample_depth$range <- factor(dsample_depth$range,
                              levels = c("Epipelagic","Mesopelagic","Bathypelagic"))
p1 <- ggplot(dsample_depth) + 
#ggplot(dsample_depth) +
  geom_point(aes(x=sample_id,y=depth,fill=range,size=size),shape=21,alpha=0.85) + 
  #geom_jitter(shape=21,alpha=0.85,size=2,position = position_jitterdodge(jitter.width = 500,jitter.height = 20)) + 
  geom_hline(yintercept = c(-200,-1000,-6000),linetype="dashed") + 
  scale_size_continuous(range = c(5,15)) +
  scale_fill_manual(values = c("#4DBBD5FF","#8491B4FF","#3C5488FF")) + 
  #scale_color_manual(values = c("#79CDCD","#708090","#191970")) +
  scale_y_continuous(limits = c(-11094,0)) + 
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        #axis.text.x = element_text(angle=45,hjust = 1,vjust = 1),
        legend.position = "none",
        panel.grid = element_blank())
#ggsave(pdepth,filename = "depth_seawater.pdf",height = 20,width = 12,units = "cm")


###### sediment depth distribution #####
dsample_depth <- read.table("4Rfigure_Sediment_depth_distribution.txt",header = T)
dsample_depth$range <- factor(dsample_depth$range,
                              levels = c("Epipelagic","Mesopelagic","Bathypelagic"))
p2 <- ggplot(dsample_depth) + 
  #ggplot(dsample_depth) +
  geom_point(aes(x=sample_id,y=depth,fill=range,size=size),shape=21,alpha=0.85) + 
  #geom_jitter(shape=21,alpha=0.85,size=2,position = position_jitterdodge(jitter.width = 500,jitter.height = 20)) + 
  geom_hline(yintercept = c(-200,-1000,-6000),linetype="dashed") + 
  scale_size_continuous(range = c(5,15)) +
  scale_fill_manual(values = c("#FDD0A2FF","#FDAE6BFF","#E6550DFF")) + 
  #scale_color_manual(values = c("#79CDCD","#708090","#191970")) +
  scale_y_continuous(limits = c(-11094,0)) + 
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        #axis.text.x = element_text(angle=45,hjust = 1,vjust = 1),
        legend.position = "none",
        panel.grid = element_blank())
#ggsave(pdepth,filename = "depth_sediment.pdf",height = 20,width = 12,units = "cm")


###### seawater sample type count #####
dsample <- read.table("4figure_sample_type_Seawater_count.txt",sep="\t",header=T)
dsample$sample_type <- factor(dsample$sample_type,levels = c(
  "Epipelagic-Seawater","Mesopelagic-Seawater","Bathypelagic-Seawater"))
p3 <- ggplot(data = dsample, mapping = aes(x = 'Content', y = Count, fill = sample_type)) + 
  #ggplot(data = dsample, mapping = aes(x = 'Content', y = Count, fill = sample_type)) + 
  geom_bar(stat = 'identity', position = 'stack') + 
  scale_fill_manual(values = c("#4DBBD5FF","#8491B4FF","#3C5488FF")) + 
  coord_polar(theta = 'y') + 
  theme_void()+ 
  theme(legend.position = "none")
#ggsave(p,filename = "sample_type_Seawater_pie.pdf",height = 20,width = 20,units = "cm")


###### sediment sample type count #####
dsample <- read.table("4figure_sample_type_Sediment_count.txt",sep="\t",header=T)
dsample$sample_type <- factor(dsample$sample_type,levels = c(
  "Epipelagic-Seawater","Mesopelagic-Seawater","Bathypelagic-Seawater",
  "Epipelagic-Sediment","Mesopelagic-Sediment","Bathypelagic-Sediment",
  "Ambiguous"))
p4 <- ggplot(data = dsample, mapping = aes(x = 'Content', y = Count, fill = sample_type)) + 
  #ggplot(data = dsample, mapping = aes(x = 'Content', y = Count, fill = sample_type)) + 
  geom_bar(stat = 'identity', position = 'stack') + 
  scale_fill_manual(values = c("#FDD0A2FF","#FDAE6BFF","#E6550DFF")) + 
  coord_polar(theta = 'y') + 
  theme_void() + 
  theme(legend.position = "none")
#ggsave(p,filename = "sample_type_Sediment_pie.pdf",height = 20,width = 20,units = "cm")


p_total <- list(p1,p2,p3,p4)
lay <- rbind(c(1,2),
             c(1,2),
             c(1,2),
             c(3,4))
p_out <- multiplot(plotlist = p_total[1:4], layout = lay)

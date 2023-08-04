

env_sem=fread("Datafile/SEM_envi.csv")
env_sem$day=as.Date(env_sem$Date)

##UB cluster=================
traj_ub20 <- readRDS("Backup/traj/TrajData_ul_2020.rds")
traj_ub21 <- readRDS("Backup/traj/TrajData_ul_2021.rds")

env_ub=subset(env_sem,env_sem$Group=="UT")
traj_ub=rbind(traj_ub20,traj_ub21)

traj_ub_sel=subset(traj_ub,traj_ub$hour.inc>-73)

traj_ub_sel
traj_ub_sel$local=traj_ub_sel$date+3600*8
traj_ub_sel$day=as.Date(traj_ub_sel$local-3600*10)

traj_ub_sel2 <- inner_join(traj_ub_sel, env_se[,-c(1:4,6)], by = "day")
traj_ub_sel2


traj_ub_n=traj_ub_sel2
traj_ub_n$d3=traj_ub_n$date-3600*10
traj_ub_n$dn=as.character(traj_ub_n$day)
table(traj_ub_n$dn)

trajMap(traj_ub_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

mycol_c=c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70")

tiff(paste0("Figure/23615ul_ctraj_n4","_angle.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
ul_clust <- trajCluster(traj_ub_n, 
                        method = "angle", 
                        n.cluster = 4, 
                        col = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
                        projection ="albers",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        grid.col="transparent",
                        xlim=c(70,140),
                        ylim=c(33,53)
)
dev.off()

ul_clust$data$traj

ul_clust <- trajCluster_ms(traj_ub_n, 
                                    method = "angle", 
                                    n.cluster = 4, 
                                    col  = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
                                    projection ="albers",
                                    orientation = c(0,45,0),
                                    map.cols ="grey45",
                                    xlab="Longitude (°)",
                                    ylab="Latitude (°)",
                                    xlim=c(70,140),
                                    ylim=c(33,55),
                                    #grid.col="black",
                                    grid.col="transparent",
                                    lwd = 6,
                                    key=F,
                           col = "Set2",
                     map.cols = openColours("Paired", 10)
)


traj_ub_c=filter(ul_clust$data$traj, hour.inc == 0)
traj_ub_c$d3
traj_ub_c$Freq=1
traj_ub_c$newd=as.factor(format(traj_ub_c$day, "%b-%d"))

data_dend_ub=traj_ub_c[,c("Freq","cluster","newd")]
data_dend_ub2=dcast(data_dend_ub,newd~cluster,sum, value.var = "Freq")
data_dend_ub2=data_dend_ub2 %>% `row.names<-`(data_dend_ub2$newd)

dend_ub <- as.dendrogram(hclust(dist(data_dend_ub2)))

p1 <- ggplot(dend_ub, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"))
p1


traj_ub_c$n2 <- factor(traj_ub_c$newd, levels = labels(dend_ub))
dend_ub
traj_ub_c

tub=unique(traj_ub_c[,c("n2","Event")])
tub$col <- ifelse(tub$Event == "Event", "red",
                  ifelse(tub$Event == "Normal", "black","black"))

ub_nv=data.frame(n2=labels(dend_ub))
ub_nv$n=row.names(ub_nv)
ub_nv=ub_nv %>% inner_join(tub)
ub_nv
ub_cs=as.vector((ub_nv$col))

dend_ub <- data_dend_ub2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80")

p1 <- ggplot(dend_ub, horiz = T)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_ub_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values = c("#FF5D00","#EBA91A","#11AAFB","#7868E6"))+
  scale_color_manual(values =c("#FF5D00","#EBA91A","#11AAFB","#7868E6"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = ub_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=18,face="bold",color = "black"),
        legend.position = c(0.9,0.15)
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("ub_traj_trajc_cluster_bar"),height = 20, width = 25, units = "cm", dpi = 300, compression="lzw")



#plot_grid(p1, p2, align = "h")+
#  ggsave(filename("ub_traj_trajc_cluster_fin"),height = 20, width = 45, units = "cm", dpi = 300, compression="lzw")


##BJ cluster=================
traj_bj20 <- readRDS("Backup/traj/TrajData_bj_2020.rds")
traj_bj21 <- readRDS("Backup/traj/TrajData_bj_2021.rds")

env_bj=subset(env_sem,env_sem$Group=="BJ")
traj_bj=rbind(traj_bj20,traj_bj21)

scale_fill_manual(values = c("#FF5D00","#EBA91A","#11AAFB","#7868E6"))

traj_bj_sel=subset(traj_bj,traj_bj$hour.inc>-73)

traj_bj_sel
traj_bj_sel$local=traj_bj_sel$date+3600*8
traj_bj_sel$day=as.Date(traj_bj_sel$local-3600*10)

traj_bj_sel2 <- inner_join(traj_bj_sel, env_bj[,-c(1:4,6)], by = "day")
traj_bj_sel2

traj_bj_n=traj_bj_sel2
traj_bj_n$d3=traj_bj_n$date-3600*10
traj_bj_n$dn=as.character(traj_bj_n$day)
traj_bj_n$Event

tiff(paste0("Figure/230615BJ_ctraj_n3","_angle2.tiff"), width = 15, height = 15,units = "cm",res = 300, compression = "lzw")
bj_clust <- trajCluster(traj_bj_n, 
                           method = "angle", 
                           n.cluster = 3, 
                           col  = c("#FF5D00","#EBA91A","#11AAFB"),
                           projection ="albers",
                           orientation = c(0,45,0),
                           xlab="Longitude (°)",
                           ylab="Latitude (°)",
                           xlim=c(70,140),
                           ylim=c(33,55),
                           #grid.col="black",
                           grid.col="transparent",
                           lwd = 6,
                           key=F,
                           map.cols = openColours("Paired", 20)
)
dev.off()

traj_bj_c=filter(bj_clust$data$traj, hour.inc == 0)
traj_bj_c$d3
traj_bj_c$Freq=1
traj_bj_c$newd=as.factor(format(traj_bj_c$day, "%b-%d"))

data_dend_bj=traj_bj_c[,c("Freq","cluster","newd")]


data_dend_bj2=dcast(data_dend_bj,newd~cluster,sum, value.var = "Freq")
data_dend_bj2=data_dend_bj2 %>% `row.names<-`(data_dend_bj2$newd)

dend_bj <- as.dendrogram(hclust(dist(data_dend_bj2)))
p1 <- ggplot(dend_bj, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"))
p1

traj_bj_c$n2 <- factor(traj_bj_c$newd, levels = labels(dend_bj))
traj_bj_c$n2
traj_bj_c$event

traj_bj_sel2$Event

tbj=unique(traj_bj_c[,c("n2","Event")])
tbj$col <- ifelse(tbj$Event == "Event", "red",
                  ifelse(tbj$Event == "Normal", "black","black"))

bj_nv=data.frame(n2=labels(dend_bj))
bj_nv$n=row.names(bj_nv)
bj_nv=bj_nv %>% inner_join(tbj)
bj_nv
bj_cs=as.vector((bj_nv$col))

dend_bj <- data_dend_bj2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_bj, horiz = T)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_bj_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values = c("#FF5D00","#EBA91A","#11AAFB"))+
  scale_color_manual(values =c("#FF5D00","#EBA91A","#11AAFB"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = bj_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position =c(0.9,0.15)
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("bj_traj_trajc_cluster_bar"),height = 20, width = 25, units = "cm", dpi = 300, compression="lzw")


##ss cluster=================
traj_ss20 <- readRDS("Backup/traj/TrajData_ss_2020.rds")
traj_ss21 <- readRDS("Backup/traj/TrajData_ss_2021.rds")

env_ss=subset(env_sem,env_sem$Group=="SS")
traj_ss=rbind(traj_ss20,traj_ss21)

traj_ss_sel=subset(traj_ss,traj_ss$hour.inc>-73)

traj_ss_sel
traj_ss_sel$local=traj_ss_sel$date+3600*8
traj_ss_sel$day=as.Date(traj_ss_sel$local-3600*10)

traj_ss_sel2 <- inner_join(traj_ss_sel, env_ss[,-c(1:4,6)], by = "day")
traj_ss_sel2

traj_ss_n=traj_ss_sel2
traj_ss_n$d3=traj_ss_n$date-3600*10
traj_ss_n$dn=as.character(traj_ss_n$day)
traj_ss_n$Event

trajMap(traj_ss_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)



tiff(paste0("Figure/230615ss_ctraj_n3","_angle2.tiff"), width = 15, height = 15,units = "cm",res = 300, compression = "lzw")
ss_clust <- trajCluster(traj_ss_n, 
                        method = "angle", 
                        n.cluster = 3, 
                        col  = c("#FF5D00","#EBA91A","#11AAFB"),
                        projection ="albers",
                        orientation = c(0,45,0),
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        xlim=c(70,140),
                        ylim=c(33,55),
                        #grid.col="black",
                        grid.col="transparent",
                        lwd = 6,
                        key=F,
                        map.cols = openColours("Paired", 20)
)
dev.off()

traj_ss_c=filter(ss_clust$data$traj, hour.inc == 0)
traj_ss_c$d3
traj_ss_c$Freq=1
traj_ss_c$newd=as.factor(format(traj_ss_c$day, "%b-%d"))

data_dend_ss=traj_ss_c[,c("Freq","cluster","newd")]


data_dend_ss2=dcast(data_dend_ss,newd~cluster,sum, value.var = "Freq")
data_dend_ss2=data_dend_ss2 %>% `row.names<-`(data_dend_ss2$newd)

dend_ss <- as.dendrogram(hclust(dist(data_dend_ss2)))
p1 <- ggplot(dend_ss, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"))
p1

traj_ss_c$n2 <- factor(traj_ss_c$newd, levels = labels(dend_ss))
traj_ss_c$n2
traj_ss_c$event

traj_ss_sel2$Event

tss=unique(traj_ss_c[,c("n2","Event")])
tss$col <- ifelse(tss$Event == "Event", "red",
                  ifelse(tss$Event == "Normal", "black","black"))

ss_nv=data.frame(n2=labels(dend_ss))
ss_nv$n=row.names(ss_nv)
ss_nv=ss_nv %>% inner_join(tss)
ss_nv
ss_cs=as.vector((ss_nv$col))

dend_ss <- data_dend_ss2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_ss, horiz = T)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_ss_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values = c("#FF5D00","#EBA91A","#11AAFB"))+
  scale_color_manual(values =c("#FF5D00","#EBA91A","#11AAFB"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = bj_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position =c(0.9,0.15)
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("ss_traj_trajc_cluster_bar"),height = 20, width = 25, units = "cm", dpi = 300, compression="lzw")



##se cluster=================
traj_se20 <- readRDS("Backup/traj/TrajData_se_2020.rds")
traj_se21 <- readRDS("Backup/traj/TrajData_se_2021.rds")

env_se=subset(env_sem,env_sem$Group=="SE")
traj_se=rbind(traj_se20,traj_se21)

traj_se_sel=subset(traj_se,traj_se$hour.inc>-73)

traj_se_sel
traj_se_sel$local=traj_se_sel$date+3600*8
traj_se_sel$day=as.Date(traj_se_sel$local-3600*10)

traj_se_sel2 <- inner_join(traj_se_sel, env_se[,-c(1:4,6)], by = "day")
traj_se_sel2

traj_se_n=traj_se_sel2
traj_se_n$d3=traj_se_n$date-3600*10
traj_se_n$dn=as.character(traj_se_n$day)
traj_se_n$Event

trajMap(traj_se_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

tiff(paste0("Figure/230615se_ctraj_n3","_angle2.tiff"), width = 15, height = 15,units = "cm",res = 300, compression = "lzw")
se_clust <- trajCluster(traj_se_n, 
                        method = "angle", 
                        n.cluster = 3, 
                        col  = c("#FF5D00","#EBA91A","#11AAFB"),
                        projection ="albers",
                        orientation = c(0,45,0),
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        xlim=c(70,140),
                        ylim=c(33,55),
                        #grid.col="black",
                        grid.col="transparent",
                        lwd = 6,
                        key=F,
                        map.cols = openColours("Paired", 20)
)
dev.off()

traj_se_c=filter(se_clust$data$traj, hour.inc == 0)
traj_se_c$d3
traj_se_c$Freq=1
traj_se_c$newd=as.factor(format(traj_se_c$day, "%b-%d"))

data_dend_se=traj_se_c[,c("Freq","cluster","newd")]

data_dend_se2=dcast(data_dend_se,newd~cluster,sum, value.var = "Freq")
data_dend_se2=data_dend_se2 %>% `row.names<-`(data_dend_se2$newd)

dend_se <- as.dendrogram(hclust(dist(data_dend_se2)))
p1 <- ggplot(dend_se, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"))
p1

traj_se_c$n2 <- factor(traj_se_c$newd, levels = labels(dend_se))
traj_se_c$n2
traj_se_c$event

traj_se_sel2$Event

tse=unique(traj_se_c[,c("n2","Event")])
tse$col <- ifelse(tse$Event == "Event", "red",
                  ifelse(tse$Event == "Normal", "black","black"))

se_nv=data.frame(n2=labels(dend_se))
se_nv$n=row.names(se_nv)
se_nv=se_nv %>% inner_join(tse)
se_nv
se_cs=as.vector((se_nv$col))

dend_se <- data_dend_se2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_se, horiz = T)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_se_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values = c("#FF5D00","#EBA91A","#11AAFB"))+
  scale_color_manual(values =c("#FF5D00","#EBA91A","#11AAFB"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = bj_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position =c(0.9,0.15)
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("se_traj_trajc_cluster_bar"),height = 20, width = 25, units = "cm", dpi = 300, compression="lzw")


##nt cluster=================
traj_nt20 <- readRDS("Backup/traj/TrajData_nt_2020.rds")
traj_nt21 <- readRDS("Backup/traj/TrajData_nt_2021.rds")

env_nt=subset(env_sem,env_sem$Group=="NT")
traj_nt=rbind(traj_nt20,traj_nt21)

traj_nt_ntl=subset(traj_nt,traj_nt$hour.inc>-73)

traj_nt_ntl
traj_nt_ntl$local=traj_nt_ntl$date+3600*8
traj_nt_ntl$day=as.Date(traj_nt_ntl$local-3600*10)

traj_nt_ntl2 <- inner_join(traj_nt_ntl, env_nt[,-c(1:4,6)], by = "day")
traj_nt_ntl2

traj_nt_n=traj_nt_ntl2
traj_nt_n$d3=traj_nt_n$date-3600*10
traj_nt_n$dn=as.character(traj_nt_n$day)
traj_nt_n$Event

trajMap(traj_nt_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

tiff(paste0("Figure/230615nt_ctraj_n3","_angle2.tiff"), width = 15, height = 15,units = "cm",res = 300, compression = "lzw")
nt_clust <- trajCluster(traj_nt_n, 
                        method = "angle", 
                        n.cluster = 3, 
                        col  = c("#FF5D00","#EBA91A","#11AAFB"),
                        projection ="albers",
                        orientation = c(0,45,0),
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        xlim=c(70,140),
                        ylim=c(33,55),
                        #grid.col="black",
                        grid.col="transparent",
                        lwd = 6,
                        key=F,
                        map.cols = openColours("Paired", 20)
)
dev.off()

traj_nt_c=filter(nt_clust$data$traj, hour.inc == 0)
traj_nt_c$d3
traj_nt_c$Freq=1
traj_nt_c$newd=as.factor(format(traj_nt_c$day, "%b-%d"))

data_dend_nt=traj_nt_c[,c("Freq","cluster","newd")]

data_dend_nt2=dcast(data_dend_nt,newd~cluster,sum, value.var = "Freq")
data_dend_nt2=data_dend_nt2 %>% `row.names<-`(data_dend_nt2$newd)

dend_nt <- as.dendrogram(hclust(dist(data_dend_nt2)))
p1 <- ggplot(dend_nt, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"))
p1

traj_nt_c$n2 <- factor(traj_nt_c$newd, levels = labels(dend_nt))
traj_nt_c$n2
traj_nt_c$event

traj_nt_ntl2$Event

tnt=unique(traj_nt_c[,c("n2","Event")])
tnt$col <- ifelse(tnt$Event == "Event", "red",
                  ifelse(tnt$Event == "Normal", "black","black"))

nt_nv=data.frame(n2=labels(dend_nt))
nt_nv$n=row.names(nt_nv)
nt_nv=nt_nv %>% inner_join(tnt)
nt_nv
nt_cs=as.vector((nt_nv$col))

dend_nt <- data_dend_nt2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_nt, horiz = T)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_nt_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values = c("#FF5D00","#EBA91A","#11AAFB"))+
  scale_color_manual(values =c("#FF5D00","#EBA91A","#11AAFB"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = bj_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position =c(0.9,0.15)
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("nt_traj_trajc_cluster_bar"),height = 20, width = 25, units = "cm", dpi = 300, compression="lzw")


#6. cluster dendrogram=====
###ub=====
traj_ub_c=filter(ul_clust$data$traj, hour.inc == 0)
traj_ub_c$d3
traj_ub_c$Freq=1
traj_ub_c$newd=as.factor(format(traj_ub_c$day, "%b-%d"))

data_dend_ub=traj_ub_c[,c("Freq","cluster","newd")]
data_dend_ub2=dcast(data_dend_ub,newd~cluster,sum, value.var = "Freq")
data_dend_ub2=data_dend_ub2 %>% `row.names<-`(data_dend_ub2$newd)

dend_ub <- as.dendrogram(hclust(dist(data_dend_ub2)))

p1 <- ggplot(dend_ub, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"))
p1

traj_ub_c$n2 <- factor(traj_ub_c$newd, levels = labels(dend_ub))
dend_ub
traj_ub_c

tub=unique(traj_ub_c[,c("n2","Event")])
tub$col <- ifelse(tub$Event == "Event", "red",
                  ifelse(tub$Event == "Normal", "black","black"))

ub_nv=data.frame(n2=labels(dend_ub))
ub_nv$n=row.names(ub_nv)
ub_nv=ub_nv %>% inner_join(tub)
ub_nv
ub_cs=as.vector((ub_nv$col))

dend_ub <- data_dend_ub2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80")

p1 <- ggplot(dend_ub, horiz = T)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_ub_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values = c("#FF5D00","#EBA91A","#11AAFB","#7868E6"))+
  scale_color_manual(values =c("#FF5D00","#EBA91A","#11AAFB","#7868E6"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = ub_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = c(0.8,0.2)
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

p2

plot_grid(p1, p2, align = "h")+
  ggsave(filename("ub_traj_trajc_cluster_fin"),height = 20, width = 45, units = "cm", dpi = 300, compression="lzw")


###bj=====
traj_bj_c=filter(bj_clust$data$traj, hour.inc == 0)
traj_bj_c$d3
traj_bj_c$Freq=1
traj_bj_c$newd=as.factor(format(traj_bj_c$day, "%b-%d"))

data_dend_bj=traj_bj_c[,c("Freq","cluster","newd")]


data_dend_bj2=dcast(data_dend_bj,newd~cluster,sum, value.var = "Freq")
data_dend_bj2=data_dend_bj2 %>% `row.names<-`(data_dend_bj2$newd)

dend_bj <- as.dendrogram(hclust(dist(data_dend_bj2)))
p1 <- ggplot(dend_bj, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"))
p1

traj_bj_c$n2 <- factor(traj_bj_c$newd, levels = labels(dend_bj))
traj_bj_c$n2
traj_bj_c$event

traj_bj_sel2$Event


tbj=unique(traj_bj_c[,c("n2","Event")])
tbj$col <- ifelse(tbj$Event == "Event", "red",
                  ifelse(tbj$Event == "Normal", "black","black"))

bj_nv=data.frame(n2=labels(dend_bj))
bj_nv$n=row.names(bj_nv)
bj_nv=bj_nv %>% inner_join(tbj)
bj_nv
bj_cs=as.vector((bj_nv$col))

dend_bj <- data_dend_bj2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_bj, horiz = T)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_bj_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values = c("#7868E6","royalblue4","#562B08"))+
  scale_color_manual(values =c("#7868E6","royalblue4","#562B08"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = bj_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "right"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("bj_traj_trajc_cluster_fin"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")



###ss=====
traj_ss_c=filter(ss_clust$data$traj, hour.inc == 0)
traj_ss_c$d3
traj_ss_c$Freq=1
traj_ss_c$newd=as.factor(format(traj_ss_c$day, "%b-%d"))

data_dend_ss=traj_ss_c[,c("Freq","cluster","newd")]


data_dend_ss2=dcast(data_dend_ss,newd~cluster,sum, value.var = "Freq")
data_dend_ss2=data_dend_ss2 %>% `row.names<-`(data_dend_ss2$newd)

dend_ss <- as.dendrogram(hclust(dist(data_dend_ss2)))
p1 <- ggplot(dend_ss, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"))
p1

traj_ss_c$n2 <- factor(traj_ss_c$newd, levels = labels(dend_ss))
traj_ss_c$n2
traj_ss_c$event

traj_ss_sel2$Event

ss=unique(traj_ss_c[,c("n2","Event")])
tss$col <- ifelse(tss$Event == "Event", "red",
                  ifelse(tss$Event == "Normal", "black","black"))

ss_nv=data.frame(n2=labels(dend_ss))
ss_nv$n=row.names(ss_nv)
ss_nv=ss_nv %>% inner_join(tss)
ss_nv
ss_cs=as.vector((ss_nv$col))

dend_ss <- data_dend_ss2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_ss, horiz = T)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_ss_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_ssw_c$n2)))+
  scale_fill_manual(values = c("#7868E6","royalblue4","#562B08"))+
  scale_color_manual(values =c("#7868E6","royalblue4","#562B08"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = ss_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "right"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("ss_traj_trajc_cluster_fin"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")



###se=====
traj_se_c=filter(se_clust$data$traj, hour.inc == 0)
traj_se_c$d3
traj_se_c$Freq=1
traj_se_c$newd=as.factor(format(traj_se_c$day, "%b-%d"))

data_dend_se=traj_se_c[,c("Freq","cluster","newd")]
data_dend_se2=dcast(data_dend_se,newd~cluster,sum, value.var = "Freq")
data_dend_se2=data_dend_se2 %>% `row.names<-`(data_dend_se2$newd)

dend_se <- as.dendrogram(hclust(dist(data_dend_se2)))
p1 <- ggplot(dend_se, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,-0.2,0.2,0.2),"cm"))
p1

traj_se_c$n2 <- factor(traj_se_c$newd, levels = labels(dend_se))
traj_se_c$n2
traj_se_c$event

traj_se_sel2$Event


tse=unique(traj_se_c[,c("n2","Event")])
tse$col <- ifelse(tse$Event == "Event", "red",
                  ifelse(tse$Event == "Normal", "black","black"))

se_nv=data.frame(n2=labels(dend_se))
se_nv$n=row.names(se_nv)
se_nv=se_nv %>% inner_join(tse)
se_nv
se_cs=as.vector((se_nv$col))

dend_se <- data_dend_se2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))


p1 <- ggplot(dend_se, horiz = T)+
  scale_x_continuous(expand = c(0.085, 0.085))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_se_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_sew_c$n2)))+
  scale_fill_manual(values = c("#BC3C29FF","#400082","#11AAFB"))+
  scale_color_manual(values =c("#BC3C29FF","#400082","#11AAFB"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = se_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("se_traj_trajc_cluster_fin"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")


###nt=====
traj_nt_c=filter(nt_clust$data$traj, hour.inc == 0)
traj_nt_c$d3
traj_nt_c$Freq=1
traj_nt_c$newd=as.factor(format(traj_nt_c$day, "%b-%d"))

data_dend_nt=traj_nt_c[,c("Freq","cluster","newd")]
data_dend_nt2=dcast(data_dend_nt,newd~cluster,sum, value.var = "Freq")
data_dend_nt2=data_dend_nt2 %>% `row.names<-`(data_dend_nt2$newd)

dend_nt <- as.dendrogram(hclust(dist(data_dend_nt2)))
p1 <- ggplot(dend_nt, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,-0.2,0.2,0.2),"cm"))
p1

traj_nt_c$n2 <- factor(traj_nt_c$newd, levels = labels(dend_nt))
traj_nt_c$n2
traj_nt_c$event

traj_nt_ntl2$Event


tnt=unique(traj_nt_c[,c("n2","Event")])
tnt$col <- ifelnt(tnt$Event == "Event", "red",
                  ifelnt(tnt$Event == "Normal", "black","black"))

nt_nv=data.frame(n2=labels(dend_nt))
nt_nv$n=row.names(nt_nv)
nt_nv=nt_nv %>% inner_join(tnt)
nt_nv
nt_cs=as.vector((nt_nv$col))

dend_nt <- data_dend_nt2 %>% dist %>% hclust %>% as.dendrogram %>% ntt("labels_cex", c(0))


p1 <- ggplot(dend_nt, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,-3.4,0.2,0.2),"cm"))

p2 <- ggplot(traj_nt_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(revernt = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_ntw_c$n2)))+
  scale_fill_manual(values = c("#BC3C29FF","#400082","#11AAFB"))+
  scale_color_manual(values =c("#BC3C29FF","#400082","#11AAFB"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = nt_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("nt_traj_trajc_cluster_fin"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")


env_2d=fread("Datafile/2DGC_enviv2.csv")
env_2d$day=as.Date(env_2d$Date)
env_2d=subset(env_2d,env_2d$Event=="Event")

##UB cluster=================
env_ub=subset(env_2d,env_2d$Group=="Ulaanbaatar")
traj_ub=rbind(traj_ub20,traj_ub21)

traj_ub_sel=subset(traj_ub,traj_ub$hour.inc>-73)

traj_ub_sel
traj_ub_sel$local=traj_ub_sel$date+3600*8
traj_ub_sel$day=as.Date(traj_ub_sel$local-3600*10)

traj_ub_sel2 <- inner_join(traj_ub_sel, env_ub[,-c(1:3,5)], by = "day")
traj_ub_sel2


traj_ub_n=traj_ub_sel2
traj_ub_n$d3=traj_ub_n$date-3600*10
traj_ub_n$dn=as.character(traj_ub_n$day)
table(traj_ub_n$dn)

trajMap(traj_ub_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

mycol_c=c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70")

tiff(paste0("Figure/23615ul_ctraj_n3","_angle.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
ul_clust <- trajCluster(traj_ub_n, 
                        method = "angle", 
                        n.cluster = 3, 
                        col = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
                        projection ="lambert",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        grid.col="transparent",
                        xlim=c(90,130),
                        ylim=c(33,53)
)
dev.off()

tiff("Figure/230615ul_ctraj_n3_nolabel.tiff", width = 15, height = 15,units = "cm",res = 300, compression = "lzw")
bjw_clust_nolabel <- trajCluster_ms(traj_ub_n, 
                                    method = "angle", 
                                    n.cluster = 3, 
                                    col  = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
                                    projection ="albers",
                                    orientation = c(0,45,0),
                                    map.cols ="grey45",
                                    xlab="Longitude (°)",
                                    ylab="Latitude (°)",
                                    xlim=c(90,130),
                                    ylim=c(33,55),
                                    #grid.col="black",
                                    grid.col="transparent",
                                    lwd = 6,
                                    key=F
)
dev.off()

##BJ cluster=================
env_bj=subset(env_2d,env_2d$Group=="Beijing")
traj_bj=rbind(traj_bj20,traj_bj21)

traj_bj_sel=subset(traj_bj,traj_bj$hour.inc>-73)

traj_bj_sel
traj_bj_sel$local=traj_bj_sel$date+3600*8
traj_bj_sel$day=as.Date(traj_bj_sel$local-3600*10)

traj_bj_sel2 <- inner_join(traj_bj_sel, env_bj[,-c(1:3,5)], by = "day")
traj_bj_sel2

traj_bj_n=traj_bj_sel2
traj_bj_n$d3=traj_bj_n$date-3600*10
traj_bj_n$dn=as.character(traj_bj_n$day)
traj_bj_n$Event

trajMap(traj_bj_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

tiff(paste0("Figure/230615BJ_ctraj_n3","_angle2.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
bj_clust <- trajCluster(traj_bj_n, 
                        #method = "Euclid",
                        method = "angle", 
                        n.cluster = 3, 
                        col = c("#7868E6","royalblue4","#562B08"),
                        projection ="lambert",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        grid.col="transparent",
                        xlim=c(70,130),
                        ylim=c(33,55)
)
dev.off()

tiff("Figure/230615BJ_ctraj_n3_nolabel.tiff", width = 15, height = 15,units = "cm",res = 300, compression = "lzw")
bjw_clust_nolabel <- trajCluster_ms(traj_bj_n, 
                                    method = "angle", 
                                    n.cluster = 3, 
                                    col = c("#7868E6","royalblue4","#562B08"),
                                    projection ="albers",
                                    orientation = c(0,45,0),
                                    map.cols ="grey45",
                                    xlab="Longitude (°)",
                                    ylab="Latitude (°)",
                                    xlim=c(90,130),
                                    ylim=c(33,55),
                                    #grid.col="black",
                                    grid.col="transparent",
                                    lwd = 6,
                                    key=F
)
dev.off()


##SE cluster=================
env_se=subset(env_2d,env_2d$Group=="Seoul")
traj_se=rbind(traj_se20,traj_se21)

traj_se_sel=subset(traj_se,traj_se$hour.inc>-73)

traj_se_sel
traj_se_sel$local=traj_se_sel$date+3600*8
traj_se_sel$day=as.Date(traj_se_sel$local-3600*10)

traj_se_sel2 <- inner_join(traj_se_sel, env_se[,-c(1:3,5)], by = "set")
traj_se_sel2

traj_se_n=traj_se_sel2
traj_se_n$d3=traj_se_n$date-3600*10
traj_se_n$dn=as.character(traj_se_n$day)

trajMap(traj_se_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

tiff(paste0("Figure/230615se_ctraj_n3","_angle.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
se_clust <- trajCluster(traj_se_n, 
                        #method = "Euclid",
                        method = "angle", 
                        n.cluster = 3, 
                        col = c("#BC3C29FF","#400082","#11AAFB"),
                        projection ="lambert",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude (°)",
                        ylab="Latitude (°)",
                        grid.col="transparent",
                        xlim=c(100,130),
                        ylim=c(33,53)
)
dev.off()

tiff("Figure/230615SE_ctraj_n3_nolabel.tiff", width = 15, height = 15,units = "cm",res = 300, compression = "lzw")
bjw_clust_nolabel <- trajCluster_ms(traj_se_n, 
                                    method = "angle", 
                                    n.cluster = 3, 
                                    col = c("#BC3C29FF","#400082","#11AAFB"),
                                    projection ="albers",
                                    orientation = c(0,45,0),
                                    map.cols ="grey45",
                                    xlab="Longitude (°)",
                                    ylab="Latitude (°)",
                                    xlim=c(90,130),
                                    ylim=c(33,55),
                                    #grid.col="black",
                                    grid.col="transparent",
                                    lwd = 6,
                                    key=F
)
dev.off()


#6. cluster dendrogram=====
###ub=====
traj_ub_c=filter(ub_clust$data$traj, hour.inc == 0)
traj_ub_c$d3
traj_ub_c$Freq=1
traj_ub_c$newd=as.factor(format(traj_ub_c$day, "%b-%d"))

data_dend_ub=traj_ub_c[,c("Freq","cluster","newd")]
data_dend_ub2=dcast(data_dend_ub,newd~cluster,sum, value.var = "Freq")
data_dend_ub2=data_dend_ub2 %>% `row.names<-`(data_dend_ub2$newd)

dend_ub <- as.dendrogram(hclust(dist(data_dend_ub2)))

p1 <- ggplot(dend_ub, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"))
p1

traj_ub_c$n2 <- factor(traj_ub_c$newd, levels = labels(dend_ub))
dend_ub
traj_ub_c

tub=unique(traj_ub_c[,c("n2","Event")])
tub$col <- ifelse(tub$Event == "Event", "red",
                  ifelse(tub$Event == "Normal", "black","black"))

ub_nv=data.frame(n2=labels(dend_ub))
ub_nv$n=row.names(ub_nv)
ub_nv=ub_nv %>% inner_join(tub)
ub_nv
ub_cs=as.vector((ub_nv$col))

dend_ub <- data_dend_ub2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80")

p1 <- ggplot(dend_ub, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_ub_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values = c("#FF5D00","#EBA91A","#638C80"))+
  scale_color_manual(values =c("#FF5D00","#EBA91A","#638C80"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = ub_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("ub_traj_trajc_cluster_fin"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")

###bj=====
traj_bj_c=filter(bj_clust$data$traj, hour.inc == 0)
traj_bj_c$d3
traj_bj_c$Freq=1
traj_bj_c$newd=as.factor(format(traj_bj_c$day, "%b-%d"))

data_dend_bj=traj_bj_c[,c("Freq","cluster","newd")]


data_dend_bj2=dcast(data_dend_bj,newd~cluster,sum, value.var = "Freq")
data_dend_bj2=data_dend_bj2 %>% `row.names<-`(data_dend_bj2$newd)

dend_bj <- as.dendrogram(hclust(dist(data_dend_bj2)))
p1 <- ggplot(dend_bj, horiz = T)+
  scale_x_continuous(expand = c(0.07, 0.07))+
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"))
p1

traj_bj_c$n2 <- factor(traj_bj_c$newd, levels = labels(dend_bj))
traj_bj_c$n2
traj_bj_c$event

traj_bj_sel2$Event


tbj=unique(traj_bj_c[,c("n2","Event")])
tbj$col <- ifelse(tbj$Event == "Event", "red",
                  ifelse(tbj$Event == "Normal", "black","black"))

bj_nv=data.frame(n2=labels(dend_bj))
bj_nv$n=row.names(bj_nv)
bj_nv=bj_nv %>% inner_join(tbj)
bj_nv
bj_cs=as.vector((bj_nv$col))

dend_bj <- data_dend_bj2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))


p1 <- ggplot(dend_bj, horiz = T)+
  scale_x_continuous(expand = c(0.065, 0.065))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_bj_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bjw_c$n2)))+
  scale_fill_manual(values = c("#7868E6","royalblue4","#562B08"))+
  scale_color_manual(values =c("#7868E6","royalblue4","#562B08"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = bj_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("bj_traj_trajc_cluster_fin"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")


###se=====
traj_se_c=filter(se_clust$data$traj, hour.inc == 0)
traj_se_c$d3
traj_se_c$Freq=1
traj_se_c$newd=as.factor(format(traj_se_c$day, "%b-%d"))

data_dend_se=traj_se_c[,c("Freq","cluster","newd")]
data_dend_se2=dcast(data_dend_se,newd~cluster,sum, value.var = "Freq")
data_dend_se2=data_dend_se2 %>% `row.names<-`(data_dend_se2$newd)

dend_se <- as.dendrogram(hclust(dist(data_dend_se2)))
p1 <- ggplot(dend_se, horiz = T)+
  scale_x_continuous(expand = c(0.09, 0.09))+
  theme(plot.margin = unit(c(0.2,-0.2,0.2,0.2),"cm"))
p1

traj_se_c$n2 <- factor(traj_se_c$newd, levels = labels(dend_se))
traj_se_c$n2
traj_se_c$event

traj_se_sel2$Event


tse=unique(traj_se_c[,c("n2","Event")])
tse$col <- ifelse(tse$Event == "Event", "red",
                  ifelse(tse$Event == "Normal", "black","black"))

se_nv=data.frame(n2=labels(dend_se))
se_nv$n=row.names(se_nv)
se_nv=se_nv %>% inner_join(tse)
se_nv
se_cs=as.vector((se_nv$col))

dend_se <- data_dend_se2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))

p1 <- ggplot(dend_se, horiz = T)+
  scale_x_continuous(expand = c(0.09, 0.09))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_se_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(reverse = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_sew_c$n2)))+
  scale_fill_manual(values = c("#BC3C29FF","#400082","#11AAFB"))+
  scale_color_manual(values =c("#BC3C29FF","#400082","#11AAFB"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = se_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("se_traj_trajc_cluster_fin"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")



###nt=====
traj_nt_c=filter(nt_clust$data$traj, hour.inc == 0)
traj_nt_c$d3
traj_nt_c$Freq=1
traj_nt_c$newd=as.factor(format(traj_nt_c$day, "%b-%d"))

data_dend_nt=traj_nt_c[,c("Freq","cluster","newd")]
data_dend_nt2=dcast(data_dend_nt,newd~cluster,sum, value.var = "Freq")
data_dend_nt2=data_dend_nt2 %>% `row.names<-`(data_dend_nt2$newd)

dend_nt <- as.dendrogram(hclust(dist(data_dend_nt2)))

p1 <- ggplot(dend_nt, horiz = T)+
  scale_x_continuous(expand = c(0.08, 0.08))+
  theme(plot.margin = unit(c(0.2,-0.2,0.2,0.2),"cm"))
p1

traj_nt_c$n2 <- factor(traj_nt_c$newd, levels = labels(dend_nt))
traj_nt_c$n2
traj_nt_c$event
traj_nt_ntl2$Event


tnt=unique(traj_nt_c[,c("n2","Event")])
tnt$col <- ifelse(tnt$Event == "Event", "red",
                  ifelse(tnt$Event == "Normal", "black","black"))

nt_nv=data.frame(n2=labels(dend_nt))
nt_nv$n=row.names(nt_nv)
nt_nv=nt_nv %>% inner_join(tnt)
nt_nv
nt_cs=as.vector((nt_nv$col))

dend_nt <- data_dend_nt2 %>% dist %>% hclust %>% as.dendrogram %>% set("labels_cex", c(0))


p1 <- ggplot(dend_nt, horiz = T)+
  scale_x_continuous(expand = c(0.09, 0.09))+
  theme(plot.margin = unit(c(0.2,-3.2,0.2,0.2),"cm"))

p2 <- ggplot(traj_nt_c, aes(x=n2, y=Freq, fill=cluster),colour=NA,alpha=0.7)+
  geom_bar(stat = "identity", position = position_fill(revernt = T), alpha=0.8)+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_ntw_c$n2)))+
  scale_fill_manual(values = c("#BC3C29FF","#400082","#11AAFB"))+
  scale_color_manual(values =c("#BC3C29FF","#400082","#11AAFB"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=22, face="bold", color = "black"),
        #axis.text.y = element_text(size=14,face="bold",color = "black"),
        axis.text.y = element_text(size=26,face="bold",color = nt_cs),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")+
  ggsave(filename("nt_traj_trajc_cluster_fin"),height = 30, width = 40, units = "cm", dpi = 300, compression="lzw")


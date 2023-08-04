env_2d=fread("Datafile/2DGC_envi.csv")
env_2d$day=as.Date(env_2d$date)

traj_se20 <- readRDS("Datafile/traj/TrajData_se_2020.rds")
traj_se21 <- readRDS("Datafile/traj/TrajData_se_2021.rds")

env_se=subset(env_2d,env_2d$group=="Seoul")
traj_se=rbind(traj_se20,traj_se21)

traj_se
traj_se_sel=subset(traj_se,traj_se$hour.inc>-73)

traj_se_sel$local=traj_se_sel$date+3600*8
traj_se_sel$day=as.Date(traj_se_sel$local-3600*10)

traj_se2_sel <- inner_join(traj_se_sel, env_se[,-c(1:3,5)], by = "day")
traj_se2_sel

trajPlot(traj_se2_sel,
         group = "day",
         col = "turbo", lwd = 2)

traj_se_n=traj_se2_sel
traj_se_n$d3=traj_se_n$date-3600*10
traj_se_n$dn=as.character(traj_se_n$day)

mycol2=c("#BC3C29FF","#0072B5FF","#E18727FF","#EE4C97FF","royalblue4","#65463E","#704F70","#C03450","grey54")

trajMap(traj_se_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

mycol_c=c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70")

se_clust <- trajCluster(traj_se_n, method = "Euclid", 
                        n.cluster = 5, 
                        col = c("#BC3C29FF","royalblue4","#E18727FF","#65463E"),
                        projection ="lambert",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude",
                        ylab="Latitude",
                        xlim=c(95,130),
                        ylim=c(33,55)
)
se_clust

tiff("Figure/se_ctraj.tiff", width = 22, height = 20,units = "cm",res = 300, compression = "lzw")
se_clust <- trajCluster(traj_se_n, method = "Euclid", 
                        n.cluster = 4, 
                        col = c("#BC3C29FF","royalblue4","#E18727FF","#65463E"),
                        projection ="lambert",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude",
                        ylab="Latitude",
                        xlim=c(95,130),
                        ylim=c(33,55)
                        )
dev.off()




traj_se_c=filter(se_clust$data$traj, hour.inc == 0)
traj_se_c$d3
is.Date(traj_se_c$day)
is.Date(traj_se_c$date)
traj_se_c$Freq=1
traj_se_c$newd=as.factor(format(traj_se_c$day, "%b-%d"))


ggplot(traj_se_c, aes(x=newd, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  scale_x_discrete(limits = rev(levels(traj_se_c$newd)))+
  scale_fill_manual(values =c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70"))+
  scale_color_manual(values =c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=16, face="bold", color = "black", margin = unit(c(0.6,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "top"
        )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("Se_traj_cluster"),height = 30, width = 20, units = "cm", dpi = 300, compression="lzw")


data_dend_se=traj_se_c[,c("Freq","cluster","newd")]
data_dend_se2=dcast(data_dend_se,newd~cluster,sum, value.var = "Freq")
data_dend_se2=data_dend_se2 %>% `row.names<-`(data_dend_se2$newd)

dend_se <- as.dendrogram(hclust(dist(data_dend_se2)))

traj_se_c$n2 <- factor(traj_se_c$newd, levels = labels(dend_se))

p1 <- ggplot(dend_se, horiz = T)
p2 <- ggplot(traj_se_c, aes(x=n2, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_se_c$n2)))+
  scale_fill_manual(values =c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70"))+
  scale_color_manual(values =c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")



traj_se_c=filter(se_clust$data$traj, hour.inc == 0)
traj_se_c$newd=as.factor(format(traj_se_c$day, "%b-%d"))

traj_se_c

ggplot(traj_se_c, aes(x=newd, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  scale_x_discrete(limits = rev(levels(traj_se_c$newd)))+
  scale_fill_manual(values =c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70"))+
  scale_color_manual(values =c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=16, face="bold", color = "black", margin = unit(c(0.6,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "top"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("Se_traj_cluster"),height = 30, width = 20, units = "cm", dpi = 300, compression="lzw")

##trajactory Beijing=====

traj_bj20 <- readRDS("Datafile/traj/TrajData_bj_2020.rds")
traj_bj21 <- readRDS("Datafile/traj/TrajData_bj_2021.rds")

env_bj=subset(env_2d,env_2d$group=="Beijing")
traj_bj=rbind(traj_bj20,traj_bj21)

traj_bj_sel=subset(traj_bj,traj_bj$hour.inc>-73)

traj_bj_sel
traj_bj_sel$local=traj_bj_sel$date+3600*8
traj_bj_sel$day=as.Date(traj_bj_sel$local-3600*10)

traj_bj_sel2 <- inner_join(traj_bj_sel, env_bj[,-c(1:3,5)], by = "day")
traj_bj_sel2

trajPlot(traj_bj_sel2,
         group = "day",
         col = "turbo", lwd = 2)

traj_bj_n=traj_bj_sel2
traj_bj_n$d3=traj_bj_n$date-3600*10
traj_bj_n$dn=as.character(traj_bj_n$day)

mycol2=c("#BC3C29FF","#0072B5FF","#E18727FF","#EE4C97FF","royalblue4","#65463E","#704F70","#C03450","grey54")

trajMap(traj_bj_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

mycol_c=c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70")

bj_clust <- trajCluster(traj_bj_n, method = "Euclid", 
                        n.cluster = 5, 
                        col = c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70"),
                        projection ="lambert",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude",
                        ylab="Latitude",
                        xlim=c(90,130),
                        ylim=c(33,55)
)


tiff("Figure/0217bj_ctraj.tiff", width = 22, height = 20,units = "cm",res = 300, compression = "lzw")
bj_clust <- trajCluster(traj_bj_n, method = "angle", 
                        n.cluster = 4, 
                        col = c("royalblue4","#65463E","#E18727FF","#BC3C29FF"),
                        projection ="lambert",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude",
                        ylab="Latitude",
                        xlim=c(80,130),
                        ylim=c(33,55)
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

traj_bj_c$n2 <- factor(traj_bj_c$newd, levels = labels(dend_bj))

p1 <- ggplot(dend_bj, horiz = T)
p2 <- ggplot(traj_bj_c, aes(x=n2, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_bj_c$n2)))+
  scale_fill_manual(values =c("royalblue4","#65463E","#E18727FF","#BC3C29FF"))+
  scale_color_manual(values =c("royalblue4","#65463E","#E18727FF","#BC3C29FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")


ggplot(traj_bj_c, aes(x=newd, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  scale_x_discrete(limits = rev(levels(traj_bj_c$newd)))+
  scale_fill_manual(values =c("royalblue4","#65463E","#E18727FF","#BC3C29FF"))+
  scale_color_manual(values =c("royalblue4","#65463E","#E18727FF","#BC3C29FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=16, face="bold", color = "black", margin = unit(c(0.6,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "top"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("bj_traj_cluster"),height = 30, width = 20, units = "cm", dpi = 300, compression="lzw")


##trajactory Ulaan=====
traj_ub20 <- readRDS("Datafile/traj/TrajData_ub_2020.rds")
traj_ub21 <- readRDS("Datafile/traj/TrajData_ub_2021.rds")

env_ub=subset(env_2d,env_2d$group=="Ulaanbaatar")
traj_ub=rbind(traj_ub20,traj_ub21)


traj_ub_sel=subset(traj_ub,traj_ub$hour.inc>-73)

traj_ub_sel
traj_ub_sel$local=traj_ub_sel$date+3600*8
traj_ub_sel$day=as.Date(traj_ub_sel$local-3600*10)

traj_ub_sel2 <- inner_join(traj_ub_sel, env_ub[,-c(1:3,5)], by = "day")
traj_ub_sel2

trajPlot(traj_ub_sel,
         group = "day",
         col = "turbo", lwd = 2)

traj_ub_n=traj_ub_sel2
traj_ub_n$d3=traj_ub_n$date-3600*10
traj_ub_n$dn=as.character(traj_ub_n$day)

mycol2=c("#BC3C29FF","#0072B5FF","#E18727FF","#EE4C97FF","royalblue4","#65463E","#704F70","#C03450","grey54")

trajMap(traj_ub_n, 
        colour  = "dn",
        cols = mycol2,
        control = "dn",
        provider = "Stamen.Terrain"
)

mycol_c=c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70")

ub_clust <- trajCluster(traj_ub_n, method = "Euclid", 
                        n.cluster = 3, 
                        col = c("#BC3C29FF","royalblue4","#E18727FF","#65463E"),
                        projection ="lambert",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude",
                        ylab="Latitude",
                        xlim=c(90,130),
                        ylim=c(33,55)
)


tiff("Figure/230217ub_ctraj.tiff", width = 22, height = 20,units = "cm",res = 300, compression = "lzw")
ub_clust <- trajCluster(traj_ub_n, method = "angle", 
                        n.cluster = 4, 
                        col = c("#65463E","royalblue4","#BC3C29FF","#E18727FF"),
                        projection ="lambert",
                        orientation = c(0,45,0),
                        map.cols ="grey45",
                        xlab="Longitude",
                        ylab="Latitude",
                        xlim=c(80,130),
                        ylim=c(33,55)
)
dev.off()


traj_ub_c=filter(ub_clust$data$traj, hour.inc == 0)
traj_ub_c$d3
traj_ub_c$Freq=1
traj_ub_c$newd=as.factor(format(traj_ub_c$day, "%b-%d"))

data_dend_ub=traj_ub_c[,c("Freq","cluster","newd")]
data_dend_ub2=dcast(data_dend_ub,newd~cluster,sum, value.var = "Freq")
data_dend_ub2=data_dend_ub2 %>% `row.names<-`(data_dend_ub2$newd)

dend_ub <- as.dendrogram(hclust(dist(data_dend_ub2)))
traj_ub_c$n2 <- factor(traj_ub_c$newd, levels = labels(dend_ub))

p1 <- ggplot(dend_ub, horiz = T)
p2 <- ggplot(traj_ub_c, aes(x=n2, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  #scale_x_discrete(limits = rev(levels(traj_ub_c$n2)))+
  scale_fill_manual(values =c("#65463E","royalblue4","#BC3C29FF","#E18727FF"))+
  scale_color_manual(values =c("#65463E","royalblue4","#BC3C29FF","#E18727FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=0.1, face="bold", color = "black", margin = unit(c(0.2,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "NULL"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)

plot_grid(p1, p2, align = "h")


ggplot(traj_ub_c, aes(x=newd, y=Freq, fill=cluster, colour=cluster))+
  geom_bar(stat = "identity", position = position_fill())+
  scale_y_continuous(name="Proportion of cluster (%)",labels = scales::percent, expand=c(0.01,0.01))+
  scale_x_discrete(limits = rev(levels(traj_ub_c$newd)))+
  scale_fill_manual(values =c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70"))+
  scale_color_manual(values =c("#BC3C29FF","royalblue4","#E18727FF","#65463E","#704F70"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(size = 1),
        plot.margin = unit(c(0.2,1.2,0.2,0.2),"cm"),
        axis.title.y = element_text(size=0.1),
        axis.title.x = element_text(size=16, face="bold", color = "black", margin = unit(c(0.6,0.2,0.0,0.2),"cm")),
        axis.text.x = element_text(size=14, face="bold", color = "black"),
        axis.text.y = element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(size=16,face="bold",color = "black"),
        legend.text = element_text(size=14,face="bold",color = "black"),
        legend.position = "top"
  )+
  coord_flip()+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("ub_traj_cluster"),height = 30, width = 20, units = "cm", dpi = 300, compression="lzw")




#counting site-specific cluster=====
cnt_cl=fread("Datafile/sitePAH_cluster.csv")

cnt_cl_m=melt(cnt_cl[,c("Group","New name","C1","C2","C3","C4","ord")], id.vars = c("Group","New name","ord"))

cnt_cl_m$Group=factor(cnt_cl_m$Group, levels = c("UT","BJ","SS","SE"))

cnt_cl_m2=cnt_cl_m[order(cnt_cl_m$ord),]
unique(cnt_cl_m2$`New name`)

cnt_cl_m2$varlab=factor(cnt_cl_m2$`New name`, levels = c("benzo[b]naphtho[2,3-d]thiophene, 6,8-dimethyl-","2,7-dimethyldibenzothiophene",
                                                         "1,4,5,8-tetramethylnaphthalene","diphenyl sulfide","naphthalene, 7-butyl-1-hexyl-" ,
                                                         "6h-benz[de]anthracen-6-one","8h-indeno[2,1-b]phenanthrene",
                                                         "1h-cyclobuta[a]naphthalen-2-one","methyl 1,2,3,4-tetrahydro-7-isopropyl-1β-methyl-phenanthrene-1-carboxylate",
                                                         "1,2-dihydrocyclobuta[b]anthracene", "11h-benzo[a]fluoren-11-one, 10-methyl-",
                                                         "4h-cyclopenta[def]chrysen-4-one","benzo[2,1-b:3,4-b']bisbenzofuran"
                                                         ))


ggplot(cnt_cl_m2, aes(x=`varlab`,y=value, fill=variable))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values =c("#BC3C29FF","royalblue4","#E18727FF","#65463E"))+
  facet_rep_wrap(.~Group, scales = "free")+
  scale_fill_manual(values =c("royalblue4","#65463E","#E18727FF","#BC3C29FF"))+
  scale_color_manual(values =c("royalblue4","#65463E","#E18727FF","#BC3C29FF"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.placement = "outside",
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 22, face = "bold",margin = unit(c(0.3,0.2,0.2,0.2),"cm")),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(1.2,0.4,0.2,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.text.x = element_text(size = 8,angle = 315,vjust = 0.5, hjust = 0.5,margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
                                   colour = "black", face = "bold",family = "Arial"),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks = element_line(size = 1.5, colour = "black"),
        axis.title.x = element_text(size = 0.1,colour = "black", face = "bold",family = "Arial"),
        axis.text.y = element_text(size = 16, colour = "black" , face = "bold",family = "Arial",  margin = unit(c(0.2,0.2,0.2,0.2),"cm")),
        axis.title.y.left = element_text(size = 0.1, colour = "black", face = "bold",
                                         family = "Arial"),
        legend.text = element_text(size = 18, colour = "black",family = "Arial",margin = unit(c(0.2,0.1,0.2,0.1),"cm"), hjust = 0.5),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"),face=2, size = 20,family = "Arial"),
        legend.box.background = element_rect(color=NA,size=0.5, fill=NA),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(1.0,"cm"),
        legend.direction = "vertical",
        legend.background = element_blank(),
        #legend.position = c(0.08,1.02)
        legend.position =c(0.94,0.89))+
  guides(fill=guide_legend(title = "Cluster"), col=F)+
  ggsave(filename("PAH_cluster_leg"),height = 30, width = 30, units = "cm", dpi = 300, compression="lzw")

data_dend_ub2$Group="Ulaanbaatar"
data_dend_bj2$Group="Beijing"
data_dend_ss2$Group="Seosan"
data_dend_se2$Group="Seoul"

cluster_day=rbind(data_dend_ub2,data_dend_bj2,data_dend_ss2,data_dend_se2)
fwrite(cluster_day,file = "Datafile/clusterbyday.csv")



##cluster============
traj_ub_n
traj_se_n
traj_bj_n

?trajCluster()

for (i in 2:5) {
 # i=5
  tiff(paste0("Figure/ul_ctraj_n",i,"_Euclid.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  #tiff(paste0("Figure/ul_ctraj_n",i,"_angle.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  ul_clust <- trajCluster(traj_ub_n, 
                           method = "Euclid",
                           #method = "angle", 
                           n.cluster = i, 
                           col = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
                           projection ="lambert",
                           orientation = c(0,30,0),
                           map.cols ="grey45",
                           xlab="Longitude (°)",
                           ylab="Latitude (°)",
                          grid.col="transparent",
                           xlim=c(75,110),
                           ylim=c(45,55)
  )
  dev.off()
  
  tiff(paste0("Figure/bj_ctraj_n",i,"_Euclid.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  #tiff(paste0("Figure/bj_ctraj_n",i,"_angle.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  bj_clust <- trajCluster(traj_bj_n, 
                           method = "Euclid",
                           #method = "angle", 
                           n.cluster = i, 
                           col = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
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
  
  tiff(paste0("Figure/se_ctraj_n",i,"_Euclid.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  #tiff(paste0("Figure/se_ctraj_n",i,"_angle.tiff"), width = 17, height = 15,units = "cm",res = 300, compression = "lzw")
  se_clust <- trajCluster(traj_se_n, 
                           method = "Euclid",
                           #method = "angle", 
                           n.cluster = i, 
                           col = c("#FF5D00","royalblue4","#EBA91A","#7868E6","#638C80"),
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
  
}

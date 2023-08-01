

tools::file_path_sans_ext(flist[i])

flist=dir("Datafile/raw/")

dt=data.table()

for (i in 1:length(flist)) {
  
  #i=1
  print(i)
  
  temp=read.xlsx2(file = paste0("Datafile/raw/",flist[i]),sheetIndex = 1)
  temp$Sample=tools::file_path_sans_ext(flist[i])

  dt=rbind(dt,temp)

}

dt
table(dt$Sample)

sem_merge=dt

fwrite(dt, file = "sem_merge.csv")

sem_merge_raw=fread("Datafile/sem_merge.csv")

sem_merge=sem_merge_raw
sem_merge$Freq=1

sem_merge$grouplab=factor(sem_merge$Group, levels = c("Ulanbatar","Beijing","Seosan","Seoul","Noto"),
                          labels = c("UT","BJ","SS","SE","NT"))

grp.mean=aggregate(sem_merge$Size, by=list(grouplab=sem_merge$grouplab, Event=sem_merge$Event),mean)

grp.mean_mor_freq=aggregate(sem_merge$Freq, by=list(grouplab=sem_merge$grouplab, Event=sem_merge$Event,Mor=sem_merge$Morphology),sum)
grp.mean_mor_tot=aggregate(sem_merge$Freq, by=list(grouplab=sem_merge$grouplab, Event=sem_merge$Event),sum) %>% `colnames<-`(c("grouplab","Event","Tot"))

grp.mean_mor_freq=grp.mean_mor_freq %>% left_join(grp.mean_mor_tot)
grp.mean_mor_freq$rel=grp.mean_mor_freq$x/grp.mean_mor_freq$Tot*100

grp.mean_mor=aggregate(sem_merge$Size, by=list(grouplab=sem_merge$grouplab, Event=sem_merge$Event,Mor=sem_merge$Morphology),mean) %>% 
  dcast(grouplab+Event~Mor, mean,value.var = "x")

grp.sd_mor=aggregate(sem_merge$Size, by=list(grouplab=sem_merge$grouplab, Event=sem_merge$Event,Mor=sem_merge$Morphology),sd) %>% 
  dcast(grouplab+Event~Mor, mean,value.var = "x")

grp.mean=aggregate(sem_merge$Size, by=list(grouplab=sem_merge$grouplab, Event=sem_merge$Event),mean)%>% `colnames<-`(c("grouplab","Event","Totmean"))
grp.sd=aggregate(sem_merge$Size, by=list(grouplab=sem_merge$grouplab, Event=sem_merge$Event),sd)%>% `colnames<-`(c("grouplab","Event","Totsd"))



ggplot(sem_merge, aes(x=Size, fill=Morphology))+
  facet_rep_wrap(Event~grouplab, scales = "free_y", nrow = 2,repeat.tick.labels = T)+
  scale_fill_manual(values = c( "#3C5488FF","#E64B35FF","#00A087FF","#4DBBD5FF"))+
  scale_y_continuous(name = "Frequency", expand = c(0.04,0.04))+
  scale_x_continuous(name = expression(bold("Particle size"~"("*"\u03bcm"*")")),
                     breaks = seq(1,9,2))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = NA, colour = "NA"),
        strip.text = element_text(colour = "black", size = 16, face = "bold",margin = unit(c(0.1,0.2,0.1,0.2),"cm"), hjust = 0.5),
        plot.title= element_text(size = 24, colour = "black", face="bold",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),family = "Arial", hjust = 0.5),
        plot.margin = unit(c(0.2,0.2,0.0,0.2),"cm"),
        panel.border = element_rect(size = 2, colour = "black"),
        axis.text.x = element_text(size = 16,angle = 0,colour = "black",family = "Arial", vjust = 0.5, hjust = 0.5),
        axis.ticks.length = unit(0.2,"cm"),
        axis.ticks = element_line(size = 1.5, colour = "black"),
        axis.title.x = element_text(size = 20, colour = "black",margin = unit(c(0.4,0.1,0.1,0.1),"cm"),face = "bold",family = "Arial"),
        axis.text.y = element_text(size = 16, colour = "black" ,margin = unit(c(0.1,0.1,0.1,0.2),"cm"),family = "Arial"),
        axis.title.y.left = element_text(size = 20, colour = "black",margin = unit(c(0.1,0.1,0.1,0.1),"cm"),face = "bold",family = "Arial"),
        legend.text = element_text(size = 14, colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.2,0.1),"cm"), hjust = 0.0),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 14,family = "Arial"),
        legend.box.background = element_blank(),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "vertical",
        legend.background = element_blank(),
        #legend.position = c(0.08,1.02)
        legend.position = c(0.95,0.30))+
  xlab("")+
  guides(fill=guide_legend(order = 1,title ="Morphology",col=c(NA,NA,NA), linetype=c(1,1,1), alpha=0.4),
         col=F)+
  ggsave(filename("SEM_particle_dist2"), height = 20, width = 45, units = "cm", dpi = 300, compression="lzw")

##donut=====

tt=c("#475657","#C69749", "#8B7E74", "#744C4C","#C7BCA1")


##UT=====
sem_merge_ul=subset(sem_merge,sem_merge$grouplab=="UT")
sem_merge_ul_ev=subset(sem_merge_ul,sem_merge_ul$Event=="Event")
sem_merge_ul_nev=subset(sem_merge_ul,sem_merge_ul$Event=="Non-event")


png("Figure/230531_ul_pie_ev.png", width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ul_ev,aes(Morphology ,Type_d ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",pielwd = 1.3, donutlwd = 1.3,pietitle = "UT",titlesize = 15,
            pieAlpha = 0.8, donutAlpha = 0.9,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol =  c("#FC4F00","#F9D949","#008000","#000080"),
            subfill = "default")
dev.off()

png("Figure/230531_ul_pie_nev.png", width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ul_nev,aes(Morphology ,Type_d ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",pielwd = 1.3, donutlwd = 1.3,pietitle = "UT",titlesize = 15,
            pieAlpha = 0.8, donutAlpha = 0.9,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol =  c("#FC4F00","#F9D949","#008000","#000080"),
            subfill = "default")
dev.off()
  
##BJ=====
sem_merge_bj=subset(sem_merge,sem_merge$grouplab=="BJ")
sem_merge_bj_ev=subset(sem_merge_bj,sem_merge_bj$Event=="Event")
sem_merge_bj_nev=subset(sem_merge_bj,sem_merge_bj$Event=="Non-event")


png("Figure/230531_bj_pie_ev.png", width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_bj_ev,aes(Morphology ,Type_d ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",pielwd = 1.3, donutlwd = 1.3,pietitle = "BJ",titlesize = 15,
            pieAlpha = 0.8, donutAlpha = 0.9,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol =  c("#FC4F00","#F9D949","#008000","#000080"),
            subfill = "default")
dev.off()

png("Figure/230531_bj_pie_nev.png", width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_bj_nev,aes(Morphology ,Type_d ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",pielwd = 1.3, donutlwd = 1.3,pietitle = "BJ",titlesize = 15,
            pieAlpha = 0.8, donutAlpha = 0.9,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol =  c("#F9D949","#008000","#000080"),
            subfill = "default")
dev.off()


##SS=====
sem_merge_ss=subset(sem_merge,sem_merge$grouplab=="SS")
sem_merge_ss_ev=subset(sem_merge_ss,sem_merge_ss$Event=="Event")
sem_merge_ss_nev=subset(sem_merge_ss,sem_merge_ss$Event=="Non-event")


png("Figure/230531_ss_pie_ev.png", width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ss_ev,aes(Morphology ,Type_d), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",pielwd = 1.3, donutlwd = 1.3,pietitle = "SS",titlesize = 15,
            pieAlpha = 0.8, donutAlpha = 0.9,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol =  c("#FC4F00","#F9D949","#008000","#000080"),
            subfill = "default")
dev.off()

png("Figure/230531_ss_pie_nev.png", width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ss_nev,aes(Morphology ,Type_d ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",pielwd = 1.3, donutlwd = 1.3,pietitle = "SS",titlesize = 15,
            pieAlpha = 0.8, donutAlpha = 0.9,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol =  c("#F9D949","#008000","#000080","#800080"),
            subfill = "default")
dev.off()

##se=====
sem_merge_se=subset(sem_merge,sem_merge$grouplab=="SE")
sem_merge_se_ev=subset(sem_merge_se,sem_merge_se$Event=="Event")
sem_merge_se_nev=subset(sem_merge_se,sem_merge_se$Event=="Non-event")


png("Figure/230531_se_pie_ev.png", width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_se_ev,aes(Morphology ,Type_d), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",pielwd = 1.3, donutlwd = 1.3,pietitle = "SE",titlesize = 15,
            pieAlpha = 0.8, donutAlpha = 0.9,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol =  c("#FC4F00","#F9D949","#008000","#000080","#800080"),
            subfill = "default")
dev.off()

png("Figure/230531_se_pie_nev.png", width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_se_nev,aes(Morphology ,Type_d ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",pielwd = 1.3, donutlwd = 1.3,pietitle = "SE",titlesize = 15,
            pieAlpha = 0.8, donutAlpha = 0.9,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol =  c("#FC4F00","#F9D949","#008000","#000080","#800080"),
            subfill = "default")
dev.off()


##se=====
sem_merge_nt=subset(sem_merge,sem_merge$grouplab=="NT")
sem_merge_nt_ev=subset(sem_merge_nt,sem_merge_nt$Event=="Event")
sem_merge_nt_nev=subset(sem_merge_nt,sem_merge_nt$Event=="Non-event")


png("Figure/230531_nt_pie_ev.png", width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_nt_ev,aes(Morphology ,Type_d), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",pielwd = 1.3, donutlwd = 1.3,pietitle = "NT",titlesize = 15,
            pieAlpha = 0.8, donutAlpha = 0.9,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol =  c("#FC4F00","#F9D949","#008000","#000080"),
            subfill = "default")
dev.off()

png("Figure/230531_nt_pie_nev.png", width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_nt_nev,aes(Morphology ,Type_d ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",pielwd = 1.3, donutlwd = 1.3,pietitle = "NT",titlesize = 15,
            pieAlpha = 0.8, donutAlpha = 0.9,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol =  c("#F9D949","#008000","#000080"),
            subfill = "default")
dev.off()




library(tidyverse)
library(data.table)
library(lemon)
library(ggforce)
require(moonBook)
require(webr)
library(grid)

filepng <- function(filename){
  # check that ftmsObj is of the correct class #
  
  filename=paste0("Figure/",filename,"_",format(Sys.time(), "%Y_%m_%d"),".png")
  
  return(filename)
}


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

grp.mean=aggregate(sem_merge$Size, by=list(grouplab=sem_merge$grouplab, Event=sem_merge$Event),mean)%>% `colnames<-`(c("grouplab","Event","Tot"))
grp.sd=aggregate(sem_merge$Size, by=list(grouplab=sem_merge$grouplab, Event=sem_merge$Event),sd)%>% `colnames<-`(c("grouplab","Event","Tot"))

grp.mean=grp.mean %>% left_join(grp.mean_mor)
grp.mean$stat="Mean"

grp.mean=grp.mean[order(grp.mean$grouplab),]

grp.sd=grp.sd %>% left_join(grp.sd_mor)
grp.sd$stat="sd"
grp.sd=grp.sd[order(grp.sd$grouplab),]

grp.stat=rbind(grp.mean,grp.sd)

#fwrite(grp.stat, file = "Datafile/Mor_stat.csv")
table(sem_merge$Morphology)

sem_merge$Morlab=factor(sem_merge$Morphology, levels = c("spherical","cluster","chainlike","irregular"),
                        labels = c("Spherical","Cluster","Chainlike","Irregular"))

ggplot(sem_merge, aes(x=Size, fill=Morlab))+
  geom_histogram(bin=40, color="black",alpha=0.8, position = position_stack(reverse = T))+
  facet_rep_wrap(Event~grouplab, scales = "free_y", nrow = 2,repeat.tick.labels = T)+
  scale_fill_manual(values = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"))+
  scale_y_continuous(name = "Frequency", expand = c(0.04,0.04))+
  scale_x_continuous(name = expression(bold("Particle size"~"("*"\u03bcm"*")")),
                     breaks = seq(1,9,2), 
                     limits = c(0,9.2),
                     expand = c(0.02,0.02)
                     )+
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
        legend.text = element_text(size = 14, colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.2,0.1),"cm"), hjust = 0.5),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 14,family = "Arial"),
        legend.box.background = element_blank(),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "vertical",
        legend.background = element_blank(),
        #legend.position = c(0.08,1.02)
        legend.position = c(0.95,0.30))+
  xlab("")+
  guides(fill=guide_legend(order = 1,title ="Morphology",col=c(NA,NA,NA), linetype=c(1,1,1), alpha=0.4, title.hjust = 0.5),
         col=F)+
  ggsave(filename("SEM_particle_dist2"), height = 20, width = 45, units = "cm", dpi = 300, compression="lzw")


#pie===
sem_merge

agg_1st=aggregate(sem_merge$Freq, by=list(grouplab=sem_merge$grouplab,Event=sem_merge$Event, Morlab=sem_merge$Morlab),sum)
agg_1st_tot=aggregate(sem_merge$Freq, by=list(grouplab=sem_merge$grouplab,Event=sem_merge$Event),sum) %>% `colnames<-`(c("grouplab","Event","Tot"))
agg_1st$Morlab=factor(agg_1st$Morlab, levels = c(),)

agg_1st=agg_1st[order(agg_1st$x),]
agg_1st=agg_1st %>% left_join(agg_1st_tot)
agg_1st$rel=round(agg_1st$x/agg_1st$Tot*100,1)

view(agg_1st)
ggplot()+
  geom_bar(data=agg_1st,aes(x="",y=x, fill=Morlab), col="white",stat="identity", size=1.5,
           position = position_fill(reverse = T),width = 1)+
  coord_polar("y")+
  scale_fill_manual(values = (c("#3C5488FF","#00A087FF","#E64B35FF","grey50")))+
  facet_rep_wrap(Event~grouplab, nrow = 2,repeat.tick.labels = T)+
  theme_bw()+
  theme(
    panel.background = element_rect(fill = "transparent", color = NA, size = 0), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA, size=0), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    strip.text = element_text(size = 28, family = "Arial",face = "bold"),
    legend.title = element_text(size = 16, vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
    legend.text = element_text(size = 12, colour = "black", family = "Arial",hjust = 0.5,margin = unit(c(0,0,0,0),"cm")),
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent", color = NA, size=0), # get rid of legend panel bg
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    panel.border = element_blank(),
    legend.position = "right",
    legend.direction = "vertical"
  )+
  guides(fill=guide_legend(reverse = T, title = ""))+
  ggsave("Figure/230704pie_pie_1st.png",height = 20, width = 45, units = "cm", dpi = 300)


##donut=====
tt=c("#6DA9E4","#884A39","#E8AA42","#CE5959","#7DB9B6","#DDDDDD")

##UT=====
sem_merge
table(sem_merge$Type2)

sem_merge$Type2=factor(sem_merge$Type2, levels = c("Carbonaceous","Mineral","N-rich","Fe-rich","Transition metal","Others"))

stat_type=aggregate(sem_merge$Freq, by=list(Grouplab=sem_merge$grouplab, Type=sem_merge$Type2),sum)
stat_type=stat_type[order(stat_type$x,decreasing = T),]

#UT:Carbonaceous, Mineral, Fe-rich, N-rich, Transition metal, Calcium Sulfate
sem_merge_ul=subset(sem_merge,sem_merge$grouplab=="UT")
sem_merge_ul_ev=subset(sem_merge_ul,sem_merge_ul$Event=="Event")
sem_merge_ul_nev=subset(sem_merge_ul,sem_merge_ul$Event=="Non-event")

png(filepng("ul_pie_ev"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ul_ev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "UT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ul_pie_ev_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ul_ev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "UT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ul_pie_nev"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ul_nev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "UT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ul_pie_nev_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ul_nev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "UT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()


##BJ=====
sem_merge_bj=subset(sem_merge,sem_merge$grouplab=="BJ")
sem_merge_bj_ev=subset(sem_merge_bj,sem_merge_bj$Event=="Event")
sem_merge_bj_nev=subset(sem_merge_bj,sem_merge_bj$Event=="Non-event")

png(filepng("bj_pie_ev"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_bj_ev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "BJ",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("bj_pie_ev_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_bj_ev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "BJ",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("bj_pie_nev"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_bj_nev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "BJ",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("bj_pie_nev_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_bj_nev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "BJ",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

##SS=====
sem_merge_ss=subset(sem_merge,sem_merge$grouplab=="SS")
sem_merge_ss_ev=subset(sem_merge_ss,sem_merge_ss$Event=="Event")
sem_merge_ss_nev=subset(sem_merge_ss,sem_merge_ss$Event=="Non-event")

png(filepng("ss_pie_ev"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ss_ev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SS",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ss_pie_ev_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ss_ev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SS",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ss_pie_nev"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ss_nev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SS",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ss_pie_nev_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_ss_nev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SS",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

##SE=====
sem_merge_se=subset(sem_merge,sem_merge$grouplab=="SE")
sem_merge_se_ev=subset(sem_merge_se,sem_merge_se$Event=="Event")
sem_merge_se_nev=subset(sem_merge_se,sem_merge_se$Event=="Non-event")

png(filepng("se_pie_ev"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_se_ev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SE",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("se_pie_ev_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_se_ev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SE",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("se_pie_nev"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_se_nev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SE",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("se_pie_nev_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_se_nev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SE",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()


##NT=====
sem_merge_nt=subset(sem_merge,sem_merge$grouplab=="NT")
sem_merge_nt_ev=subset(sem_merge_nt,sem_merge_nt$Event=="Event")
sem_merge_nt_nev=subset(sem_merge_nt,sem_merge_nt$Event=="Non-event")

png(filepng("nt_pie_ev"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_nt_ev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "NT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("nt_pie_ev_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_nt_ev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "NT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("nt_pie_nev"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_nt_nev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "NT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("nt_pie_nev_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_nt_nev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "NT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()


####2nd============
sem_merge_raw_2nd=fread("Datafile/sem_2nd_merge.csv")
sem_merge_2nd=sem_merge_raw_2nd
sem_merge_2nd$Freq=1

table(sem_merge_raw_2nd$Morphology)
sem_merge_2nd$grouplab=factor(sem_merge_2nd$Group, levels = c("UT","BJ","SS","SE","NT"),
                          labels = c("UT","BJ","SS","SE","NT"))

grp.mean_2nd=aggregate(sem_merge_2nd$Size, by=list(grouplab=sem_merge_2nd$grouplab, Event=sem_merge_2nd$Event),mean, na.rm=T)

grp.mean_mor_2nd_freq=aggregate(sem_merge_2nd$Freq, by=list(grouplab=sem_merge_2nd$grouplab, Event=sem_merge_2nd$Event,Mor=sem_merge_2nd$Morphology),sum)
grp.mean_mor_2nd_tot=aggregate(sem_merge_2nd$Freq, by=list(grouplab=sem_merge_2nd$grouplab, Event=sem_merge_2nd$Event),sum) %>% `colnames<-`(c("grouplab","Event","Tot"))

grp.mean_mor_2nd_freq=grp.mean_mor_2nd_freq %>% left_join(grp.mean_mor_2nd_tot)
grp.mean_mor_2nd_freq$rel=grp.mean_mor_2nd_freq$x/grp.mean_mor_2nd_freq$Tot*100


grp.mean_mor_2nd=aggregate(sem_merge_2nd$Size, by=list(grouplab=sem_merge_2nd$grouplab, Event=sem_merge_2nd$Event,Mor=sem_merge_2nd$Morphology),mean, na.rm=T) %>% 
  dcast(grouplab+Event~Mor, mean,value.var = "x")
grp.mean_mor_2nd

grp.sd_mor_2nd=aggregate(sem_merge_2nd$Size, by=list(grouplab=sem_merge_2nd$grouplab, Event=sem_merge_2nd$Event,Mor=sem_merge_2nd$Morphology),sd,na.rm=T) %>% 
  dcast(grouplab+Event~Mor, mean,value.var = "x")

grp.mean_2nd=aggregate(sem_merge_2nd$Size, by=list(grouplab=sem_merge_2nd$grouplab, Event=sem_merge_2nd$Event),mean, na.rm=T)%>% `colnames<-`(c("grouplab","Event","Tot"))
grp.sd_2nd=aggregate(sem_merge_2nd$Size, by=list(grouplab=sem_merge_2nd$grouplab, Event=sem_merge_2nd$Event),sd,na.rm=T)%>% `colnames<-`(c("grouplab","Event","Tot"))

grp.mean_2nd=grp.mean_2nd %>% left_join(grp.mean_mor_2nd)
grp.mean_2nd$stat="Mean"
grp.mean_2nd=grp.mean_2nd[order(grp.mean_2nd$grouplab),]


grp.sd_2nd=grp.sd_2nd %>% left_join(grp.sd_mor_2nd)
grp.sd_2nd$stat="sd"
grp.sd_2nd=grp.sd_2nd[order(grp.sd_2nd$grouplab),]

grp.stat_2nd=rbind(grp.mean_2nd,grp.sd_2nd)

#fwrite(grp.stat_2nd, file = "Datafile/Mor_stat_2nd.csv")
table(sem_merge_2nd$Morphology)

sem_merge_2nd$Morlab=factor(sem_merge_2nd$Morphology,
                        levels = c("Spherical","Cluster","Chainlike","Irregular"))

ggplot(sem_merge_2nd, aes(x=Size, fill=Morlab))+
  geom_histogram(bin=40, color="black",alpha=0.8, position = position_stack(reverse = T))+
  facet_rep_wrap(Event~grouplab, scales = "free_y", nrow = 2,repeat.tick.labels = T)+
  scale_fill_manual(values = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"))+
  scale_y_continuous(name = "Frequency", expand = c(0.04,0.04))+
  scale_x_continuous(name = expression(bold("Particle size"~"("*"\u03bcm"*")")),
                     breaks = seq(1,9,2),
                     limits = c(0,9.2),
                     expand = c(0.02,0.02)
                     )+
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
        legend.text = element_text(size = 14, colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.2,0.1),"cm"), hjust = 0.5),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 14,family = "Arial"),
        legend.box.background = element_blank(),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "vertical",
        legend.background = element_blank(),
        #legend.position = c(0.08,1.02)
        legend.position = c(0.95,0.30))+
  xlab("")+
  guides(fill=guide_legend(order = 1,title ="Morphology",col=c(NA,NA,NA), linetype=c(1,1,1), alpha=0.4, title.hjust = 0.5),
         col=F)+
  ggsave(filename("SEM_particle_dist2_2nd"), height = 20, width = 45, units = "cm", dpi = 300, compression="lzw")




agg_2nd=aggregate(sem_merge_2nd$Freq, by=list(grouplab=sem_merge_2nd$grouplab,Event=sem_merge_2nd$Event, Morlab=sem_merge_2nd$Morlab),sum)
agg_2nd_tot=aggregate(sem_merge_2nd$Freq, by=list(grouplab=sem_merge_2nd$grouplab,Event=sem_merge_2nd$Event),sum) %>% `colnames<-`(c("grouplab","Event","Tot"))
#agg_2nd$Morlab=factor(agg_2nd$Morlab, levels = c(),)

agg_2nd=agg_2nd[order(agg_2nd$x),]
agg_2nd=agg_2nd %>% left_join(agg_2nd_tot)
agg_2nd$rel=round(agg_2nd$x/agg_2nd$Tot*100,1)

view(agg_2nd)
ggplot()+
  geom_bar(data=agg_2nd,aes(x="",y=x, fill=Morlab), col="white",stat="identity", size=1.5,
           position = position_fill(reverse = T),width = 1)+
  coord_polar("y")+
  scale_fill_manual(values = (c("#3C5488FF","#00A087FF","#E64B35FF","grey50")))+
  facet_rep_wrap(Event~grouplab, nrow = 2,repeat.tick.labels = T)+
  theme_bw()+
  theme(
    panel.background = element_rect(fill = "transparent", color = NA, size = 0), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA, size=0), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    strip.text = element_text(size = 28, family = "Arial",face = "bold"),
    legend.title = element_text(size = 16, vjust = 0.75,colour = "black", family = "Arial",face = "bold"),
    legend.text = element_text(size = 12, colour = "black", family = "Arial",hjust = 0.5,margin = unit(c(0,0,0,0),"cm")),
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent", color = NA, size=0), # get rid of legend panel bg
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    panel.border = element_blank(),
    legend.position = "right",
    legend.direction = "vertical"
  )+
  guides(fill=guide_legend(reverse = T, title = ""))+
  ggsave("Figure/230704pie_pie_2nd.png",height = 20, width = 45, units = "cm", dpi = 300)

##donut=====
tt=c("#6DA9E4","#884A39","#E8AA42","#CE5959","#7DB9B6","#DDDDDD")

##UT=====
sem_merge_2nd
table(sem_merge_2nd$Type2)

sem_merge_2nd$Type2=factor(sem_merge_2nd$Type2, levels = c("Carbonaceous","Mineral","N-rich","Fe-rich","Transition metal","Others"))

stat_type_2nd=aggregate(sem_merge_2nd$Freq, by=list(Grouplab=sem_merge_2nd$grouplab, Type=sem_merge_2nd$Type2),sum)
stat_type_2nd=stat_type_2nd[order(stat_type_2nd$x,decreasing = T),]

#UT:Carbonaceous, Mineral, Fe-rich, N-rich, Transition metal, Calcium Sulfate
sem_merge_2nd_ul=subset(sem_merge_2nd,sem_merge_2nd$grouplab=="UT")
sem_merge_2nd_ul_ev=subset(sem_merge_2nd_ul,sem_merge_2nd_ul$Event=="Event")
sem_merge_2nd_ul_nev=subset(sem_merge_2nd_ul,sem_merge_2nd_ul$Event=="Non-event")

png(filepng("ul_pie_ev_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_ul_ev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "UT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ul_pie_ev_fin_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_ul_ev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "UT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ul_pie_nev_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_ul_nev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "UT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ul_pie_nev_2nd_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_ul_nev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "UT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()


##BJ=====
sem_merge_2nd_bj=subset(sem_merge_2nd,sem_merge_2nd$grouplab=="BJ")
sem_merge_2nd_bj_ev=subset(sem_merge_2nd_bj,sem_merge_2nd_bj$Event=="Event")
sem_merge_2nd_bj_nev=subset(sem_merge_2nd_bj,sem_merge_2nd_bj$Event=="Non-event")

png(filepng("bj_pie_ev_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_bj_ev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "BJ",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("bj_pie_ev_2nd_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_bj_ev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "BJ",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("bj_pie_nev_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_bj_nev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "BJ",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("bj_pie_nev_2nd_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_bj_nev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "BJ",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

##SS=====
sem_merge_2nd_ss=subset(sem_merge_2nd,sem_merge_2nd$grouplab=="SS")
sem_merge_2nd_ss_ev=subset(sem_merge_2nd_ss,sem_merge_2nd_ss$Event=="Event")
sem_merge_2nd_ss_nev=subset(sem_merge_2nd_ss,sem_merge_2nd_ss$Event=="Non-event")

png(filepng("ss_pie_ev_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_ss_ev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SS",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ss_pie_ev_2nd_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_ss_ev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SS",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ss_pie_nev_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_ss_nev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SS",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("ss_pie_nev_2nd_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_ss_nev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SS",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

##SE=====
sem_merge_2nd_se=subset(sem_merge_2nd,sem_merge_2nd$grouplab=="SE")
sem_merge_2nd_se_ev=subset(sem_merge_2nd_se,sem_merge_2nd_se$Event=="Event")
sem_merge_2nd_se_nev=subset(sem_merge_2nd_se,sem_merge_2nd_se$Event=="Non-event")

png(filepng("se_pie_ev_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_se_ev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SE",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("se_pie_ev_2nd_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_se_ev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SE",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("se_pie_nev_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_se_nev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SE",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("se_pie_nev_2nd_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_se_nev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "SE",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()


##NT=====
sem_merge_2nd_nt=subset(sem_merge_2nd,sem_merge_2nd$grouplab=="NT")
sem_merge_2nd_nt_ev=subset(sem_merge_2nd_nt,sem_merge_2nd_nt$Event=="Event")
sem_merge_2nd_nt_nev=subset(sem_merge_2nd_nt,sem_merge_2nd_nt$Event=="Non-event")

png(filepng("nt_pie_ev_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_nt_ev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "NT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("nt_pie_ev_2nd_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_nt_ev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "NT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("nt_pie_nev_2nd"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_nt_nev,aes(Morlab ,Type2 ), pieLabelSize=6,donutLabelSize = 8,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "NT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0.8,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()

png(filepng("nt_pie_nev_2nd_fin"), width = 20, height = 20,units = "cm",res = 300, bg = "transparent")
PieDonut_ms(sem_merge_2nd_nt_nev,aes(Morlab ,Type2 ), pieLabelSize=0,donutLabelSize = 0,
            r1=getOption("PieDonut.r1",0.97), r2=getOption("PieDonut.r2",1.2),
            color="black",piecolor = "white",donutcolor = "black",pielwd = 1.6, donutlwd = 1.3,
            pietitle = "NT",titlesize = 15,ratioByGroup=F,
            pieAlpha = 1, donutAlpha = 1,showPieName = T,
            showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.01),
            labelpositionThreshold=0.05,labelposition=0,fill="manual",fillcol = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"),
            subfill = "manual",subfillcol=rep(tt,4))
dev.off()




sem_merge
sem_merge_1st=sem_merge[,c("point_id","Morphology","Size","Class2","Type","Detail","Type2","Group","Year","Month","Day","Event","Freq","grouplab","Morlab")]
sem_merge_2nd_sel=sem_merge_2nd[,-c("Date")]

sem_merge_1st$pd="1st"
sem_merge_2nd_sel$pd="2nd"

sem_merge_1st=sem_merge_1st %>% `colnames<-`(c("point_id","Morphology","Size","Class","Type","Detail","Type2","Group","Year","Month","Day","Event","Freq","grouplab","Morlab"))
sem_merge_2nd_sel

dim(sem_merge_1st)
dim(sem_merge_2nd_sel)

sem_merge_all=rbind(sem_merge_1st,sem_merge_2nd_sel)
sem_merge_all

sem_merge_1st

int_breaks <- function(x, n = 5) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
}

ggplot(sem_merge_all, aes(x=Size, fill=Morlab))+
  geom_blank()+
  geom_histogram(data=sem_merge_1st,aes(x=Size, fill=Morlab),bin=40, color="black",alpha=0.8, position = position_stack(reverse = T))+
  facet_rep_wrap(Event~grouplab, scales = "free_y", ncol = 5,repeat.tick.labels = T)+
  scale_fill_manual(values = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"))+
  scale_y_continuous(name = "Frequency", expand = c(0.04,0.04),breaks = function(x) int_breaks(x, n = 5))+
  scale_x_continuous(name = expression(bold("Particle size"~"("*"\u03bcm"*")")),
                     breaks = seq(1,9,2),
                     limits = c(0,9.2),
                     expand = c(0.02,0.02)
  )+
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
        legend.text = element_text(size = 14, colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.2,0.1),"cm"), hjust = 0.5),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 14,family = "Arial"),
        legend.box.background = element_blank(),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "vertical",
        legend.background = element_blank(),
        #legend.position = c(0.08,1.02)
        legend.position = c(0.95,0.30))+
  xlab("")+
  guides(fill=guide_legend(order = 1,title ="Morphology",col=c(NA,NA,NA), linetype=c(1,1,1), alpha=0.4, title.hjust = 0.5),
         col=F)+
  ggsave(filename("SEM_particle_dist2_1st"), height = 20, width = 45, units = "cm", dpi = 300, compression="lzw")


ggplot(sem_merge_all, aes(x=Size, fill=Morlab))+
  geom_blank()+
  geom_histogram(data=sem_merge_2nd_sel,aes(x=Size, fill=Morlab),bin=40, color="black",alpha=0.8, position = position_stack(reverse = T))+
  facet_rep_wrap(Event~grouplab, scales = "free_y", ncol = 5,repeat.tick.labels = T)+
  scale_fill_manual(values = c("#3C5488FF","#00A087FF","#E64B35FF","grey50"))+
  scale_y_continuous(name = "Frequency", expand = c(0.04,0.04),breaks = function(x) int_breaks(x, n = 5))+
  scale_x_continuous(name = expression(bold("Particle size"~"("*"\u03bcm"*")")),
                     breaks = seq(1,9,2),
                     limits = c(0,9.2),
                     expand = c(0.02,0.02)
  )+
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
        legend.text = element_text(size = 14, colour = "black",family = "Arial",margin = unit(c(0.1,0.1,0.2,0.1),"cm"), hjust = 0.5),
        legend.title = element_text(margin = unit(c(0.0,0.1,0.0,0.1),"cm"), size = 14,family = "Arial"),
        legend.box.background = element_blank(),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.direction = "vertical",
        legend.background = element_blank(),
        #legend.position = c(0.08,1.02)
        legend.position = c(0.95,0.30))+
  xlab("")+
  guides(fill=guide_legend(order = 1,title ="Morphology",col=c(NA,NA,NA), linetype=c(1,1,1), alpha=0.4, title.hjust = 0.5),
         col=F)+
  ggsave(filename("SEM_particle_dist2_2nd"), height = 20, width = 45, units = "cm", dpi = 300, compression="lzw")


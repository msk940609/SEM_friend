envi=fread("Datafile/SEM_envi.csv")


table(envi$Event)


envi_m=melt(envi[,c("SEM","Group","No","Event","PM2.5","Temp","RH","NH4+","NO3-","SO42-","NO","NOx","SO2","CO","O3")],id.vars = c("SEM","Group","No","Event"))

envi_m$Grouplab=factor(envi_m$Group, levels = c("UT","BJ","SS","SE","NT"))

envi_m_ev=subset(envi_m,envi_m$SEM=="1st")
envi_m_nev=subset(envi_m,envi_m$SEM=="2nd")

ggplot(envi_m_1st, aes(x=Group, y=value, fill=Event))+
  geom_boxplot()+
  facet_rep_wrap(.~variable,repeat.tick.labels = T,ncol=3, scales = "free")


envi_m_mean=aggregate(envi_m$value,by=list(Group=envi_m$Grouplab, var=envi_m$variable, Event=envi_m$Event, Season=envi_m$SEM),mean, na.rm=T) %>% `colnames<-`(c("Group","variable","Event","Season","mean"))
envi_m_sd=aggregate(envi_m$value,by=list(Group=envi_m$Grouplab, var=envi_m$variable, Event=envi_m$Event, Season=envi_m$SEM),sd, na.rm=T) %>% `colnames<-`(c("Group","variable","Event","Season","sd"))

envi_m_mean=envi_m_mean %>% left_join(envi_m_sd)
envi_m_mean$Grouplab=factor(envi_m_mean$Group, levels = c("UT","BJ","SS","SE","NT"))

table(envi_m_mean$variable)

envi_m_mean$varlab=factor(envi_m_mean$variable, levels = c("PM2.5","Temp","RH","O3",
                                                           "CO","NO","NOx","SO2",
                                                           "NH4+","NO3-","SO42-"),
                           labels = c(
                             expression(bold("PM"["2.5"]~"("*"\u03bcg/"*m^"3"*")")),
                             expression(bold("Temperature (℃)")),
                             expression(bold("RH (%)")),
                             expression(bold("O"["3"]~"(ppb)")),
                             expression(bold("CO"~"(ppb)")),
                             expression(bold("NO"~"(ppb)")),
                             expression(bold("NO"["x"]~"(ppb)")),
                             expression(bold("SO"["2"]~"(ppb)")),
                             expression(bold("NH"["4"]^" +"~"("*"\u03bcg/"*m^"3"*")")),
                             expression(bold("NO"["3"]^" -"~"("*"\u03bcg/"*m^"3"*")")),
                             expression(bold("SO"["4"]^" 2-"~"("*"\u03bcg/"*m^"3"*")"))
                           ))


envi_m_mean_1st=subset(envi_m_mean,envi_m_mean$Season=="1st")
envi_m_mean_2nd=subset(envi_m_mean,envi_m_mean$Season=="2nd")


barwidth=0.3

labs_grp = c(expression(bold("UT")),expression(bold("BJ")),expression(bold("SS")), expression(bold("SE")), expression(bold("NT")))

pl <- ggplot()+
  geom_bar_pattern(data=envi_m_mean_1st,position= position_dodge(), stat = "identity",
                   aes( x=as.numeric(Group)+0.01, y=round(mean,2),
                        pattern =as.factor(Event), fill=as.factor(Event)
                   ), 
                   pattern = "none",
                   col="black",
                   #fill    = "#FFA500",
                   #pattern_fill    = "#3C5488FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 1.1
  )+
  geom_bar_pattern(data=envi_m_mean_2nd,position= position_dodge(), stat = "identity",
                   aes( x=as.numeric(Group)+0.1+barwidth, y=round(mean,2),
                        pattern =as.factor(Event), fill=as.factor(Event)
                   ), 
                   pattern = "stripe",
                   col="black",
                   #fill    = "#89CFF0"
                   #pattern_fill    = "#89CFF0",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.4, 
                   pattern_key_scale_factor = 1.1
  )+
  facet_rep_wrap(.~varlab,repeat.tick.labels = T,scales = "free", ncol=4, labeller = label_parsed,strip.position="left")+
  #scale_x_continuous(name="",labels = c("UT","BJ","SS","SE","NT"),expand = c(0.02,0.02))+
  scale_x_continuous(name="",breaks = (c(1.2,2.2,3.2,4.2,5.2)),labels =labs_grp,expand = c(0.02,0.02))+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c("#FFA500","#89CFF0"))+
  scale_pattern_fill_manual(values = c("#FFA500","#89CFF0"))+
  theme_bw()+
  theme(
    plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
    plot.title = element_text(size=18, face = 2, hjust = 0.5),
    panel.grid = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 13, colour = "black", angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size=15, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")
#  ggsave(filename("envi_trend"),height = 20, width = 40, units = "cm", dpi = 300, compression="lzw")



pz <- ggplot()+
  geom_bar_pattern(data=envi_m_mean_1st,position= position_dodge(), stat = "identity",
                   aes( x=as.numeric(Group)+0.01, y=round(mean,2),
                        pattern =as.factor(Event), fill=as.factor(Event)
                   ), 
                   pattern = "none",
                   col="black",
                   #fill    = "#FFA500",
                   #pattern_fill    = "#3C5488FF",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.6, 
                   pattern_key_scale_factor = 1.1
  )+
  geom_bar_pattern(data=envi_m_mean_2nd,position= position_dodge(), stat = "identity",
                   aes( x=as.numeric(Group)+0.1+barwidth, y=round(mean,2),
                        pattern =as.factor(Event), fill=as.factor(Event)
                   ), 
                   pattern = "stripe",
                   col="black",
                   #fill    = "#89CFF0"
                   #pattern_fill    = "#89CFF0",
                   width=barwidth,
                   alpha=0.7,
                   pattern_density          = 0.4, 
                   pattern_key_scale_factor = 1.1
  )+
  facet_rep_wrap(.~varlab,repeat.tick.labels = T,scales = "free", ncol=4, labeller = label_parsed,strip.position="left")+
  #scale_x_continuous(name="",labels = c("UT","BJ","SS","SE","NT"),expand = c(0.02,0.02))+
  scale_x_continuous(name="",breaks = (c(1.2,2.2,3.2,4.2,5.2)),labels =labs_grp,expand = c(0.02,0.02))+
  scale_y_continuous(expand = c(0.02,0.02))+
  scale_fill_manual(values = c("#FFA500","#89CFF0"))+
  scale_pattern_fill_manual(values = c("#FFA500","#89CFF0"))+
  coord_cartesian(ylim = c(0,15))+
  theme_bw()+
  theme(
    plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
    plot.title = element_text(size=18, face = 2, hjust = 0.5),
    panel.grid = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size=18, face = 2),
    axis.title.x = element_text(size=0.1),
    axis.title.y = element_text(size=0.1),
    axis.text.x = element_text(size = 13, colour = "black", angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size=15, colour = "black",face = 2),
    legend.title = element_text(size = 16, face = 2),
    legend.text = element_text(size = 14, face = 1),
    legend.direction = "horizontal",
    legend.position = "NULL")
#  ggsave(filename("envi_trend_zoom"),height = 20, width = 40, units = "cm", dpi = 300, compression="lzw")

pl/pz+ggsave(filename("envi_trend_merge"),height = 40, width = 40, units = "cm", dpi = 300, compression="lzw")


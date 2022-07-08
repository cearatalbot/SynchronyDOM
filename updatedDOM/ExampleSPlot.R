##example code for synchrony dot plots
library(ggplot2)
data<-read.csv("significantSynchResults.csv")

#a plot for only the limno variables
Limno<-dataSub[data$varGroup=="Limnological",] #subset by variable group
Limno$LUgroup_f<-factor(Limno$LUgroup, levels=c("allsites", "agDominated", "mixed", "wetDominated")) #set as factor in the order that we want them listed in the legend

###ggplot theme I used 
borderThemeBox<-theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                      panel.background = element_rect(colour = "black", size=1, fill=NA), 
                      strip.background = element_blank(), 
                      panel.spacing = unit(0.3, "lines"), axis.ticks.length = unit(0.2, "lines"), 
                      text = element_text(size = 12),legend.background=element_blank(), 
                      axis.text.x=element_text(), aspect.ratio = 0.2)
#color palette 
cols<-c("black","#5F0F40", "#E36414", "#0F4C5C")
#set variables for x axis as factors in order that I want them plotted
Limno$var_f<-factor(Limno$var, levels=c("DOC", "TDN", "TDP", "TP", "pH", "DO", "TSS", "SPC"))
Lim<-ggplot(data = Limno, aes(x=var_f, y=S, group=LUgroup_f))+
  geom_point(size=3, alpha=0.75, aes(color = LUgroup_f))+
  scale_colour_manual("Land use", values=cols, labels=c("All sites", "Agriculturally dominated", "Mixed", "Wetland dominated"))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1, 0.25))+
  scale_x_discrete("Variable")+
  borderThemeBox
Lim
#ggsave(filename = "SynchronyLimnoVars.png", plot=Lim, width = 6, height = 2, units= "in", device='png', dpi=320)

##Figures from synchrony analysis
#Libraries
library(ggplot2)
library(cowplot)
library(ggpubr)
library(RColorBrewer)

setwd(" ")

#read synchrony output
data<-read.csv(file="Out/synchronyResults_2024Apr18.csv", stringsAsFactors=F)

data$var[which(data$var=="S.COND")]<-"SPC"
data$varGroup<-c(rep("Limnological", times=4), rep("DOM", times=(4*13)), 
                 rep("Limnological", times=(4*5)), rep("Physical", times=4), 
                 rep("Limnological", times=(4*2)), rep("DOM", times=(4*5)))
borderTheme0.5<-theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                      panel.background = element_rect(colour = "black", size=1, fill=NA), 
                      strip.background = element_blank(), 
                      panel.spacing = unit(0.3, "lines"), axis.ticks.length = unit(0.2, "lines"), 
                      text = element_text(size = 14),legend.background=element_blank(), 
                       axis.text.x=element_text(), aspect.ratio = 0.5, legend.position = "none")
borderTheme0.5Leg<-theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                      panel.background = element_rect(colour = "black", size=1, fill=NA), 
                      strip.background = element_blank(), 
                      panel.spacing = unit(0.3, "lines"), axis.ticks.length = unit(0.2, "lines"), 
                      text = element_text(size = 14),legend.background=element_blank(), 
                      axis.text.x=element_text(), legend.key=element_blank(), aspect.ratio = 0.5)

borderTheme1<-theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                      panel.background = element_rect(colour = "black", size=1, fill=NA), 
                      strip.background = element_blank(), 
                      panel.spacing = unit(0.3, "lines"), axis.ticks.length = unit(0.2, "lines"), 
                      text = element_text(size = 15),legend.background=element_blank(), 
                      axis.text.x=element_text(), aspect.ratio = 0.3,
                      legend.key=element_blank(),legend.position = "none")

borderTheme1.5<-theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                      panel.background = element_rect(colour = "black", size=1, fill=NA), 
                      strip.background = element_blank(), 
                      panel.spacing = unit(0.3, "lines"), axis.ticks.length = unit(0.2, "lines"), 
                      text = element_text(size = 15),legend.background=element_blank(), 
                      axis.text.x=element_text(), aspect.ratio = 1.8,
                      legend.key=element_blank())
borderThemeBox<-theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                      panel.background = element_rect(colour = "black", size=1, fill=NA), 
                      strip.background = element_blank(), 
                      panel.spacing = unit(0.3, "lines"), axis.ticks.length = unit(0.2, "lines"), 
                      text = element_text(size = 15),legend.background=element_blank(), 
                      axis.text.x=element_text(), aspect.ratio = 0.2)

#variables that were in dataset but were not included in analysis
rmVars<-c("CHL", "SRP", "PC1", "PC2", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "E280", "A254")
for(i in 1:length(rmVars)){
  data<-data[which(data$var!=rmVars[i]), ]
}


dataSub<-data[which(data$pVal < 0.05),] #susbet for statistically significant data only, using p value from synchrony analysis 
cols<-c("#6B5671",  "#F0606A", "#F5B266", "#71BCC1")
brewer.pal(n = 8, name = "YlGn")

#write.csv(dataSub, "Out/significantSynchResults.csv", row.names=F)
Limno<-dataSub[dataSub$varGroup=="Limnological",]
Limno$LUgroup_f<-factor(Limno$LUgroup, levels=c("allsites", "agDominated", "mixed", "wetDominated"))
Physical<-dataSub[dataSub$varGroup=="Physical",]
Physical$LUgroup_f<-factor(Physical$LUgroup, levels=c("allsites", "agDominated", "mixed", "wetDominated"))
DOM<-dataSub[dataSub$varGroup=="DOM",]
DOM$LUgroup_f<-factor(DOM$LUgroup, levels=c("allsites", "agDominated", "mixed", "wetDominated"))

Limno$var_f<-factor(Limno$var, levels=unique(Limno$var))
Lim<-ggplot(data = Limno, aes(x=var_f, y=S, group=LUgroup_f))+
  geom_jitter(size=3, alpha=1, aes(color = LUgroup_f), width=0.2)+
  scale_colour_manual("Land use", values=cols, labels=c("All sites", "Agriculturally dominated", "Mixed", "Wetland dominated"))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1, 0.25))+
  scale_x_discrete("Variable")+
  annotate("text", x=0.7, y=0.96, label= "B.", size=6)+
  borderTheme0.5
Lim
ggsave(filename = "Figures/SynchronyLimnoVars_20240823.png", plot=Lim, width = 6, height = 3, units= "in", device='png', dpi=320)

Physical$var_f<-factor(Physical$var, levels=unique(Physical$var))
Phys<-ggplot(data = Physical, aes(x=var, y=S, group=LUgroup_f))+
  geom_jitter(size=3, alpha=1,aes(color = LUgroup_f), width=0.2)+
  scale_colour_manual("Land use",values=cols, labels=c("All sites", "Agriculturally dominated", "Mixed", "Wetland dominated"))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1, 0.25))+
  scale_x_discrete("Variable", labels=c("Temp"))+
  annotate("text", x=0.6, y=0.96, label= "A.", size=6)+
  borderTheme1.5
Phys  
ggsave(filename = "Figures/SynchronyPhysVars_20240823.png", plot=Phys, width = 9, height = 3, units= "in", device='png', dpi=320)

DOM$var_f<-factor(DOM$var, levels=unique(DOM$var))
pDOM<-ggplot(data = DOM, aes(x=var_f, y=S, group=LUgroup_f))+
  geom_jitter(size=3, alpha=1, aes(color = LUgroup_f), width=0.2)+
  scale_colour_manual("Land use",values=cols, labels=c("All sites", "Agriculturally dominated", "Mixed", "Wetland dominated"))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1, 0.25))+
  scale_x_discrete("Variable", labels=c(expression("A"[350]), "SR", "\u03B2:\u03B1", "FI", "HIX", expression("SUVA"[254]), expression( "C"[TH]),expression("C"[MH]),expression("C"[MP]), expression("C"[F])))+
  annotate("text", x=0.8, y=0.96, label= "C.", size=6)+
  borderTheme0.5
pDOM
ggsave(filename = "Figures/SynchronyDOMVars_20240823.png", plot=pDOM, width =9, height = 3, units= "in", device='png', dpi=320)


dataSub$LUgroup_f<-factor(dataSub$LUgroup, levels=unique(dataSub$LUgroup))
dataSub$varGroup_f<-factor(dataSub$varGroup, levels=c("Physical", "Limnological", "DOM"))
mypoints<-dataSub[which(dataSub$varGroup_f=="Physical"), ]

pdata<-ggplot(data = dataSub, aes(x=varGroup_f, y=S, fill=LUgroup_f))+
  stat_boxplot(geom ='errorbar', width = 0.7)+
  geom_boxplot(alpha=1, width=0.7, )+
  scale_fill_manual("Land use",values=cols, labels=c("All sites", "Agriculturally dominated", "Mixed", "Wetland dominated"))+
  scale_color_manual("Land use",values=cols, labels=c("All sites", "Agriculturally dominated", "Mixed", "Wetland dominated"))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1, 0.25))+
  scale_x_discrete(" ")+
  geom_point(data=mypoints, position=position_dodge(width=0.7), aes(color=LUgroup_f), size=2)+
  borderTheme0.5Leg
pdata
ggsave(filename = "Figures/SynchronyBox_20240823.png", plot=pdata, width = 6, height = 3, units= "in", device='png', dpi=320)



###Maps
library(raster)
library(sf)
provinces <- getData(country="Canada", level=1)
USA<-getData(country="USA", level=0)
#format CA and USA data
    ON<-provinces[provinces@data$NAME_1=="Ontario", ]
    prov<-st_as_sf(provinces)
    simp_prov<-st_simplify(prov,preserveTopology=TRUE,dTolerance=5000)
    ON<-st_as_sf(ON)
    simp_ON<-st_simplify(ON,preserveTopology=TRUE,dTolerance=5000)
    USA<-st_as_sf(USA)
    simp_USA<-st_simplify(USA,preserveTopology=TRUE,dTolerance=5000)
    mp1<-ggplot(data=simp_ON)+geom_sf()+coord_sf(crs=st_crs(ON))+theme_bw()

#Open coords for sites, create a box
siteLU<-read.csv(file="Out/landUseGroups.csv", stringsAsFactors=F)
site_coords<-read.csv(file="rawData/streamLatLon.csv", stringsAsFactors=F)
site_coords$Group<-0
#loop over to add land use group to coordinate data
for(i in 1:nrow(site_coords)){
  site_coords$Group[i]<-siteLU$landUse[which(siteLU$SiteNum==site_coords$num_id[i])]
}
yLim<-range(site_coords$lat)
y_coords<-c((yLim[1]-0.2), (yLim[2]+0.2))
xLim<-range(site_coords$lon)
x_coords<-c((xLim[1]-0.2), (xLim[2]+0.2))
poly1<-sp::Polygon(cbind(x_coords,y_coords))
rect <- data.frame(
  x = c((xLim[2]-3.7), (xLim[2]+0.5), (xLim[2]+0.5), (xLim[2]-3.7)),
  y = c((yLim[1]-0.5), (yLim[1]-0.5), (yLim[2]+0.5), (yLim[2]+0.5))
)
poly1<-sp::Polygon(rect)
firstPoly <- sp::Polygons(list(poly1), ID = "A")
myBox<-sp::SpatialPolygons(list(firstPoly))
myBox1<-st_as_sf(myBox)
st_crs(myBox1)<-st_crs(simp_prov)
can<-ggplot(data=simp_prov)+geom_sf(fill="white", color="grey")+coord_sf(crs=st_crs(simp_prov))#+theme_minimal()
can1<-can+geom_sf(data=simp_USA, fill="white", color="grey")+scale_x_continuous(limits=c(-170,-50))+
  scale_y_continuous(limits=c(25,85))+theme_light(base_size=13)
can2<-can1+geom_sf(data=simp_ON, fill="white", color="black")
can3<-can2+geom_sf(data=myBox1, fill=NA, color="black", size=1)
#ggsave(filename = "Figures/LargeMap.png", plot=can3, width = 4, height = 3.2, units= "in", device='png', dpi=320)

#inset map of points
streams <- data.frame(ID =site_coords$num_id,
                     x = site_coords$lon,
                     y = site_coords$lat, 
                     LU=site_coords$Group)
ranX<-range(rect$x)
ranY<-range(rect$y)
mCols<-cols[2:4]
streams$LU_f<-factor(streams$LU, levels=c("agDominated", "mixed", "wetDominated"))
ont<-ggplot(data=simp_ON)+geom_sf(fill="white", color="grey")+coord_sf(crs=st_crs(simp_prov))+
  theme_light(base_size=14)+scale_x_continuous(" ", limits=c(ranX[1], ranX[2]))+
  scale_y_continuous(" ",limits=c(ranY[1], ranY[2]))+
  geom_point(data = streams, aes(x = x, y = y, color=LU_f), size=2)+scale_color_manual("Land use", values=mCols,labels=c("Agriculturally dominated", "Mixed", "Wetland dominated"))
ont
#ggsave(filename = "Figures/OntarioMap.png", plot=ont, width = 5.5, height = 3, units= "in", device='png', dpi=320)


##Completed analyses for Synchrony DOM manuscript
#setup for synchrony, group land use, Moran's I
#####group stream sites###
#packages
library(vegan)
library(ggplot2)
library(dendextend)
library(cowplot)
library(ggpubr)
library(ggdendro)

#wd
setwd("/Users/cearatalbot/Rcode/SynchronyDOM/updatedDOM/")
#get supporting functions
source("synchrony/sFunctions.R") #supporting functions for analysis

#read data
dfSub<-read.csv(file="rawData/LandUse.csv", stringsAsFactors=F)
data<-read.csv(file="rawData/ASN_allData.csv", stringsAsFactors=F)
siteLookup<-read.csv("rawData/SiteNameNum.csv", stringsAsFactors = F)

#update means with 2019 data
dfSub$SiteNum<-NA
allsites<-dfSub[,1]
allsites<-nameToNumber(siteList=allsites, lookupTable = siteLookup)
dfSub$SiteNum<-allsites

  #calculate means
  for(i in 1:nrow(dfSub)){
    dfSub$TDNavg[i]<-mean(as.numeric(data[which(data$Sample.Name==dfSub[i,12]), 32]),na.rm=T) #TDN
    dfSub$TDPavg[i]<-mean(as.numeric(data[which(data$Sample.Name==dfSub[i,12]), 33]), na.rm=T) #TDP
    dfSub$DOCavg[i]<-mean(as.numeric(data[which(data$Sample.Name==dfSub[i,12]), 5]), na.rm=T) #DOC
    dfSub$TPavg[i]<-mean(as.numeric(data[which(data$Sample.Name==dfSub[i,12]), 36]), na.rm=T) #TP
    dfSub$TSSavg[i]<-mean(as.numeric(data[which(data$Sample.Name==dfSub[i,12]), 37]), na.rm=T) #TSS
    }

row.names(dfSub)<-as.character(unlist(dfSub[,1])) #use stream column to rename columns
dfSub<-dfSub[,-1 ] #delete the stream column and site number
#new##
#dfSub$CWetlaNA<-NULL
#dfSub$RWetlaNA<-NULL
#dfSub$CTotalCrop<-NULL
#dfSub$RTotal.Crop<-NULL
#dfSub$Cmono<-NULL
row.names(dfSub)<-dfSub$SiteNum
d<-dist(dfSub[,1:10])
hc<-hclust(d)
hcd <- as.dendrogram(hc)
colorL<-c()
colorL[1:7]<-"#0F4C5C"
colorL[8:37]<-"#E36414"
colorL[38:45]<-"#5F0F40"
labels_colors(hcd)<-colorL
dendrop<-plot(hcd, ylab = "Height")
#plot(hc, xlab="Distance") #labels = dfSub$LandUse

ddata <- dendro_data(hcd, type = "rectangle")
dplot<-ggdendrogram(hcd, rotate = FALSE, size = 3, ylab="Height", labels=T, )
dendro.plot <- dplot + theme(axis.text.y = element_text(size = 6))
dplot<-dplot+labs(y="Height")
p <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  #coord_flip() + 
  #scale_y_reverse("Height", expand = c(0.2, 0))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(), 
        panel.background = element_rect(fill=NA),
        axis.line.y.left=element_line(size=0.3),
        text = element_text(size = 12),
        )+
  scale_y_continuous("Height", limits=c(0,200), breaks=seq(0,200, 50))+
  geom_text(data = ddata$labels , 
            aes(x = x, y = y, label = label), size = 3, vjust = 1.2, hjust=0.5) #color=colorL
p
ggsave(filename = "Figures/Dendrogram.png", plot=p, width = 7.5, height = 2.5, units= "in", device='png', dpi=320)

cols<-c("#5F0F40", "#E36414", "#0F4C5C")

dfSub$landUse<-c("mixed","mixed", "wetDominated", "mixed",
"mixed", "mixed", "wetDominated", "wetDominated","mixed",
"mixed", "mixed", "mixed", "wetDominated", "agDominated",
"agDominated", "mixed", "wetDominated", "mixed", "mixed",
"wetDominated", "mixed", "mixed", "wetDominated", "mixed",
"mixed", "agDominated", "mixed", "mixed", "mixed",
"mixed", "agDominated", "mixed", "agDominated", 
"mixed", "wetDominated", "mixed", "mixed", 
"agDominated", "agDominated", "mixed", "wetDominated",
"mixed", "agDominated", "mixed", "agDominated")

#write.csv(dfSub, "Out/landUseGroups.csv", row.names=F)
landuse.ano <- anosim(d, dfSub$landUse)
summary(landuse.ano) #R2=0.76, P=0.001
plot(landuse.ano) 
 
borderTheme0.5<-theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                      panel.background = element_rect(colour = "black", size=1, fill=NA), 
                      strip.background = element_blank(), 
                      panel.spacing = unit(0.3, "lines"), axis.ticks.length = unit(0.2, "lines"), 
                      text = element_text(size = 12),legend.background=element_blank(), 
                      legend.key = element_rect(), axis.text.x=element_text(), aspect.ratio = 1)

cols<-c("#F0606A", "#F5B266", "#71BCC1")
DOC<-ggplot(dfSub, aes(x=landUse, y=DOCavg))+geom_boxplot(fill=cols)+
  borderTheme0.5+
  scale_x_discrete("", labels=c("Agriculture \nDominated", "Mixed", "Wetland \n Dominated"))+
  scale_y_continuous(expression('DOC'~(mg~C~L^-1)), limits=c(0,30), breaks=seq(0,30,10))

TDN<-ggplot(dfSub, aes(x=landUse, y=TDNavg))+geom_boxplot(fill=cols)+
borderTheme0.5+
  scale_x_discrete("", labels=c("Agriculture \nDominated", "Mixed", "Wetland \n Dominated"))+
  scale_y_continuous(expression('TDN'~(mg~N~L^-1)), limits=c(0,5), breaks=seq(0,5,1))

TSS<-ggplot(dfSub, aes(x=landUse, y=TSSavg))+geom_boxplot(fill=cols)+
borderTheme0.5+
  scale_x_discrete("", labels=c("Agriculture \nDominated", "Mixed", "Wetland \n Dominated"))+
  scale_y_continuous(expression('TSS'~(mg~L^-1)),limits=c(0,52), breaks=seq(0,50,10))

TDP<-ggplot(dfSub, aes(x=landUse, y=TDPavg))+geom_boxplot(fill=cols)+
borderTheme0.5+
  scale_x_discrete("", labels=c("Agriculture \nDominated", "Mixed", "Wetland \n Dominated"))+
  scale_y_continuous(expression('TDP'~('\U03BC'~g~P~L^-1)), limits=c(0,90), breaks=seq(0,90,20))

#arrange and save multipanel plots
png("Figures/Figure1.png",  width = 9, height = 3, units= "in", res=320)#PNG device
ggarrange(DOC, TDN, TDP, TSS, nrow=1,ncol=4, common.legend = TRUE, legend="right", align = "hv")
dev.off()#Close device

#png("Figures/CLuster.png",  width = 8, height = 2.5, units= "in", res=320)#PNG device
#ggarrange(p, nrow=1)
#dev.off()#Close device


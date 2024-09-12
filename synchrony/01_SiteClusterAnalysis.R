##Completed analyses for Synchrony DOM manuscript
#setup for synchrony, group land use, Moran's I
#####group stream sites###
#packages
library(vegan)
library(ggplot2)
library(dendextend)
library(ggdendro)
library(ape)

#wd
setwd("/Users/cearatalbot/RCode/SynchronyDOM/")
#get supporting functions
source("synchrony/sFunctions.R") #supporting functions for analysis

#read data
dfSub<-read.csv(file="rawData/LandUse.csv", stringsAsFactors=F)
data<-read.csv(file="rawData/ASN_AllData_Synchrony.csv", stringsAsFactors=F)
siteLookup<-read.csv("rawData/SiteNameNum.csv", stringsAsFactors = F)

#update means with 2019 data
siteLookup[,2]<-as.character(siteLookup[,2])
dfSub$SiteNum<-NA
dfSub$SiteNum<-nameToNumber(siteList=dfSub[,1], lookupTable = siteLookup)

#calculate means
for(i in 1:nrow(dfSub)){
  dfSub$TDNavg[i]<-mean(as.numeric(data$TDN[which(data$ASN.SITE.NO==dfSub[i,12])]),na.rm=T) #TDN
  dfSub$TDPavg[i]<-mean(as.numeric(data$TDP[which(data$ASN.SITE.NO==dfSub[i,12])]), na.rm=T) #TDP
  dfSub$DOCavg[i]<-mean(as.numeric(data$DOC[which(data$ASN.SITE.NO==dfSub[i,12])]), na.rm=T) #DOC
  dfSub$TPavg[i]<-mean(as.numeric(data$TP[which(data$ASN.SITE.NO==dfSub[i,12])]), na.rm=T) #TP
  dfSub$TSSavg[i]<-mean(as.numeric(data$TSS[which(data$ASN.SITE.NO==dfSub[i,12])]), na.rm=T) #TSS
}

row.names(dfSub)<-as.character(unlist(dfSub[,1])) #use stream column to rename columns
dfSub<-dfSub[,-1 ] #delete the stream column and site number
row.names(dfSub)<-dfSub$SiteNum
d<-dist(dfSub[,1:10])
hc<-hclust(d)
hcd <- as.dendrogram(hc)
mycuts<-cutree(hcd, 4) #from 'dendextend' package

landUse<-c()
for(i in 1:length(mycuts)){
  if(mycuts[[i]]==1){
    landUse[i]<-"wetDominated"
  }else if(mycuts[[i]]==2){
    landUse[i]<-"mixed"
  }else if(mycuts[[i]]==3 | mycuts[[i]]==4 ){
    landUse[i]<-"agDominated"
  }
}

#store sites with their land use class
order_LU<-data.frame(matrix(ncol=2, nrow=length(mycuts)))
colnames(order_LU)<-c("site", "landUse")
order_LU$site<-hc$labels
order_LU$landUse<-landUse

mycols<-c("#5F0F40", "#E36414", "#0F4C5C")
dfSub$landUse<-NA
for(i in 1:nrow(dfSub)){
  dfSub$landUse[i]<-try(order_LU$landUse[which(order_LU$site==dfSub$SiteNum[i])])
}

#see where cuts are
plot(as.phylo(hcd),tip.color = mycols[mycuts])

#nicer plot
ddata <- dendro_data(hcd, type = "rectangle")
p <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(), 
        panel.background = element_rect(fill=NA),
        axis.line.y.left=element_line(size=0.3),
        text = element_text(size = 12),)+
  scale_y_continuous("Height", limits=c(0,4000), breaks=seq(0,4000, 1000))+
  scale_x_discrete()+
  geom_text(data = ddata$labels , 
            aes(x = x, y = y, label = label), size = 3, vjust = 1.2, hjust=0.5) #color=colorL
p
#ggsave(filename = "Figures/Dendrogram.png", plot=p, width = 7.5, height = 2.8, units= "in", device='png', dpi=320)

#write.csv(dfSub, "Out/landUseGroups.csv", row.names=F)

landuse.ano <- anosim(d, dfSub$landUse)
summary(landuse.ano) #R2=0.819, P=0.001
plot(landuse.ano) 


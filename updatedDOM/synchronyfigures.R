############Synchrony figures#######

library(reshape2)
results<-read.csv(file="C:/SynchronyR/Synchrony_results_update.csv", header = TRUE)
rlong <- melt(results, id.vars="Variable", value.name="S")
rlong2<-transform(rlong, year.rank = ave(S, c(variable, Variable), FUN = function(x) rank(-x, ties.method = "first")))
rlong2$SiteNum<-"0"
sitenum <- ifelse(rlong2$variable=='All', 0,
               ifelse(rlong2$variable == 'LowAg', 20, 
                      ifelse(rlong2$variable == 'IntAg', 40,  
                             ifelse(rlong2$variable == 'HighAg', 60,
                                    ifelse(rlong2$variable == 'LowW', 80,
                                    ifelse(rlong2$variable == 'HighW', 100 ,1))))))
               
rlong2$SiteNum<- sitenum
rlong2$newrank<-rlong2$SiteNum+rlong2$year.rank
library(ggplot2)
library(ggrepel)

#library(ggplot2)
#g2<-ggplot(data= rlong2, aes(x=x, y=S, group= variable)) +geom_point(aes(shape=Variable), size=2.5)+theme_classic(base_size = 18) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0)) +scale_x_discrete("Rank", breaks=c("1.1","2.1","3.1","4.1", "5.1", "6.1"), labels=NULL)+scale_shape_manual(values = c(1,17)) 
#g2<-ggplot(data= rlong2, aes(x=x, y=S, group= variable)) +geom_point(aes(shape=variable), size=2.5)+theme_classic(base_size = 18) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0)) +scale_x_discrete("Rank", breaks=c("1.1","3.1","5.1","7.1", "9.1", "11.1"), labels=NULL)+scale_shape_manual(values = c(17, 0, 2, 1, 15,4))
#best with all
#g2<-ggplot(data= rlong2, aes(x=year.rank, y=S, group= variable)) +geom_point(aes(shape=variable),size=2.5)+theme_classic(base_size = 18)+theme(strip.background = element_blank(),strip.text.x = element_blank()) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0)) +scale_x_discrete("Rank", breaks=c("2","4","6", "8", "10", "12", "13", "14"), labels=NULL)+scale_shape_manual(values = c(1,17, 2, 4, 5, 7))  +facet_grid(~variable) +geom_text(aes(label=Variable),hjust=0, vjust=0)+labs(fill="")
#w/no legend title
#g2<-ggplot(data= rlong2, aes(x=year.rank, y=S, group= variable)) +geom_point(aes(shape=variable),size=2.5)+theme_classic(base_size = 18)+theme(strip.background = element_blank(),strip.text.x = element_blank(), legend.title=element_blank()) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0)) +scale_x_discrete("Rank", breaks=c("2","4","6", "8", "10", "12", "13", "14"), labels=NULL)+scale_shape_manual(values = c(1,17, 2, 4, 5, 7))  +facet_grid(~variable) +geom_text(aes(label=Variable),hjust=0, vjust=0)

sites<-split(rlong2, rlong2$variable)
All<-sites$All
All$Type<-"NA"
All$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM")
Lowag<-sites$LowAg
Lowag$Type<-"NA"
Lowag$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM")
Intag<-sites$IntAg
Intag$Type<-"NA"
Intag$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM","DOM" )
Highag<-sites$HighAg
Highag$Type<-"NA"
Highag$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM")
Loww<-sites$LowW
Loww$Type<-"NA"
Loww$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM")
Highw<-sites$HighW
Highw$Type<-"NA"
Highw$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM")

agriculture<-rbind.data.frame(Lowag, Intag, Highag)
wetland<-rbind.data.frame(Loww, Highw)

agwet<-rbind.data.frame(Highag, Highw)

#best
g2<-ggplot(data= All, aes(x=newrank, y=S, group= Type)) +geom_point(aes(shape=Type),size=3)+theme_classic(base_size = 18)+theme(strip.background = element_blank(),strip.text.x = element_blank()) +scale_y_continuous(limits=c(-0.25,1),breaks=seq(-0.25,1,0.25),expand=c(0,0)) +scale_x_discrete("Rank", limits=c(0,18),breaks=c("2","4","6", "8", "10", "12", "13", "14", "16", "18"), labels=NULL)+scale_shape_manual(values = c(0,17, 16)) +geom_text(aes(label=Variable),hjust=0, vjust=0)+labs(fill="")
#geomrepel
g2<-ggplot(data= All, aes(x=newrank, y=S, group= Type)) +geom_point(aes(shape=Type),size=3)+theme_classic(base_size = 22)+theme(strip.background = element_blank(),strip.text.x = element_blank()) +scale_y_continuous(limits=c(-0.25,1),breaks=seq(-0.25,1,0.25),expand=c(0,0)) +scale_x_discrete("Rank", limits=c(0,18),breaks=c("2","4","6", "8", "10", "12", "13", "14", "16", "18"), labels=NULL)+scale_shape_manual(values = c(0,17, 16)) +geom_text_repel(aes(label=Variable),hjust=0.5, vjust=-0.5, size=5)+labs(fill="")
g2

g3<-ggplot(data= agriculture, aes(x=newrank, y=S, group= Type)) +geom_point(aes(shape=Type),size=3)+theme_classic(base_size = 28)+theme(strip.background = element_blank(),strip.text.x = element_blank()) +scale_y_continuous(limits=c(-0.40,1),breaks=seq(-0.40,1,0.20),expand=c(0,0)) +scale_x_discrete("Rank", limits=c(20,80),breaks=c("20","30","40", "50", "60", "70", "80"), labels=NULL)+scale_shape_manual(values = c(0,17, 16))+labs(fill="")+ geom_text_repel(aes(label=Variable),hjust=0.5, vjust=-0.5, size=5)
g3

g4<-ggplot(data= wetland, aes(x=newrank, y=S, group= Type)) +geom_point(aes(shape=Type),size=3)+theme_classic(base_size = 22)+theme(strip.background = element_blank(),strip.text.x = element_blank()) +scale_y_continuous(limits=c(-0.40,1),breaks=seq(-0.40,1,0.20),expand=c(0,0)) +scale_x_discrete("Rank", limits=c(80,120),breaks=c("80","90","100", "120"), labels=NULL)+scale_shape_manual(values = c(0,17, 16))+labs(fill="")+ geom_text_repel(aes(label=Variable),hjust=0.5, vjust=-0.5, size=5)
g4
#no labels
g2<-ggplot(data= agriculture, aes(x=newrank, y=S, group= Type)) +geom_point(aes(shape=Type),size=3)+theme_classic(base_size = 22)+theme(strip.background = element_blank(),strip.text.x = element_blank()) +scale_y_continuous(limits=c(-0.40,1),breaks=seq(-0.40,1,0.20),expand=c(0,0)) +scale_x_discrete("Rank", limits=c(20,80),breaks=c("20","30","40", "50", "60", "70", "80"), labels=NULL)+scale_shape_manual(values = c(0,17, 16))+labs(fill="")
g2<-ggplot(data= wetland, aes(x=newrank, y=S, group= Type)) +geom_point(aes(shape=Type),size=3)+theme_classic(base_size = 22)+theme(strip.background = element_blank(),strip.text.x = element_blank()) +scale_y_continuous(limits=c(-0.40,1),breaks=seq(-0.40,1,0.20),expand=c(0,0)) +scale_x_discrete("Rank", limits=c(80,120),breaks=c("80","90","100", "120"), labels=NULL)+scale_shape_manual(values = c(0,17, 16))+labs(fill="")
#bars
t<-ggplot(wetland, aes(Variable, S, fill= variable))
t+geom_bar(position= position_dodge(width=0.7), stat= "identity", color= "black", size= 1, width=0.7)+ scale_fill_manual(values=c("white","grey60" ,"black"))+theme_classic(base_size = 20)+theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 1)) + theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1), aspect.ratio=1)
#individual
#all
g2<-ggplot(data= All, aes(x=year.rank, y=S)) +geom_point(size=2.5)+theme_classic(base_size = 18)+theme(strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 3) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0)) +scale_x_discrete("Rank", limits=c(1,14), breaks=c("2","4","6", "8", "10", "12", "13", "14"), labels=NULL)+geom_text(aes(label=Variable),hjust=0, vjust=0)
#
g2<-ggplot(data= Lowag, aes(x=year.rank, y=S)) +geom_point(size=2.5)+theme_classic(base_size = 18)+theme(strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 3) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0)) +scale_x_discrete("Rank", limits=c(1,14), breaks=c("2","4","6", "8", "10", "12", "13", "14"), labels=NULL)+geom_text(aes(label=Variable),hjust=0, vjust=0)

#individual panels
g2<-ggplot(data= All, aes(x=year.rank, y=S)) +geom_point(size=2.5)+theme_classic(base_size = 22)+theme(strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0))+geom_text(aes(label=Variable),hjust=0, vjust=-0.5, size= 6)+labs(x="Rank")
g3<-ggplot(data= Lowag, aes(x=year.rank, y=S)) +geom_point(size=2.5)+theme_classic(base_size = 22)+theme(strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0))+geom_text(aes(label=Variable),hjust=0, vjust=-0.5, size= 6)+labs(x="Rank")
g4<-ggplot(data= Intag, aes(x=year.rank, y=S)) +geom_point(size=2.5)+theme_classic(base_size = 22)+theme(strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0))+geom_text(aes(label=Variable),hjust=0, vjust=-0.5, size= 6)+labs(x="Rank")
g5<-ggplot(data= Highag, aes(x=year.rank, y=S)) +geom_point(size=2.5)+theme_classic(base_size = 22)+theme(strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0))+geom_text(aes(label=Variable),hjust=0, vjust=-0.5, size= 6)+labs(x="Rank")
g6<-ggplot(data= Loww, aes(x=year.rank, y=S)) +geom_point(size=2.5)+theme_classic(base_size = 22)+theme(strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0))+geom_text(aes(label=Variable),hjust=0, vjust=-0.5, size= 6)+labs(x="Rank")
g7<-ggplot(data= Highw, aes(x=year.rank, y=S)) +geom_point(size=2.5)+theme_classic(base_size = 22)+theme(strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1),expand=c(0,0))+geom_text(aes(label=Variable),hjust=0, vjust=-0.5, size= 6)+labs(x="Rank")

#individual labelled variable types
g2<-ggplot(data= All, aes(x=year.rank, y=S)) +geom_point(aes(shape=Type),size=2.5)+theme_classic(base_size = 14)+theme(panel.border = element_rect(colour = "black", fill=NA, size=1),strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1, legend.position= "none") +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2),expand=c(0,0))+labs(x="Rank", y= "Synchrony")+scale_shape_manual(values = c(1,17, 2))+ geom_text_repel(aes(label=Variable),hjust=0.5, vjust=-0.5, size=4)
g3<-ggplot(data= Lowag, aes(x=year.rank, y=S)) +geom_point(aes(shape=Type),size=2.5)+theme_classic(base_size = 14)+theme(panel.border = element_rect(colour = "black", fill=NA, size=1),strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1, legend.position= "none") +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2),expand=c(0,0))+labs(x="Rank", y= " ")+scale_shape_manual(values = c(1,17, 2))+ geom_text_repel(aes(label=Variable),hjust=0.5, vjust=-0.5, size=4)
g4<-ggplot(data= Intag, aes(x=year.rank, y=S)) +geom_point(aes(shape=Type),size=2.5)+theme_classic(base_size = 14)+theme(panel.border = element_rect(colour = "black", fill=NA, size=1),strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1, legend.position= c(0.9,0.9)) +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2),expand=c(0,0))+labs(x="Rank", y = " ")+scale_shape_manual(values = c(1,17, 2))+ geom_text_repel(aes(label=Variable),hjust=0.5, vjust=-0.5, size=4)
g5<-ggplot(data= Highag, aes(x=year.rank, y=S)) +geom_point(aes(shape=Type),size=2.5)+theme_classic(base_size = 14)+theme(panel.border = element_rect(colour = "black", fill=NA, size=1), strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1, legend.position= "none") +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2),expand=c(0,0))+labs(x="Rank", y= "Synchrony")+scale_shape_manual(values = c(1,17, 2))+ geom_text_repel(aes(label=Variable),hjust=0.5, vjust=-0.5, size=4)
g6<-ggplot(data= Loww, aes(x=year.rank, y=S)) +geom_point(aes(shape=Type),size=2.5)+theme_classic(base_size = 14)+theme(panel.border = element_rect(colour = "black", fill=NA, size=1), strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1, legend.position= "none") +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2),expand=c(0,0))+labs(x="Rank", y = " ")+scale_shape_manual(values = c(1,17, 2))+ geom_text_repel(aes(label=Variable),hjust=0.5, vjust=-0.5, size=4)
g7<-ggplot(data= Highw, aes(x=year.rank, y=S)) +geom_point(aes(shape=Type),size=2.5)+theme_classic(base_size = 14)+theme(panel.border = element_rect(colour = "black", fill=NA, size=1), strip.background = element_blank(),strip.text.x = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), aspect.ratio= 1, legend.position="none") +scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2),expand=c(0,0))+labs(x="Rank", y =" ")+scale_shape_manual(values = c(1,17, 2))+ geom_text_repel(aes(label=Variable),hjust=0.5, vjust=-0.5, size=4)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
g2
g3
g4
g5
g6
g7

#
library(ggrepel)

###maps
library(maps)
library(mapdata)

latlon<-read.csv(file="C:/SynchronyR/stream_sites.csv", header = TRUE)
map("worldHires", "Canada", xlim=c(-85, -75), ylim= c(40,50))
points(latlon$lon, latlon$lat, pch= 19, col= "black", cex= 0.5)
map.axes(cex.axis=0.8)
#inmap
par(usr=c(-85, -75, 40, 50))
rect(xleft =-80.5,ybottom = 42.5,xright = -75.5,ytop = 40.2,col = "white")
plot(Ontario, xlim=c(-80.5,-75.5), ylim=c(40.2,42.5),add=T)

map('province', fill = FALSE, xlim = c(-125, -114), ylim = c(32.2, 42.5), xlab = "lon", ylab = "lat")
map.axes(cex.axis=0.8)

points(latlon$lon, latlon$lat, pch= 19, col= "black", cex= 0.5)

maps::map.scale(x=-124, y=34, ratio=FALSE, relwidth=0.3)
north.arrow(xb=-116, yb=41, len=0.22, lab="N") 

# Inmap
par(usr=c(-216, -63, 22, 144))
rect(xleft =-126.2,ybottom = 23.8,xright = -65.5,ytop = 50.6,col = "white")
map("usa", xlim=c(-126.2,-65.5), ylim=c(23.8,50.6),add=T)
map("state", xlim=c(-126.2,-65.5), ylim=c(23.8,50.6),add=T, boundary = F, interior = T, lty=2)
map("state", region="california", fill=T, add=T)
points(-121.6945, 39.36708, bg = "white", pch = 21)

#####box plots
DOC<-read.csv(file="C:/SynchronyR/AllYearsDOC.csv", header = TRUE)
TDN<-read.csv(file="C:/SynchronyR/AllYearsTDN.csv", header = TRUE)
DIS<-read.csv(file="C:/SynchronyR/AllYearsDisch.csv", header = TRUE)
TP<-read.csv(file="C:/SynchronyR/AllYearsTP.csv", header = TRUE)
FI<-read.csv(file="C:/SynchronyR/combined_FI.csv", header = TRUE)

mdata<- as.data.frame(t(dat1))
mdata[mdata==""]<- NA 
colnames(mdata) <- as.character(unlist(mdata[1,]))
mdata = mdata[-1, ]
mdata = mdata[-1, ]
rownames(mdata) <- 1:nrow(mdata)
mdata[1:45] <- lapply(mdata[1:45], as.character)
mdata[1:45] <- lapply(mdata[1:45], as.numeric)
mdata <- mdata[,colSums(is.na(mdata))<nrow(mdata)]
mdata<-mdata[rowSums(is.na(mdata)) != ncol(mdata),]

DOC<-stack(mdata)
DOC$DOC<-"DOC"
TDN<-stack(mdata)
TDN$TDN<-"TDN"
DIS<-stack(mdata)
DIS$DIS<-"DIS"
DIS$values[DIS$values < 0] <- 0
TP<-stack(mdata)
TP$TP<-"TP"
FI<- stack(mdata)
FI$FI<-"FI"

boxdata<-cbind.data.frame(DOC$values, TDN$values, TP$values, DIS$values)
colnames<-c("DOC", "TDN", "TP", "DIS")
names(boxdata)<-colnames


library(ggplot2)
library(reshape2)
datmelt <- melt(boxdata, value.name="Value")
g <- ggplot(datmelt, aes(variable, Value))+geom_boxplot()+ theme_classic(base_size = 18)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 1, axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
g+facet_grid(.~variable, scales="free_y")

g <- ggplot(DOC, aes(DOC, values))+geom_boxplot()+ theme_classic(base_size = 18)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 1, axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +scale_y_continuous(limits=c(0, 45), breaks=seq(0,45,5), expand=c(0,0))+labs(y=expression(DOC~(mg~C~L^-1)))


#DOC
g <- ggplot(DOC, aes(DOC, values))+geom_boxplot()+ theme_classic(base_size = 24)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 3, axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +scale_y_continuous(limits=c(0, 50), breaks=seq(0,50,10), expand=c(0,0))+labs(y=expression(DOC~(mg~-C~L^-1)))
#TDN
g <- ggplot(TDN, aes(TDN, values))+geom_boxplot()+ theme_classic(base_size = 24)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 3, axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +scale_y_continuous(limits=c(0, 10), breaks=seq(0,10,1), expand=c(0,0))+labs(y=expression(TDN~(mg~-N~L^-1)))
#TP
g <- ggplot(TP, aes(TP, values))+geom_boxplot()+ theme_classic(base_size = 24)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 3, axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +scale_y_continuous(limits=c(0,800), breaks=seq(0,800,100), expand=c(0,0))+labs(y=expression(TP~(ug~-P~L^-1)))
#DIS
g <- ggplot(DIS, aes(DIS, values))+geom_boxplot()+ theme_classic(base_size = 24)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 3, axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +scale_y_continuous(limits=c(0, 15), breaks=seq(0,15,5), expand=c(0,0))+labs(y=expression(Discharge~m^3~s^-1))
stat_boxplot(geom ='errorbar')
#FI
g <- ggplot(FI, aes(FI, values))+geom_boxplot()+ theme_classic(base_size = 24)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 3, axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5), expand=c(0,0))+labs(y="Fluorescence Index")

####boxplots for synchrony
library(reshape2)
results<-read.csv(file="C:/SynchronyR/synchrony_results_bplot2.csv", header = TRUE)
rlong <- melt(results, id.vars="Variable", value.name="S")
rlong2<-transform(rlong, year.rank = ave(S, c(variable, Variable), FUN = function(x) rank(-x, ties.method = "first")))
sites<-split(rlong2, rlong2$variable)
All<-sites$All
All$Type<-"NA"
All$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM")
Lowag<-sites$LowAg
Lowag$Type<-"NA"
Lowag$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM")
Intag<-sites$IntAg
Intag$Type<-"NA"
Intag$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM","DOM" )
Highag<-sites$HighAg
Highag$Type<-"NA"
Highag$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM")
Loww<-sites$LowW
Loww$Type<-"NA"
Loww$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM")
Highw<-sites$HighW
Highw$Type<-"NA"
Highw$Type<- c("Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Limnological", "Physical", "Physical", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM", "DOM")

alldata<-rbind.data.frame(All, Highag, Intag, Lowag, Highw, Loww)

vars<-split(alldata, alldata$Type)

Limno<-vars$Limnological
DOM<-vars$DOM
Phys<-vars$Physical

library(ggplot2)
###to add a line between words on x axis tick mark labels
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}
g <- ggplot(Phys, aes(variable, abs(S)))+stat_boxplot(geom ='errorbar')+geom_boxplot()+ theme_classic(base_size = 20)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 1,axis.ticks.x=element_blank())+ scale_x_discrete("Land use", breaks=c("All", "HighAg", "IntAg", "LowAg", "HighW", "LowW"), labels=addline_format(c("all", "high ag", "med ag", "low ag", "high wet", "low wet"))) +labs(y="Synchrony") + scale_y_continuous(limits=c(0,1), breaks=seq(0,1, 0.25), expand=c(0,0))
g

gl <- ggplot(Limno, aes(variable, abs(S)))+stat_boxplot(geom ='errorbar')+geom_boxplot()+ theme_classic(base_size = 20)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 1,axis.ticks.x=element_blank())+ scale_x_discrete("Land use", breaks=c("All", "HighAg", "IntAg", "LowAg", "HighW", "LowW"), labels=addline_format(c("all", "high ag", "med ag", "low ag", "high wet", "low wet"))) +labs(y="Synchrony") + scale_y_continuous(limits=c(0,1), breaks=seq(0,1, 0.25), expand=c(0,0))
gl

gd <- ggplot(DOM, aes(variable, abs(S)))+stat_boxplot(geom ='errorbar')+geom_boxplot()+theme_classic(base_size = 20)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 1,axis.ticks.x=element_blank())+ scale_x_discrete("Land use", breaks=c("All", "HighAg", "IntAg", "LowAg", "HighW", "LowW"), labels=addline_format(c("all", "high ag", "med ag", "low ag", "high wet", "low wet"))) +labs(y="Synchrony") + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.25), expand=c(0,0))
gd

###adding a parafac boxplot
results2<-read.csv(file="C:/SynchronyR/synchrony_results_PARA.csv", header = TRUE)
rlonga <- melt(results2, id.vars="Variable", value.name="S")

gp <- ggplot(rlonga, aes(variable, abs(S)))+stat_boxplot(geom ='errorbar')+geom_boxplot()+ theme_classic(base_size = 20)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background = element_blank(), aspect.ratio = 1,axis.ticks.x=element_blank())+ scale_x_discrete("Land use", breaks=c("All", "HighAg", "IntAg", "LowAg", "HighW", "LowW"), labels=addline_format(c("all", "high ag", "med ag", "low ag", "high wet", "low wet"))) +labs(y="Synchrony") + scale_y_continuous(limits=c(0,1), breaks=seq(0,1, 0.25), expand=c(0,0))
gp

###############scatter plots for conc vs land use#########
mdata<-read.csv(file="C:/SynchronyR/Landuseandconc.csv", header = TRUE)
library(ggplot2)

##TDN and cropland
m1 <- lm(mdata$TDNavg ~ mdata$Cmono)
summary(m1)

s2<-ggplot(mdata, aes(Cmono, TDNavg))
s2<-s2+geom_point(size = 2)
s2<-s2+theme_classic(base_size= 12)+theme(axis.line.x = element_line(color="black", size = 1),axis.line.y = element_line(color="black", size = 1), aspect.ratio = 1)+ labs(fill="Site", y=expression('TDN'~mg~L^-1), x="% Monoculture")+ scale_y_continuous(limits= c(0, 5),breaks=seq(0, 5, 1), expand=c(0,0))+ scale_x_continuous(limits= c(0, 100),breaks=seq(0, 100, 20), expand=c(0,0))
s2<-s2+geom_smooth(method = "lm", se = FALSE, color= "black")+geom_text(aes(80, 4, label = paste("R2 = 0.70", "\n","P = 4.9e-13")))
s2


##DOC and wetland
m2 <- lm(mdata$DOCavg ~ mdata$CWetlaNA)
summary(m2)

s3<-ggplot(mdata, aes(CWetlaNA, DOCavg))
s3<-s3+geom_point(size = 2)
s3<-s3+theme_classic(base_size= 12)+theme(axis.line.x = element_line(color="black", size = 1),axis.line.y = element_line(color="black", size = 1), aspect.ratio = 1)+ labs(fill="Site", y=expression('DOC'~mg~L^-1), x="% Wetland")+ scale_y_continuous(limits= c(0, 20),breaks=seq(0, 20, 5), expand=c(0,0))+ scale_x_continuous(limits= c(0, 100),breaks=seq(0, 100, 20), expand=c(0,0))
s3<-s3+geom_smooth(method = "lm", se = FALSE, color= "black")+geom_text(aes(60, 16, label = paste("R2 = 0.39", "\n","P = 0.00003")))
s3

##cropland and wetland
m <- lm(mdata$CWetlaNA ~ mdata$Cmono)
summary(m)

s4<-ggplot(mdata, aes(Cmono, CWetlaNA))
s4<-s4+geom_point(size = 2)
s4<-s4+theme_classic(base_size= 12)+theme(axis.line.x = element_line(color="black", size = 1),axis.line.y = element_line(color="black", size = 1), aspect.ratio = 1)+ labs(fill="Site", y="% Wetland", x="% Monoculture")+ scale_y_continuous(limits= c(0, 100),breaks=seq(0, 100, 20), expand=c(0,0))+ scale_x_continuous(limits= c(0, 100),breaks=seq(0, 100, 20), expand=c(0,0))
s4<-s4 +geom_smooth(method = "lm", se = FALSE, color= "black")+geom_text(aes(65, 60, label = paste("R2 = 0.28", "\n","P = 0.0001")))
s4


###make panels
library(ggpubr)
ggarrange(s2, s3, s4 + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)
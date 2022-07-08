#synchrony DOM PCA
library(reshape2)
setwd("/Users/cearatalbot/Rcode/SynchronyDOM/CearaFormattedFiles//")#set wd

allsites<-c("Martin Creek", "Pearns (Perrin) Creek", "Tributary to Mariposa brook",
            "Layton River", "Hawkstone Creek",	"Zephyr Creek",  "Emily Creek", 
            "Beaver River Upstream",	"Meade Creek", "Coldwater Creek",	
            "Humber River", "Indian River", "Pigeon River",	"McLaren Creek", 
            "Staples River", "Beaverton River or Creek" ,"Mariposa Brook/Scugog", 
            "Whytes Creek", "Talbot River", "Bluffs Creek",  "Schomberg River", 
            "Maskinonge (Jersey) River",	"Uxbridge Brook", 
            "Scugog River", "Lower Nottawasaga River",	"Sheldon Creek", 
            "Innisfil Creek", "Baxter Creek", "Jackson Creek",
            "Boyne River", "Fleetwood Creek",	"Cavanville Creek","East Cross Creek",
            "Nonquon River", "Teeswater", "Thames River", "Penville Creek", 
            "Tributary to Boyne River", "Tributary to Schomber Canal", 
            "Ausable River", "Fish Creek", "Beenton Creek", "Black River")

# A function that subsets data frame into land use groups using matching columns 
# Usage: pullsites(df, sites), df= data frame to take columns from, sites=a list of sites to match with columns 
pullSites<-function(df, sites){
  x<-df[which(df[,1]==sites[1]),] #find strean names in column 1 that match names in site list
  for(i in 2:length(sites)){ 
    z<-df[which(df[,1]==sites[i]),]
    x<-rbind(x,z)} #combine each row into a new data frame
  return(x)
}
fiList<-c("A350", "SR","HIX", "FI", "SUVA254", 
          "E280", "BA")
for(i in 1:7){
df<-read.csv(paste("C", i, "data.csv", sep=""), stringsAsFactors=F) #read individual files
df<-pullSites(df=df, sites=allsites) #subset to get our sites 
df$June_2017<-(df$June_2017*100) ##fixing division error in files
df$September_2017<-(df$September_2017*100)##
df$Component<-paste("C", i, sep="") #label component
df<-df[,-2] #rm stream name
assign(paste("C", i, sep=""), df) #rename
}

for(i in 1:7){
  df<-read.csv(paste(fiList[i], "data.csv", sep=""), stringsAsFactors=F) #read individual files
  df<-pullSites(df=df, sites=allsites) #subset to get our sites 
  df$Component<-fiList[i] #label component
  df<-df[,-2] #rm stream name
  assign(fiList[i], df) #rename
}
#there is a division error in E280 and SUVA254 on May 2006 date; remove it
E280$May_2006<-"NA"
SUVA254$May_2006<-"NA"
mydata<-rbind(C1, C2, C3, C4, C5, C6, C7, A350, 
              SR, HIX, FI, SUVA254, E280, BA) #check: if columns don't match, this won't work
mydata[mydata=="MISSING"] <- NA #replace cells labeled "MISSING" with NA
mydata[mydata==""] <- NA #replace blank cells with NA
#mydata<-mydata[,colSums(is.na(mydata)) < nrow(mydata)] 
#mydata<-mydata[complete.cases(mydata[,2:(ncol(mydata)-1)]),]


#mydata_long<-melt(mydata, by=list(c("Component", "Stream")))
mydata_long<-melt(mydata, id.vars = c ("Component", "Stream"))
mydata_long$value<-as.numeric(mydata_long$value)
#mydata_wide<-dcast(mydata_long, Stream+variable+value~Component, fun.aggregate = mean) #doesnt actually calculate a mean, just gives value since each combo of vars only has 1 val
mydata_wide<-dcast(mydata_long, Stream+variable~Component, value.var = "value", fun.aggregate = mean) #doesnt actually calculate a mean, just gives value since each combo of vars only has 1 val
colnames(mydata_wide)[4]<-"\u03B2:\u03B1"
modelPCA<-prcomp(na.omit(mydata_wide[,3:16]), scale. = T)
dfPCA<-summary(modelPCA)
#write.csv(dfPCA, "PCA_out.csv", row.names=F )
#theme
borderTheme0.5<-theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                      panel.background = element_rect(colour = "black", size=1, fill=NA), 
                      strip.background = element_blank(), 
                      panel.spacing = unit(0.3, "lines"), axis.ticks.length = unit(0.2, "lines"), 
                      text = element_text(size = 14),legend.background=element_blank(), 
                      legend.key = element_rect(fill = NA), axis.text.x=element_text())

library(ggfortify)
#labels don't work
labspca<-c(expression("A"[350]), "\u03B2:\u03B1", "C1", "C2", "C3", "C4", "C5", "C6", "C7", expression("E"[280]), "FI", "HIX", "SR", expression("SUVA"[254]))
pcaplot<-autoplot(modelPCA, loadings = TRUE, loadings.label=T, loadings.label.hjust=1, loadings.label.vjust=-0.5, loadings.label.size=3, loadings.label.colour="black", loadings.colour="black",geom="point", colour="grey")+borderTheme0.5
pcaplot
ggsave(filename = "PCA_plot.png", plot=pcaplot, width = 4, height = 3, units= "in", device='png', dpi=320)
#pcaplot<-autoplot(modelPCA, loadings = TRUE, loadings.label=T, loadings.label.hjust=1.3, loadings.label.size=3)
#ggsave(filename = "PCA_plot_noSUVA.png", plot=pcaplot, width = 4, height = 3, units= "in", device='png', dpi=320)

box1<-ggplot(mydata_long, aes(x=Component,y=value))+geom_boxplot()+scale_y_continuous("Value")+theme_bw()
ggsave(filename = "boxplot.png", plot=box1, width = 4, height = 3, units= "in", device='png', dpi=320)

#there is a negative value in BA
#outliers
q25<-summary(mydata_wide$E280)[[2]]
q75<-summary(mydata_wide$E280)[[5]]
qlow<-summary(mydata_wide$E280)[[2]]-1.5*IQR(mydata_wide$E280, na.rm=T)
qhigh<-summary(mydata_wide$E280)[[5]]+1.5*IQR(mydata_wide$E280, na.rm=T)

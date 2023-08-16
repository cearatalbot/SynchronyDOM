####Separating synchrony years based on start

#set working directory
setwd("/Users/cearatalbot/Rcode/SynchronyDOM/updatedDOM/") #file path to folder where you have the data files

#load packages
library(synchrony) #this package contains the synchrony functions
library(plyr) #contains match_df function
library(reshape2)
library(ggplot2)
library(ape)
source("synchrony/sFunctions.R") #supporting functions for analysis

########SETUP SPECIFIC TO DATASET#####
siteLookup<-read.csv("rawData/SiteNameNum.csv", stringsAsFactors = F)
data<-read.csv("rawData/ASN_allData.csv", stringsAsFactors = F)#all data
landUse<-read.csv("Out/landUseGroups.csv", stringsAsFactors = F)#all data

mixed<-as.character(landUse[which(landUse$landUse=="mixed"), 11])
wetDominated<-as.character(landUse[which(landUse$landUse=="wetDominated"), 11])
agDominated<-as.character(landUse[which(landUse$landUse=="agDominated"), 11])
allsites<-as.character(landUse$SiteNum)

#data to be included in PCA
pcaVars<-c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "A350", 
           "SR", "HIX.OHNO", "FI", "SUVA254", "E280", "BA")
#make a list of site lists
siteList<-list(allsites=allsites, wetDominated=wetDominated, 
               mixed=mixed, agDominated=agDominated)


#some sites have just site number and some have "ASN" + site number
#remove ASN in front of site number
for(i in 1:nrow(data)){
  if(substr(data$Sample.Name[i], 1,3)=="ASN"){
    data$Sample.Name[i]<-substr(data$Sample.Name[i], 4, nchar(data$Sample.Name[i]))
  } 
}
#colnames(data)
#remove/rename variables
colnames(data)[5]<-"DOC"
data$HIX<-NULL
data$FieldNotes<-NULL
data$X<-NULL
data$X.1<-NULL
data$Time<-NULL
data$E280_oldmeth<-NULL
data$Salinity<-NULL
data$NH4<-NULL
data$NO3<-NULL
data$DIC<-NULL

data[which(data$SR > 6),18]<-NA #outlier in SR

data$Cth<-data$C1+data$C2+data$C3#make grouped PARAFAC terrestrial-humic
data$Cmh<-data$C5+data$C6 
data$Cmp<-data$C7
data$Cf<-data$C4

mons<-c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
monCh<-c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
monChab<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov")
vars<-list() #empty list to store data frames for each variable


for(i in 5:ncol(data)){
  #stream Num, date1, date2, etc. 
  dfsub<-data [,c(1,2,4,i)] #get one variable at a time
  dfsub$monYr<-NA 
  for(x in 1:nrow(dfsub)){ #loop to get dates in new format
    dfsub$monYr[x]<-paste(mons[which((dfsub$Month[x]==monCh | dfsub$Month[x]==monChab))], dfsub[x,3], sep="-") #combine Date
  }
  dfsub<-dfsub[,-2:-3] #remove old date columns
  dfsub[,1]<-as.numeric(dfsub[,1]) #get rid of leading 0's
  dfsub<-dfsub[-which(is.na(dfsub$Sample.Name)),] #remove NA rows
  dfsub[,2]<-as.numeric(dfsub[,2]) 
  dfsub[is.na(dfsub)] <- -999 #replace NA with -999
  #new data.frame for transformed data
  df<-data.frame(matrix(ncol=(1+length(unique(dfsub$monYr))), nrow=length(unique(dfsub$Sample.Name, na.rm=T))))
  colnames(df)<-c("Site", unique(dfsub$monYr)) #column names
  df[,1]<-unique(dfsub$Sample.Name, na.rm=T) #site numbers
  #manually convert long to wide
  for(x in 1:nrow(df)){
    for(z in 2:ncol(df)){
      if(length(dfsub[which((dfsub[,1]==df[x,1]) & (dfsub[,3]==colnames(df)[z])),2])>0){
        df[x,z]<-dfsub[which((dfsub[,1]==df[x,1]) & (dfsub[,3]==colnames(df)[z])),2] 
      } else{
        df[x,z]<-as.numeric(-999)
      } #end if else
    } #end z loop
  } #end x loop
  ###z-score
  dfSite<-data.frame(Site=df$Site) #save site IDs
  dates<-colnames(df) #save column names
  df[df==-999.00] <- NA  #return to NA
  df<-as.matrix(df[,-1]) #get data without sites
  class(df)<-"numeric" ##back to numeric 
  df<-ScoreVals(df) #z-score
  df<-cbind(dfSite,df) #combine z-scored data with site ID
  colnames(df)<-dates 
  df[is.na(df)] <- -999 #return NA to -999 value
  vars[[i-4]]<-df
  names(vars)[i-4]<-colnames(dfsub)[2]
}


AllFiles<-list() #empty list to store all files subsetted by land use
AllFilesSub<-list() #empty list

subyears<-c("2006", "2007")
#subyears<-c("2006", "2007", "2011", "2013", "2015", "2016", "2017", "2018", "2019")
# a loop that formats input data for the synchrony analysis
for(i in 1:length(vars)){   
  DF1<-vars[[i]]
  if(names(vars)[i]=="SUVA254"){  #division error in May 2006 for SUVA254 and E280
    DF1$`05-2006`<-NULL
  } else if(names(vars)[i]=="E280"){
    DF1$`05-2006`<-NULL
  }
  
  for(y in 1:length(subyears)){
    if(y==1){
    DF_y<-cbind(Site=DF1[,1], DF1[,which(substr(names(DF1), 4, 7)==subyears[y])])
    }else{
    DF_y<-cbind(DF_y, DF1[,which(substr(names(DF1), 4, 7)==subyears[y])])
    }
  } 
  
  DF<- as.data.frame(t(DF_y)) #transpose to make river names the column names (i.e. long to wide format)
  colnames(DF) <- as.character(unlist(DF[1,])) #use stream column to rename columns
  #DF <- DF[-1, ] #delete the site number column
  DF$Date<-row.names(DF) #make a new column for the sampling date 
  row.names(DF)<-seq(1, nrow(DF), by=1) #rename rows 
  
  DF[DF==-999.00] <- NA #replace blank cells with NA
  DF[1:(ncol(DF)-1)] <- lapply(DF[1:(ncol(DF)-1)], as.character) #make data character
  DF[1:(ncol(DF)-1)] <- lapply(DF[1:(ncol(DF)-1)], as.numeric) #this will have a warning message, it's okay. It is telling you that the blanks were replaced with "NA"
  DF[1:(ncol(DF)-1)]<-lapply(DF[1:(ncol(DF)-1)], round, digits=3) #round to the number of digits in the original data
  
  #DF<- DF[,colSums(is.na(DF))<nrow(DF)] #remove streams with no data
  #DF<-DF[rowSums(is.na(DF)) != (ncol(DF)-1),] #remove dates with no data
  
  #loop function over all variables and subset for all land cover groups
  for(y in 1:length(siteList)){
    DFsub<-pullSites(df=DF, sites=siteList[[y]]) #get sites for each LU group
    DFsub$Date<-DF$Date
    AllFilesSub[[y]]<-DFsub
    names(AllFilesSub)[y]<-paste(names(vars[i]), names(siteList[y]), sep="") #change name of data in list to the same name as above
    AllFiles[[i]]<-AllFilesSub #append full list with new data
    names(AllFiles)[i]<-paste(names(vars[i])) #rename in appended list
  }
}

##############synchrony analysis###########
#empty DF to store results 
SynchronyResults<-data.frame(matrix(ncol=4,nrow=0)) #make an empty data frame with 4 cols and 0 rows
colnames(SynchronyResults)<-c("Variable", "Group", "S", "pVal") #rename columns

it<-0 #for storing PCA data
#loop for analysis
for(z in 1:length(AllFiles)){
  subVars<-AllFiles[[z]] #get subsetted data sets for one variable at a time
  try(
    for(b in 1:length(subVars)){
      df<-subVars[[b]]
      df<-df[-1,] #remove row with names
      for(y in 1:nrow(df)){
        df$Date[y]<-paste("01-",df$Date[y], sep="")
      }
      df<-df[order(as.Date(df$Date, format="%d-%m-%Y")),]
      df<-as.matrix(df) #get an individual data set 
      
      newDF<-as.data.frame(df) ##new data frame for plots later
      newDF$Group<-names(subVars[b]) ##group label for plots
      
      NAME<-paste(names(subVars[b]), "ts", sep="_") ##rename files so I can keep it
      assign(NAME, newDF) ##
      
      row.names(df)<-df[,ncol(df)]
      df<-df[,-ncol(df)] ##remove date column
      df<-df[,colSums(is.na(df))< nrow(df)] #remove streams with no data
      df<-df[rowSums(is.na(df)) != ncol(df),] #remove dates with no data
      for(i in 1:nrow(df)){
        try(
          df[i,]<-ifelse(which(sum(is.na(df[i,])) >= (ncol(df)/2)) == TRUE, NA, print("good"))
          , silent=T)} #this removes rows where more than half of the data is missing
      
      df<-df[,colSums(is.na(df))<nrow(df)] # again, remove streams with no data 
      df<-df[rowSums(is.na(df)) != ncol(df),] # again, remove dates with no data
      
      #remove any empty rows
      for(p in 1:nrow(df)){
        rowNA<-sum(is.na(df[p,])==TRUE)
        if(rowNA==ncol(df)){
          df<-df[-p,]
        }
      }
      class(df)<-"numeric"
      
      ###CODE TO EXTRACT DATA FOR PCA TO KEEP PRE-PROCESSING CONSISTENT
      for(p in 1:length(pcaVars)){
        if(names(AllFiles)[z]==pcaVars[p]){
          if(b==1){
            it=it+1
            dfp<-data.frame(df)
            colnames(dfp)<-colnames(df)
            dfp$Component<-names(AllFiles)[z]
            dfp$Date<-unlist(row.names(dfp))
            dfpLong<-melt(dfp, id.vars = c("Component", "Date"))
            
            if(it==1){
              dfOne<-dfpLong
            } else{
              dfOne<-rbind(dfOne, dfpLong)
            }
          }
        } #end if
      }#end p loop/GET DATA FOR PCA
      df<-na.omit(df)#######
      results<-meancorr(data=df,nrands=999,alternative= "two.tailed",type=1,quiet=TRUE) #do the analysis
      SynchronyResults1<-data.frame(matrix(ncol=4,nrow=0)) #temporary data frame to store results
      
      colnames(SynchronyResults1)<-c("Variable", "Group", "S", "pVal") #you'll fill these columns next
      SynchronyResults1[1,1]<-names(subVars[b]) #data name 
      SynchronyResults1[1,2]<-names(subVars[b]) #data name
      SynchronyResults1[1,3]<-results$obs #this is the synchrony value, S
      SynchronyResults1[1,4]<-results$pval #this is the p value
      SynchronyResults<-rbind(SynchronyResults, SynchronyResults1) #append the data frame for all results
    })
}#end synchrony loop

########PCA##########
dfOne$value<-as.numeric(dfOne$value) #DOM indices we extracted above
mydata_wide<-dcast(dfOne, variable+Date~Component) #convert to wide
colnames(mydata_wide)[4]<-"\u03B2:\u03B1" #rename BA to greek characters for labels
modelPCA<-prcomp(na.omit(mydata_wide[,3:9]), scale. = F) #run PCA, data is already z-transformed so no scaling
dfPCA<-summary(modelPCA) 

#ggplot plot theme
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
#ggsave(filename = "Figures/PCA.png", plot=pcaplot, width = 4, height = 3.5, units= "in", device='png', dpi=320)

loadings <- modelPCA$rotation #get loadings
axes <- predict(modelPCA, newdata=mydata_wide[,3:9]) #predict PC1 and PC2 for each stream at each date
newData<-cbind(mydata_wide, axes[,1:2]) #recombine with other DOM data

PC1<-newData[,c(1:2,10)]  #format PC data
PC1_wide<-dcast(PC1, Date~variable)
PC2<-newData[,c(1:2,11)]
PC2_wide<-dcast(PC2, Date~variable)

pcList<-list() #setup for synchrony analysis
pcList[[1]]<-PC1_wide
pcList[[2]]<-PC2_wide
names(pcList)<-c("PC1", "PC2")
PCFilesSub<-list()
PCFiles<-list()

#group sites
for(i in 1:2){
  DF<-pcList[[i]]
  for(y in 1:length(siteList)){
    DFsub<-pullSites(df=DF, sites=siteList[[y]]) #get sites for each LU group
    DFsub$Date<-DF$Date
    PCFilesSub[[y]]<-DFsub
    names(PCFilesSub)[y]<-paste(names(pcList)[i], names(siteList)[y], sep="") #change name of data in list to the same name as above
    PCFiles[[i]]<-PCFilesSub #append full list with new data
    names(PCFiles)[i]<-paste(names(pcList)[i]) #rename in appended list
  }
}

#####run synchrony on list of grouped data#####
for(z in 1:length(PCFiles)){
  subVars<-PCFiles[[z]] 
  for(b in 1:length(subVars)){
    df<-as.matrix(subVars[[b]]) #get an individual data set 
    df<-df[,1:(ncol(df)-1)] #remove date column
    
    df<-df[,colSums(is.na(df[,1:ncol(df)]))< nrow(df[,1:ncol(df)])] #remove streams with no data
    df<-df[rowSums(is.na(df[,1:ncol(df)])) != ncol(df[,1:ncol(df)]),] #remove dates with no data
    #synchrony
    class(df)<-"numeric"
    results<-meancorr(data=df,nrands=999,alternative= "two.tailed",type=1,quiet=TRUE) #do the analysis
    SynchronyResults1<-data.frame(matrix(ncol=4,nrow=0)) #temporary data frame to store results
    
    colnames(SynchronyResults1)<-c("Variable", "Group", "S", "pVal") #you'll fill these columns next
    SynchronyResults1[1,1]<-names(pcList)[z] #data name 
    SynchronyResults1[1,2]<-names(subVars[b]) #data name
    SynchronyResults1[1,3]<-results$obs #this is the synchrony value, S
    SynchronyResults1[1,4]<-results$pval #this is the p value
    SynchronyResults<-rbind(SynchronyResults, SynchronyResults1) #append the data frame for all results
    
  }
}

write.csv(SynchronyResults, "Out/synchronyResults_subHydro_spring.csv", row.names=F)


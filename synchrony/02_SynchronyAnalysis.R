####Stream synchrony for 45 streams in Southern Ontario, Canada####
#By CJT, updated Aug 16, 2024

#set working directory
setwd(" ") #file path to folder where you have the data files

#load packages
library(synchrony) 
library(plyr) 
library(reshape2)
library(ape)
source("synchrony/sFunctions.R") #supporting functions for analysis

########SETUP SPECIFIC TO DATASET#####
data<-read.csv("rawData/ASN_AllData_Synchrony.csv", stringsAsFactors = F)#all data
landUse<-read.csv("Out/landUseGroups.csv", stringsAsFactors = F)#all data

mixed<-as.character(landUse[which(landUse$landUse=="mixed"), 11])
wetDominated<-as.character(landUse[which(landUse$landUse=="wetDominated"), 11])
agDominated<-as.character(landUse[which(landUse$landUse=="agDominated"), 11])
allsites<-as.character(landUse$SiteNum)

siteList<-list(allsites=allsites, wetDominated=wetDominated, 
               mixed=mixed, agDominated=agDominated)

mons<-c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
monCh<-c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
monChab<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov")

vars<-list() #empty list to store data frames for each variable
for(i in 5:ncol(data)){
  #stream Num, date1, date2, etc. 
  dfsub<-data[,c(1,3,4,i)] #get one variable at a time
  dfsub$monYr<-paste(dfsub$MONTH, dfsub$YEAR, sep="-")
  dfsub<-dfsub[,-2:-3] #remove old date columns
  dfsub<-dfsub[-which(is.na(dfsub[,2])),] #remove NA rows
  dfsub[,2]<-ave(as.numeric(dfsub[,2]), dfsub[,1], FUN=scale) #z-score
  #dfsub[is.na(dfsub)] <- -999 #replace NA with -999
  #new data.frame for transformed data
  df<-data.frame(matrix(ncol=(1+length(unique(dfsub$monYr))), nrow=length(unique(dfsub$ASN.SITE.NO, na.rm=T))))
  colnames(df)<-c("Site", unique(dfsub$monYr)) #column names
  df[,1]<-unique(dfsub$ASN.SITE.NO, na.rm=T) #site numbers
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
###All data should now be in a list called vars and separated by variable

########SUBSET THE DATA BY LAND USE GROUP##########
AllFiles<-list() #empty list to store all files subsetted by land use
AllFilesSub<-list() #empty list

# a loop that formats input data for the synchrony analysis
for(i in 1:length(vars)){   
  DF1<-vars[[i]]
  DF1$`10-2004`<-NULL #these dates have a lot of missing data for various reasons.. and were removed for all streams.
  DF1$`04-2005`<-NULL 
  DF1$`06-2005`<-NULL
  DF1$`07-2005`<-NULL
  DF1$`08-2005`<-NULL
  DF1$`09-2005`<-NULL 
  DF1$`11-2005`<-NULL
  DF1$'9-2007'<-NULL #check.. many vars not sampled
  DF1$'11-2007'<-NULL #check...redone
  DF1$'9-2009'<-NULL #check
  DF1$'8-2012'<-NULL #only TDN was measured
  DF1$`08-2015`<-NULL 
  DF1$`06-2017`<-NULL #many streams not sampled
  DF1$`08-2017`<-NULL #many vars and streams not sampled
  DF1$`09-2017`<-NULL #many streams not sampled
  DF1$`05-2019`<-NULL #many streams not sampled
  
  DF<- as.data.frame(t(DF1)) #transpose to make river names the column names (i.e. long to wide format)
  colnames(DF) <- as.character(unlist(DF[1,])) #use stream column to rename columns
  DF$Date<-row.names(DF) #make a new column for the sampling date 
  row.names(DF)<-seq(1, nrow(DF), by=1) #rename rows 
  
  DF[DF==-999.00] <- NA #replace blank cells with NA
  DF[1:(ncol(DF)-1)] <- lapply(DF[1:(ncol(DF)-1)], as.character) #make data character
  DF[1:(ncol(DF)-1)] <- lapply(DF[1:(ncol(DF)-1)], as.numeric) #this will have a warning message, it's okay. It is telling you that the blanks were replaced with "NA"
  DF[1:(ncol(DF)-1)]<-lapply(DF[1:(ncol(DF)-1)], round, digits=3) #round to the number of digits in the original data
  
  #loop function over all variables and subset for all land cover groups
  for(y in 1:length(siteList)){
    DFsub<-pullSites(df=DF, sites=siteList[[y]]) #get sites for each LU group
    DFsub$Date<-DF$Date
    AllFilesSub[[y]]<-DFsub
    names(AllFilesSub)[y]<-paste(names(vars[i]), names(siteList[y]), sep="_") #change name of data in list to the same name as above
    AllFiles[[i]]<-AllFilesSub #append full list with new data
    names(AllFiles)[i]<-paste(names(vars[i])) #rename in appended list
  }
}

##############synchrony analysis###########
#empty DF to store results 
SynchronyResults<-data.frame(matrix(ncol=5,nrow=0)) #make an empty data frame with 4 cols and 0 rows
colnames(SynchronyResults)<-c("Label", "Group", "S", "pVal", "n") #rename columns

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
    df<-df[order(as.Date(df$Date, format="%d-%m-%Y")),] #format date
    df<-as.matrix(df) #get an individual data set 

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
    
    df<-na.omit(df)#######
    results<-meancorr(data=df,nrands=999,alternative= "two.tailed",type=1,quiet=TRUE) #do the analysis
    SynchronyResults1<-data.frame(matrix(ncol=5,nrow=0)) #temporary data frame to store results

    colnames(SynchronyResults1)<-c("Label", "Group", "S", "pVal", "n") #you'll fill these columns next
    SynchronyResults1[1,1]<-names(subVars[b]) #data name 
    SynchronyResults1[1,3]<-results$obs #this is the synchrony value, S
    SynchronyResults1[1,4]<-results$pval #this is the p value
    SynchronyResults1[1,5]<-length(which(!is.na(df)))
    SynchronyResults<-rbind(SynchronyResults, SynchronyResults1) #append the data frame for all results
    })
}#end synchrony loop

###error in cor() will occur when variables do not have enough datapoints for analysis,
###they will not show up in the results table. There are two instances of this. 

SynchronyResults$LUgroup<-matrix(unlist(matrix(strsplit(SynchronyResults$Label, split="_"))),ncol=2,byrow=T)[,2]
SynchronyResults$var<-matrix(unlist(matrix(strsplit(SynchronyResults$Label, split="_"))),ncol=2,byrow=T)[,1]

#write.csv(SynchronyResults, "Out/synchronyResults_2024Apr18.csv", row.names=F)


##############################
####Moran's i calculated from z-scored values
spaceData<-read.csv("rawData/streamLatLon.csv", stringsAsFactors = F)#all data
spaceVars<-AllFiles

for(z in 1:length(spaceVars)){
  subVars<-spaceVars[[z]] 
  for(b in 1:length(subVars)){
    subVars1<-data.frame(subVars[b])
    colnames(subVars1)<-subVars1[1,]
    subVars1<-subVars1[-1,]
    subData<-data.frame(matrix(nrow=(ncol(subVars1)-1), ncol=4))
    subData[,1]<- colnames(subVars1)[1:(ncol(subVars1)-1)]
    subData[,2]<-colMeans(subVars1[,1:(ncol(subVars1)-1)], na.rm=T)
    colnames(subData)<-c("Stream", "Mean", "lat", "lon")
      for(x in 1:nrow(subData)){
       subData[x,3:4]<-spaceData[which(subData[x,1]==spaceData[,1]),2:3]
      }#end x loop
    #matrix of inverse distance weights.. flat plane, not spherical because same region
    dists<-as.matrix(dist(cbind(subData$lon, subData$lat)))
    dists.inv <- 1/dists 
    diag(dists.inv) <- 0
    moran<-Moran.I(subData$Mean, dists.inv, na.rm=T)
    
    if(z==1 & b==1){ #store the results in a table
      storeMoran<-data.frame(matrix(nrow=length(spaceVars), ncol=(length(subVars)*2+1)))
      colnames(storeMoran)<-c("allSitesI", "allSitesP", "wetDominatedI", "wetDominatedP", "mixedI","mixedP", "agDominatedI", "adDominatedP", "Var")
      storeMoran[z,b]<-round(moran$observed, digits=3)
      storeMoran[z,b+1]<-round(moran$p.value, digits=3)
    } else{
      storeMoran[z,(b*2-1)]<-round(moran$observed, digits=3)
      storeMoran[z,(b*2)]<-round(moran$p.value, digits=3)
    }

  }#end b loop
  storeMoran[z,ncol(storeMoran)]<-names(spaceVars[z])
} #end z loop

#write.csv(storeMoran, "Out/moranIResults.csv", row.names=F)

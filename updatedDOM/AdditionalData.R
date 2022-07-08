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

data$landUse<-0
for(i in 1:nrow(data)){
  if(length(landUse$landUse[which(landUse$SiteNum==data$Sample.Name[i])]) > 0){
  data$landUse[i]<-landUse$landUse[which(landUse$SiteNum==data$Sample.Name[i])]
  }
}

AllSites<-data[which(data$landUse != 0), ]
length(which(AllSites$Cth == 0))

ggplot(data=AllSites, aes(x=landUse, y=E280))+ geom_boxplot()

###Supporting functions for Synchrony analysis#####
#By CJT, December 23, 2021

nameToNumber<-function(siteList, lookupTable){ 
  for(i in 1:length(siteList)){
    siteList[i]<-lookupTable[which(lookupTable$Stream==siteList[i]),2]
  }
  return(siteList)
}


# A function that subsets data frame into land use groups using matching columns 
# Usage: pullsites(df, sites), df= data frame to take columns from, sites=a list of sites to match with columns 
pullSites<-function(df, sites){
  for(i in 1:length(sites)){
    if(i==1){
      df1<-data.frame(df[,which(colnames(df)==sites[i])])
      colnames(df1)<-colnames(df)[which(colnames(df)==sites[i])]
    } else{
      df2<-data.frame(df[,which(colnames(df)==sites[i])])
      colnames(df2)<-colnames(df)[which(colnames(df)==sites[i])]
      df1<-cbind(df1, df2)
    }
  } 
  #colnames(df1)<-sites
  return(df1)}



#function to z-score data
ScoreVals<-function(Vals){
  my_sd<- sd(Vals, na.rm = T)*sqrt((length(Vals)-1)/(length(Vals)))
  my_mean <- mean(Vals, na.rm=T)
  my_scored<-matrix(ncol=ncol(Vals), nrow=nrow(Vals))
  class(my_scored)<-"numeric"
  for(x in 1:ncol(Vals)){
    for(y in 1:nrow(Vals)){
      if(!is.na(Vals[y,x]))
        my_scored[y,x]<-round(((Vals[y,x]-my_mean)/my_sd), digits=2)
    } #end y loop
    my_scored[y,x]<-"NA"
    class(my_scored)<-"numeric"
  } #end x loop
  return(my_scored)}

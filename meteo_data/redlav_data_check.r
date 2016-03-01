#######################################################################################################################

library(jsonlite)
library(rCharts)
library(rjson)
library(lubridate)

#######################################################################################################################
sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}
ppFactor = 1.576;
rpmFactor = 0.00006109;
convFactor = 0.0275;
#######################################################################################################################

setwd("/home/alf/Scrivania/documents/lav_redlav_data/redlav")
list_files=Sys.glob("*.txt")

a=sapply(list_files,function(x) {b=read.csv(x,header=FALSE);b[grep(":",b[,1]),]=NA;b[,7]=gsub("\003","", b[,7]);b$V19=NULL;return(b)} )
a=lapply(a,function(x) x[,1:18])
names(a)=NULL
data_redlav=do.call('rbind',a)



networkInterface=read.csv("/home/alf/Scrivania/documents/lav_redlav_data/networkInterface.csv",stringsAsFactors = FALSE)
#networkInterface=sort.data.frame(networkInterface,by="device_idDevice",decreasing = FALSE)
#write.csv(networkInterface,"/home/alf/Scrivania/lav_redlav_data/networkInterface.csv",row.names=FALSE)
macAddress_str=as.character(networkInterface$macAddress)
table_data_station=read.csv("/home/alf/Scrivania/documents/lav_redlav_data/table_data_station.csv",stringsAsFactors = FALSE)




# $temp = $sense[1];
# $hum = $sense[2];
# $volt = $sense[3] * $convFactor;
# $voltLiPo = $sense[4] * $convFactor;
# $solar1 = $sense[5];
# $solar2 = $sense[6];
# $waterTemp = $sense[7];
# $RPMTops = $sense[8] * $rpmFactor;			
# $RPMLast = $sense[9] * $rpmFactor;
# $pp = $sense[10] * $ppFactor;
# $ppLast = $sense[11] * $ppFactor;	

data_redlav_station=list()

for ( i in 1:10) {
data_redlav_station[[i]]=data_redlav[grep(macAddress_str[i],data_redlav$V7),]
}

data_redlav_final=lapply(data_redlav_station,FUN=arrange_data)


for ( i in 1:10) {
  data_redlav_final[[i]]$id=paste0("0",macAddress_str[i])
  write.csv(data_redlav_final[[i]],file=paste0(table_data_station$regione[i],"_",gsub(" ","_",table_data_station$X[i]),"_",table_data_station$id[i],".csv"),row.names=FALSE)
}


arrange_data=function(data_station,idstation,ppFactor = 1.576,rpmFactor = 0.00006109,convFactor = 0.0275,na.filling="spline") {
                      require(zoo)
                      require(lubridate)
                      datatime=ISOdate(as.numeric(data_station$V3)+2000, 
                                       as.numeric(data_station$V2), 
                                       as.numeric(data_station$V1), 
                                       hour = as.numeric(data_station$V4), 
                                       min = as.numeric(data_station$V5), 
                                       sec = 0,
                                       tz="CEST") 
                      data_station_new=data.frame(datatime=datatime,
                                              mese=month(datatime),
                                              data=day(datatime),
                                              ora=hour(datatime),
                                              tair=as.numeric(data_station$V8),
                                              rhum=as.numeric(data_station$V9),
                                              watertemp=as.numeric(data_station$V14), 
                                              volt = as.numeric(data_station$V10) * convFactor,
                                              voltLiPo = as.numeric(data_station$V11) * convFactor,
                                              solar1 =  as.numeric(data_station$V12),
                                              solar2 = as.numeric(data_station$V13),
                                              RPMTops = as.numeric(data_station$V15) * rpmFactor,			
                                              RPMLast = as.numeric(data_station$V16) * rpmFactor,
                                              pp = as.numeric(data_station$V17) * ppFactor,
                                              ppLast = as.numeric(data_station$V18)* ppFactor)	
                        
                       data_station_new=sort.data.frame(data_station_new,by=c("datatime"),decreasing = F)
                       index_NA=which(data_station_new$tair==0 & data_station_new$rhum==0)
                       data_station_new$interpolate=rep(0,nrow(data_station_new))
                       data_station_new$tair[index_NA]=NA
                       data_station_new$rhum[index_NA]=NA
                       data_station_new$interpolate[index_NA]=1
                       data_station_new$tair=round(na.approx(data_station_new$tair),2)
                       data_station_new$rhum=round(na.approx(data_station_new$tair),2)
                       return(data_station_new)

}

partworth_utility_basic<-function(coefficients,data,nalts,optout,attribute_type){
  library(RColorBrewer)
  #error control
  if (class(nalts)!="numeric"){
    stop("INSERT a numeric value of nalts")
  }
  for (i in 1:(length(data)-2)){
    if(attribute_type[i]!="C"&attribute_type[i]!="NC"){
      stop("INSERT AS ATTRIBUTE TYPE C OR NC")
    }
  }
  if (nalts==1&optout==FALSE){
    stop("insert more than 1 alternative or set the parameter optout as TRUE")
  }
  ###################################
  if(optout==TRUE){
    nalts=nalts+1
  }
  coefficients=coefficients[-c(1:(nalts-1))]
  vector<-names(coefficients)
  partworth<-coefficients
  partworth<-cbind(partworth,vector)
  partworth_final<-rep(0,length(attribute_type))
  j<-0
  for (i in 1:length(attribute_type)){
    if (attribute_type[i]=="NC"){
      partworth_final[i]=max(as.numeric(partworth[(i+j):(i+j+length(levels(as.factor(as.matrix(data[,i]))))-2),1]),0)-min(as.numeric(partworth[(i+j):(i+j+length(levels(as.factor(as.matrix(data[,i]))))-2),1]),0)
      j<-j+length(levels(as.factor(as.matrix(data[,i]))))-2

    }else{
      partworth_final[i]<-(max(as.numeric(levels(as.factor(as.matrix(data[,i])))))-min(as.numeric(levels(as.factor(as.matrix(data[,i]))))))*abs(as.numeric(partworth[i+j,1]))
    }
  }
  sum<-sum(partworth_final)

  for (i in 1: length(attribute_type)){
    partworth_final[i]<-(partworth_final[i])/sum
  }
  partworth_final<-round(partworth_final,digits=3)*100
  z<-barplot(partworth_final,names.arg=names(data[,1:length(attribute_type)]),col=brewer.pal(n =length(attribute_type), name = "RdBu"),horiz=FALSE,main="Partworth utilities",cex.names=1.2)
  p<-paste(partworth_final,"%")
  q<-text(z,partworth_final-1.2,labels=p,cex=1.5)


  return (list(partworth_utility=partworth_final))



}

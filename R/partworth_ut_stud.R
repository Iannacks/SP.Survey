partworth_utility<-function(coefficients,nalts,optout,attribute_type,design){
  library(RColorBrewer)
  #variable control
  for (i in 1:length(design[4,4:length(design[1,])])){
    if(attribute_type[i]!="C"&attribute_type[i]!="NC"){
      stop("INSERT AS ATTRIBUTE TYPE C OR NC")
    }
  }
  if (class(nalts)!="numeric"){
    stop("INSERT a numeric value of nalts")
  }
  if (nalts==1&optout==FALSE){
    stop("insert more than 1 alternative or set the parameter optout as TRUE")
  }

  if(length(design[4,4:length(design[1,])])<=1){
    stop("insert a candidate design matrix with length >1")
  }

  ##########################################################
  #consider optout option
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
   partworth_final[i]=max(as.numeric(partworth[(i+j):(i+j+length(levels(factor(design[,3+i])))-2),1]),0)-min(as.numeric(partworth[(i+j):(i+j+length(levels(factor(design[,3+i])))-2),1]),0)
   j<-j+length(levels(factor(design[,3+i])))-2

   }else{
   partworth_final[i]<-(max(as.numeric(levels(factor(design[,3+i]))))-min(as.numeric(levels(factor(design[,3+i])))))*abs(as.numeric(partworth[i+j,1]))
   }
   }
    sum<-sum(partworth_final)

    for (i in 1: length(attribute_type)){
    partworth_final[i]<-(partworth_final[i])/sum
    }
    partworth_final<-round(partworth_final,digits=3)*100
    z<-barplot(partworth_final,names.arg=names(design)[4:length(design[1,])],col=brewer.pal(n =length(attribute_type), name = "RdBu"),horiz=FALSE,main="Partworth utilities",cex.names=1.2)
    p<-paste(partworth_final,"%")
    q<-text(z,partworth_final-1.2,labels=p,cex=1.5)


    return (list(partworth_utility=partworth_final))



}

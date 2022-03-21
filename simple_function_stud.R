SP_mlogit_basic<-function(dataset,attribute_type){
library(mlogit)
#error control
   for (i in 1:(length(dataset)-2)){
      if(attribute_type[i]!="C"&attribute_type[i]!="NC"){
         stop("INSERT AS ATTRIBUTE TYPE C OR NC")
      }
   }

   if(names(dataset)[length(dataset)]!="resp"){
   stop("last dataset column must contain responses and must be called resp ")
   }
   if(names(dataset)[length(dataset)-1]!="alt"){
      stop("penultimate dataset column must contain alternatives and must be called alt ")
   }
   ###################################################################
 names<-names(dataset)[1:length(attribute_type)]
 for (i in 1:length(attribute_type)){
   if(attribute_type[i]=="NC"){
     names[i]=paste("as.factor(",names[i],")")
   }else{
     names[i]=names[i]
   }

 }
if(length(attribute_type)==1){
  q<-as.formula(paste("resp~",names[1]))
 }else if(length(attribute_type)==2){
 q<-as.formula(paste("resp~",names[1],"+",names[2]))
 }else if(length(attribute_type)==3){
   q<-as.formula(paste("resp~",names[1],"+",names[2],"+",names[3]))
 }else if(length(attribute_type)==4){
   q<-as.formula(paste("resp~",names[1],"+",names[2],"+",names[3],"+",names[4]))
 }else if(length(attribute_type)==5){
   q<-as.formula(paste("resp~",names[1],"+",names[2],"+",names[3],"+",names[4],"+",names[5]))
 }else if(length(attribute_type)==6){
   q<-as.formula(paste("resp~",names[1],"+",names[2],"+",names[3],"+",names[4],"+",names[5],"+",names[6]))
 } else if(length(attribute_type)==7){
   q<-as.formula(paste("resp~",names[1],"+",names[2],"+",names[3],"+",names[4],"+",names[5],"+",names[6],"+",names[7]))
 } else if (length(attribute_type)==8){
   q<-as.formula(paste("resp~",names[1],"+",names[2],"+",names[3],"+",names[4],"+",names[5],"+",names[6],"+",names[7],"+",names[8]))
 }else{
   q<-as.formula(paste("resp~",names[1],"+",names[2],"+",names[3],"+",names[4],"+",names[5],"+",names[6],"+",names[7],"+",names[8],"+",names[9]))
 }
  dat<-mlogit.data(data=dataset, shape="long",choice="resp",alt.var="alt")
  mod<-mlogit(q,data=dat)
  res<-summary(mod)

}



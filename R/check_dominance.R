check_dominance<-function(sign,design,attribute.names){
#prova
#sign=c("-","+","+","-","+")
#sign<-NULL
#attribute.names<-list(c("3","5","6","10"),c("2/3 days","Same day"),c("Pick-up","Home"),
#c("15","100","150","200","300"),c("Low","Medium","High"))
#design<-design
  #attribute.names=list(delivery_cost = c("3","5","6","10"),
                       #delivery_time = c("2/3 days","Same day"),delivery_location=c("Pick-up","Home"))
  #sign=c("-","+","+")
########################################
dom<-0
p<-0
list_dom<-vector()
des<-design
if(is.null(sign)==TRUE){
sign=rep("",length(attribute.names))
}
#for(i in 1:length(attribute.names)){
#design[,3+i]<-factor(design[,3+i])
#levels(design[,3+i])<-attribute.names[[i]] #Set attribute levels
#}
for (h in 1:max(as.numeric(design$card))){ #start check dominance cycle
temp<-list()
for(i in 1:length(attribute.names)){
if(sign[i]=="+"){
temp[[i]]<-which(as.numeric(design[design$card==h,3+i])==max(as.numeric(design[design$card==h,3+i])))
}else if(sign[i]=="-"){
temp[[i]]<-which(as.numeric(design[design$card==h,3+i])==min(as.numeric(design[design$card==h,3+i])))
}else{
print(paste("i è",i))
t<-which(as.numeric(design[design$card==h,3+i])==min(as.numeric(design[design$card==h,3+i])))
if(length(t)==length(design[design$card==h,1])){
  temp[[i]]=t
print(temp[[i]])
}else{
temp[[i]]<-NULL
}
}
}
print(design[design$card==h,])
print(unlist(temp))
#if(length(levels(factor(unlist(temp))))==1){
#dom<-dom+1
#}
alt<-length(design[design$card==h,1])
if(alt==2){
alt1=which(unlist(temp)==1)
alt2=which(unlist(temp)==2)
if(sign[1]==rep("")){
if(length(alt1)>=(length(attribute.names)-1)){
dom=dom+1
p<-p+1
  list_dom[p]<-h
  }
}else{
if(length(alt1)==length(attribute.names)|length(alt2)==length(attribute.names)){
dom=dom+1
p<-p+1
list_dom[p]<-h
}
}
}else if(alt==3){
alt1=which(unlist(temp)==1)
alt2=which(unlist(temp)==2)
alt3=which(unlist(temp)==3)
if(sign[1]==""){
  if(length(alt1)>=(length(attribute.names)-1)){
    dom=dom+1
    p<-p+1
    list_dom[p]<-h
  }
}else{
if(length(alt1)==length(attribute.names)|length(alt2)==length(attribute.names)|length(alt3)==length(attribute.names)){
  dom=dom+1
  p<-p+1
  list_dom[p]<-h
}
}
}else if(alt==4){
alt1=which(unlist(temp)==1)
alt2=which(unlist(temp)==2)
alt3=which(unlist(temp)==3)
alt4=which(unlist(temp)==4)
if(sign[1]==""){
  if(length(alt1)>=(length(attribute.names)-1)){
    dom=dom+1
    p<-p+1
    list_dom[p]<-h
  }
}else{
if(length(alt1)==length(attribute.names)|length(alt2)==length(attribute.names)|length(alt3)==length(attribute.names)|length(alt4)==length(attribute.names)){
  dom=dom+1
  p<-p+1
  list_dom[p]<-h
}
}
}else if(alt==5){
alt1=which(unlist(temp)==1)
alt2=which(unlist(temp)==2)
alt3=which(unlist(temp)==3)
alt4=which(unlist(temp)==4)
alt5=which(unlist(temp)==5)
if(sign[1]==""){
  if(length(alt1)>=(length(attribute.names)-1)){
    dom=dom+1
    p<-p+1
    list_dom[p]<-h
  }
}else{
if(length(alt1)==length(attribute.names)|length(alt2)==length(attribute.names)|length(alt3)==length(attribute.names)|length(alt4)==length(attribute.names)|length(alt5)==length(attribute.names)){
  dom=dom+1
  p<-p+1
  list_dom[p]<-h
}
}
}else if(alt==6){
alt1=which(unlist(temp)==1)
alt2=which(unlist(temp)==2)
alt3=which(unlist(temp)==3)
alt4=which(unlist(temp)==4)
alt5=which(unlist(temp)==5)
alt6=which(unlist(temp)==6)
if(sign[1]==""){
  if(length(alt1)>=(length(attribute.names)-1)){
    dom=dom+1
    p<-p+1
    list_dom[p]<-h
  }
}else{
if(length(alt1)==length(attribute.names)|length(alt2)==length(attribute.names)|length(alt3)==length(attribute.names)|length(alt4)==length(attribute.names)|length(alt5)==length(attribute.names)|length(alt6)==length(attribute.names)){
  dom=dom+1
  p<-p+1
  list_dom[p]<-h
}
}
}else{
stop("error the number inserted is wrong")
}
}#end dominance cycle
return(p=list(dominance=dom/max(as.numeric(design$card))*100,task_dominated=list_dom,starting_des=design))
}


des_function<-function(attribute.names,alt=2,set=NULL,block=NULL,typology,seed=NULL){
#import libraries
  library(choiceDes)
  library(support.CEs)
  library(DoE.base)

#prove
#candidate=expand.grid(list(x=c(1,2,3),y=c("a","b","c","d"),z=c("40","80","120")))
#alt<-2
#typology=0
#set<-10
#block<-5
###########################################################################
#error control
if(typology==0&is.null(set)==TRUE){
stop("insert the number of task")
}
if(typology==0&is.null(block)==TRUE){
    stop("insert the number of block")
}
if(is.null(alt)==TRUE|is.numeric(alt)==FALSE){
stop("insert the number of alternatives")
}

if(is.numeric(set)==FALSE&typology==0){
stop("insert a numeric value for the set")
}

  if(is.numeric(block)==FALSE&typology==0){
    stop("insert a numeric value for the blocks")
  }

  if(is.numeric(alt)==FALSE&typology==0){
    stop("insert a numeric value for the alternatives")
  }



candidate<-expand.grid(attribute.names)
r_lev<-vector()
for(i in 1:length(candidate[1,])){
r_lev[i]<-length(levels(factor(candidate[,i])))
}
if(typology==0){
set.seed(seed)
design<-dcm.design.cand(cand=candidate,sets=set,nb=block,alts=alt,Rd=20)
design<-design$levels
design<-as.data.frame(design)
colnames(design)[1]<-c("card")
colnames(design)[2]<-"block"
colnames(design)[3]<-"task"
for(i in 1:length(candidate[1,])){
  colnames(design)[3+i]<-colnames(candidate)[i]
}
}else{

temp<-show.oas(nlevels=r_lev)[,2]
if(temp[1]>0){
temp<-sort(unique(show.oas(nlevels=r_lev)[,2]))
temp<-temp[-which(temp>prod(r_lev))]
tot_choice<-menu(choices=as.character(temp),title="choose question number",graphics=TRUE)
total_questions<-temp[tot_choice]

divisors <- function(x){
y <- seq_len(x)
y[ x%%y == 0 ]
}
divisors<-divisors(total_questions)
set_choice<-as.numeric(menu(choices=as.character(divisors),"choose task number",graphics=TRUE))
set<-divisors[set_choice]
block<-total_questions/divisors[set_choice]
design<-rotation.design(candidate.array=oa.design(nlevel=r_lev,nruns=total_questions),nalternatives=alt,
                        nblocks=block,attribute.names=attribute.names)
t<-list()
for (i in 1:length(design$alternatives$alt.1[,1])){
  if(alt==2){
    t[[i]]<-rbind(design$alternatives$alt.1[i,],design$alternatives$alt.2[i,])
  }else if(alt==3){
    t[[i]]<-rbind(design$alternatives$alt.1[i,],design$alternatives$alt.2[i,],design$alternatives$alt.3[i,])
  }else if(alt==4){
    t[[i]]<-rbind(design$alternatives$alt.1[i,],design$alternatives$alt.2[i,],design$alternatives$alt.3[i,],design$alternatives$alt.4[i,])
  }else{
    t[[i]]<-rbind(design$alternatives$alt.1[i,],design$alternatives$alt.2[i,],design$alternatives$alt.3[i,],design$alternatives$alt.4[i,],design$alternatives$alt.5[i,])
  }
}
#change the list into a matrix
z<-data.frame()
for(i in 1:length(design$alternatives$alt.1[,1])){
  z<-rbind(z,t[[i]])
}
z<-as.matrix(z)
colnames(z)[1:2]<-c("block","task")
z<-z[,-3]
design<-matrix(nrow=block*alt*set,ncol=3+length(candidate[1,]))
design[,1]<-as.vector(t(sapply((1:alt),function(i)(1:(set*block)))))
design[,2]<-z[,1]
design[,3]<-z[,2]

for(i in 1:length(candidate[1,])){
design[,3+i]<-z[,2+i]
}
design<-as.data.frame(design)
colnames(design)[1]<-c("card")
colnames(design)[2]<-"block"
colnames(design)[3]<-"task"
for(i in 1:length(candidate[1,])){
  colnames(design)[3+i]<-colnames(candidate)[i]
}



}else{
stop("There are no orthogonal design presents, please change parameters")
}



}
return(design)
}

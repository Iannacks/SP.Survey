SP_mlogit<-function(attribute_type,seed=NULL,cand,nalts,nblocks,sets,
dataset_forms,nrespondents,optout,base=NULL,conf=FALSE,conf_level=NULL,exp_design=NULL,other_attributes=NULL){
  library(choiceDes)
  library(varhandle)
  library(readxl)
  library(mlogit)
  library(logitr)


  #error control
  for (i in 1:length(cand[1,])){
    if(attribute_type[i]!="C"&attribute_type[i]!="NC"){
      stop("INSERT AS ATTRIBUTE TYPE C OR NC")
    }
  }
  if (class(nalts)!="numeric"|class(nblocks)!="numeric"|class(sets)!="numeric"|class(nalts)!="numeric"|class(nblocks)!="numeric"){
    stop("INSERT a numeric value of nalts/nblocks/sets")
  }
  if(length(nrespondents)!=nblocks){
    stop("nrespondents parameter must have the same length of the parameter nblocks")
  }
  if(class(nrespondents)!="numeric"){
    stop("nrespondents parameter must be numeric")
  }

  if(class(cand)!="data.frame"){
    stop("parameter cand must have class data.frame ")
  }
  if(length(cand[1,])<=1){
    stop("insert a candidate design matrix with length >1" )
  }
  if(length(cand[1,])!=length(attribute_type)){
    stop("parameter cand and attribute_type must have the same length")
  }

  if(is.null(base)==TRUE&optout==TRUE){
    stop("set the parameter optout as FALSE or if it must be TRUE insert the parameter BASE")
  }
  if(is.null(base)==FALSE&optout==FALSE){
    stop("delete the parameter base or set the parameter optout as TRUE")
  }
  if (nalts==1&optout==FALSE){
    stop("insert more than 1 alternative or set the parameter optout as TRUE")
  }

  #Load the experimental design if already present
  if(is.null(exp_design)==FALSE){
    design<-read_excel(exp_design)
  }

  #set the seed
  if(is.null(exp_design)==TRUE){
  set.seed(seed)
  }

  #create experimental design matrix
  if(is.null(exp_design)==TRUE){
  design <- dcm.design.cand(cand=cand, sets=sets, nb=nblocks, alts=nalts)
  design<-design$levels
  for (i in 1:length(cand[1,])){
    colnames(design)[3+i]=names(cand)[i]
  }
  }else{
    for (i in 1:length(cand[1,])){
      colnames(design)[3+i]=names(cand)[i]
    }
  }

  #dataset creation->column creation
  a<-list()
  for(i in 1:length(cand[1,])){
    if(attribute_type[i]=="NC"){
      a[[i]]<-to.dummy(design[,3+i],prefix=names(design)[3+i])
       a[[i]]<-as.data.frame(a[[i]])[-1]


    }else{
      a[[i]]<-as.data.frame(design[,3+i])

    }
  }

  a<-as.data.frame(a)
  print(a)
  #add name of the continuous variables
  temp<-which(attribute_type=="C")
  names(a)[temp]<-names(cand)[temp]
  #definition of the total number column
  tot_col<-0
  for (i in 1:length(cand[1,])){
    if (attribute_type[i]=="NC"){
      tot_col<-(length(levels(factor(cand[,i])))-1)+tot_col

    }else{
      tot_col<-tot_col+1

    }
  }
  #Add blocks, task
  #set the seed
  if(is.null(exp_design)==TRUE){

  card<-as.numeric(design[,1])
  block<-as.numeric(design[,2])
  task<-as.numeric(design[,3])
  }else{
    card<-design[,1]
    block<-design[,2]
    task<-design[,3]
  }
  dat<-a

  #insert optout option
  if(optout==TRUE){
    base<-t(base)
    base<-as.matrix(base)
    temp<-list()
    k<-0
    dat_a<-data.frame()
    for (i in 1:(sets*nblocks)){
      temp[[i]]<-as.matrix(dat[(i+k):(i+nalts-1+k),])
      temp[[i]]<-rbind(temp[[i]],base)
      k<-k+nalts-1
      temp[[i]]<-as.data.frame(temp[[i]])
      dat_a<-rbind(dat_a,temp[[i]])
    }
    q<-as.data.frame(dat_a)
  }
  #replicate blocks * number of respondents
  if(optout==TRUE){
    nalts<-nalts+1
    dat<-q

  }

  #create blocks
  temp1<-sapply(1:(sets*(nalts)),function(i)(1:nblocks))
  temp1<-t(temp1)
  temp1<-as.vector(temp1)
  dat$block<-temp1
  temp<-list()
  dat_f<-data.frame()
  for (i in 1:nblocks){
    temp[[i]]<-do.call(rbind, replicate(nrespondents[i],subset(dat,dat$block==i), simplify=FALSE))
    temp[[i]]<-as.data.frame(temp[[i]])
    dat_f<-rbind(dat_f,temp[[i]])
  }
  #add alternatives
  dat_f$ALT<-rep(c(1:nalts),sets*sum(nrespondents))
  #add sets
  temp<-sapply((1:(nalts)),function(i)(1:sets))
  temp<-t(temp)
  temp<-as.vector(temp)
  dat_f$set<-temp
  print(dat_f$set)
  #add responses
  resp<-list()
  resp_f<-c()
  for(i in 1:nblocks){
    resp[[i]]<-read_excel(dataset_forms[i])#step1
    resp[[i]]<-as.vector(t(resp[[i]][,2:(sets+1)]))
    resp[[i]]<-to.dummy(resp[[i]],"resp")
    resp[[i]]<-as.vector(t(resp[[i]]))
    resp_f<-append(resp_f,resp[[i]])
  }

  dat_f$resp<-resp_f
  #add other variables

  if(length(other_attributes)!=0){
    for(z in 1: length(other_attributes)){
    other<-list()
    other_f<-data.frame()
    temp<-list()

    for(i in 1:nblocks){
      other[[i]]<-read_excel(dataset_forms[i])#step1
      other[[i]]<-as.vector(t(other[[i]][,(sets+1+z)]))
      for(j in 1:length(other[[i]])){
        temp[[j]]<-do.call(rbind, replicate(sets*nalts,other[[i]][j], simplify=FALSE))
        other_f<-rbind(other_f,temp[[j]])
      }
    }
names(other_f)<-other_attributes[z]

 dat_f<-cbind(dat_f,other_f)
    }
}
  #set the final multinomial dataset
  for (i in 1:tot_col){
    dat_f[,i]=as.numeric(as.character(dat_f[,i]))
  }
  names<-names(dat_f[,1:tot_col])
  names<-append("resp",names)
  q<-as.formula(dat_f[,c(names)])

  dat_mlogit<-mlogit.data(dat_f, shape="long",choice="resp",alt.var="ALT")
  mod<-mlogit(q,data=dat_mlogit)
  res<-summary(mod)
  if(conf==TRUE){
    for(i in 1:length(res$coefficients)){
      if(res$CoefTable[i,length(res$CoefTable[1,])]>conf_level){
        res$coefficients[i]=0
      }else{
        res$coefficients[i]=res$coefficients[i]
      }
    }
  }

  return(list(summary=res,dataset=dat_f))
}


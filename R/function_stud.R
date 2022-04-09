SP_mlogit<-function(attribute_type,seed,cand,nalts,nblocks,sets,
                      dataset_forms,nrespondents,optout,base=NULL,conf=FALSE,conf_level=NULL){
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
  if (class(nalts)!="numeric"|class(nblocks)!="numeric"|class(sets)!="numeric"|class(nalts)!="numeric"|class(nblocks)!="numeric"|class(seed)!="numeric"){
    stop("INSERT a numeric value of nalts/nblocks/sets/seed")
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
  if(length(cand[1,])<=1|length(cand[1,])>5){
    stop("insert a candidate design matrix with length >1 and <=5")
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


  #set the seed
  set.seed(seed)
  #names creation
  tot_col<-0
  for (i in 1:length(cand[1,])){
    print(i)
    if (attribute_type[i]=="NC"){
      tot_col<-(length(levels(factor(cand[,i])))-1)+tot_col

    }else{
      tot_col<-tot_col+1

    }
  }
  names_dataset<-rep("name",tot_col)
  temp<-list()
  z<-0
  for (i in 1:length(attribute_type)){
    if(attribute_type[i]=="NC"){
      temp[[i]]<-paste(names(cand[1,])[i],levels(factor(cand[,i]))[-1],sep="_")


    }else{
      temp[[i]]<-names(cand[1,])[i]
    }
  }
  names_dataset<-unlist(temp)


  #create experimental design matrix
  design <- dcm.design.cand(cand=cand, sets=sets, nb=nblocks, alts=nalts)
  design<-design$levels
  for (i in 1:length(cand[1,])){
    colnames(design)[3+i]=names(cand)[i]
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
  #combine column name with the dataset
  colnames(a)<-names_dataset
  #Add blocks, task
  card<-as.numeric(design[,1])
  block<-as.numeric(design[,2])
  task<-as.numeric(design[,3])
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


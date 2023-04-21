SP_mlogit<-function(attribute_type,dataset_forms,nrespondents,base=NULL,conf=FALSE,
          conf_level=NULL,design=NULL,other_attributes=NULL,optout=FALSE){
  library(choiceDes)
  library(varhandle)
  library(readxl)
  library(mlogit)
  library(logitr)
  #prove
  ########################################################################################################################
  #sets=10
  #nalts=2
  #nblocks=2
  #attribute_type = c("C","NC","NC","C","NC")
  #dataset_forms=c("C:/Users/utente/Downloads/Block 1 DELIVERY (Risposte) (6).xlsx",
  #                "C:/Users/utente/Downloads/_Block 2 DELIVERY (Risposte) (5).xlsx")
  #nrespondents=c(7,1)

  #optout=FALSE
  #other_attributes = "university"
  #design=design
  #############################################################################################################
  #error control
  if(class(design)!="data.frame"){
    stop("Insert a design of class data.frame or matrix")
  }
  #define the length of the design
  len<-length(design[1,])-3
  for (i in 1:len){
    if(attribute_type[i]!="C"&attribute_type[i]!="NC"){
      stop("INSERT AS ATTRIBUTE TYPE C OR NC")
    }
  }
  if(class(nrespondents)!="numeric"){
    stop("nrespondents parameter must be numeric")
  }

  if(len!=length(attribute_type)){
    stop(" and attribute_type must have the same length of the total attributes number")
  }


  #Load the experimental design if already present
  #if(is.null(exp_design)==FALSE){
  # design<-read_excel(exp_design)
  #}

  sets<-max(design$task)
  nblocks<-max(design$block)
  dat_temp1<-subset(design,design$task==1&design$block==1)
  nalts<-length(dat_temp1[,1])


  #dataset creation->column creation
  a<-list()
  for(i in 1:len){
    if(attribute_type[i]=="NC"){
      a[[i]]<-to.dummy(design[,3+i],prefix=colnames(design)[3+i])
      a[[i]]<-as.data.frame(a[[i]])[-1]


    }else{
      a[[i]]<-as.data.frame(design[,3+i])
      colnames(a[[i]])<-colnames(design)[3+i]
    }
  }

  a<-as.data.frame(a)
  dat<-a
  #definition of the total number column
  tot_col<-0
  for (i in 1:len){
    if (attribute_type[i]=="NC"){
      tot_col<-(length(levels(factor(design[,3+i])))-1)+tot_col

    }else{
      tot_col<-tot_col+1

    }
  }
  #insert optout option
  if(optout==TRUE){
    base<-as.matrix(t(base))
    temp<-list()
    k<-0
    dat_a<-data.frame()
    for (i in 1:(sets*nblocks)){
      temp[[i]]<-as.matrix(a[(i+k):(i+nalts-1+k),])
      temp[[i]]<-rbind(temp[[i]],base)
      k<-k+nalts-1
      temp[[i]]<-as.data.frame(temp[[i]])
      dat_a<-rbind(dat_a,temp[[i]])
      print(dat)
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
    for(z in 1:length(other_attributes)){
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


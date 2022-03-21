SP_mlogit<-function(attribute_type,seed,cand,nalts,nblocks,sets,dataset_forms,nrespondents,optout=FALSE,base=NULL){
  library(survival)
  library(readxl)
  library(varhandle)
  library(mlogit)
  library(nnet)
  library(choiceDes)
  library(support.CEs)
  library(stringr)
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



  #names creation
  tot_col<-0

  for (i in 1:length(cand[1,])){

    if (attribute_type[i]=="NC"){
      tot_col<-(length(levels(cand[,i]))-1)+tot_col

    }else{
      tot_col<-tot_col+1

    }
  }
  names_dataset<-rep("name",tot_col)

  #base parameter control
  if(length(base)!=tot_col&optout==TRUE){
  stop("modify the length of the parameter base according to the columns number of the final dataset you want to create")
  }
  #########################
  if (length(attribute_type)==2){
    if(attribute_type[1]=="NC"){
      names_at1<-paste(names(cand[1,])[1],levels(cand[,1])[-1],sep="_")
    }else{
      names_at1<-names(cand[1,])[1]
    }
    if(attribute_type[2]=="NC"){
      names_at2<-paste(names(cand[1,])[2],levels(cand[,2])[-1],sep="_")
    }else{
      names_at2<-names(cand[1,])[2]
    }

    names_dataset<-c(names_at1,names_at2)
  }
  if (length(attribute_type)==3){
    if(attribute_type[1]=="NC"){
      names_at1<-paste(names(cand[1,])[1],levels(cand[,1])[-1],sep="_")
    }else{
      names_at1<-names(cand[1,])[1]
    }
    if(attribute_type[2]=="NC"){
      names_at2<-paste(names(cand[1,])[2],levels(cand[,2])[-1],sep="_")
    }else{
      names_at2<-names(cand[1,])[2]
    }
    if(attribute_type[3]=="NC"){
      names_at3<-paste(names(cand[1,])[3],levels(cand[,3])[-1],sep="_")
    }else{
      names_at3<-names(cand[1,])[3]
    }
    names_dataset<-c(names_at1,names_at2,names_at3)
  }
  if (length(attribute_type)==4){
    if(attribute_type[1]=="NC"){
      names_at1<-paste(names(cand[1,])[1],levels(cand[,1])[-1],sep="_")
    }else{
      names_at1<-names(cand[1,])[1]
    }
    if(attribute_type[2]=="NC"){
      names_at2<-paste(names(cand[1,])[2],levels(cand[,2])[-1],sep="_")
    }else{
      names_at2<-names(cand[1,])[2]
    }
    if(attribute_type[3]=="NC"){
      names_at3<-paste(names(cand[1,])[3],levels(cand[,3])[-1],sep="_")
    }else{
      names_at3<-names(cand[1,])[3]
    }
    if(attribute_type[4]=="NC"){
      names_at4<-paste(names(cand[1,])[4],levels(cand[,4])[-1],sep="_")
    }else{
      names_at4<-names(cand[1,])[4]
    }


    names_dataset<-c(names_at1,names_at2,names_at3,names_at4)
  }

  if (length(attribute_type)==5){
    if(attribute_type[1]=="NC"){
      names_at1<-paste(names(cand[1,])[1],levels(cand[,1])[-1],sep="_")
    }else{
      names_at1<-names(cand[1,])[1]
    }
    if(attribute_type[2]=="NC"){
      names_at2<-paste(names(cand[1,])[2],levels(cand[,2])[-1],sep="_")
    }else{
      names_at2<-names(cand[1,])[2]
    }
    if(attribute_type[3]=="NC"){
      names_at3<-paste(names(cand[1,])[3],levels(cand[,3])[-1],sep="_")
    }else{
      names_at3<-names(cand[1,])[3]
    }
    if(attribute_type[4]=="NC"){
      names_at4<-paste(names(cand[1,])[4],levels(cand[,4])[-1],sep="_")
    }else{
      names_at4<-names(cand[1,])[4]
    }
    if(attribute_type[5]=="NC"){
      names_at5<-paste(names(cand[1,])[5],levels(cand[,5])[-1],sep="_")
    }else{
      names_at5<-names(cand[1,])[5]
    }

    names_dataset<-c(names_at1,names_at2,names_at3,names_at4,names_at5)
  }


  #####################################################################################################################################
  set.seed(seed)
  design <- dcm.design.cand(cand=cand, sets=sets, nb=nblocks, alts=nalts)
  design<-design$levels
  if (length(cand)==1){
    colnames(design)[4]=names(cand)[1]

  }else if(length(cand)==2){
    colnames(design)[4]=names(cand)[1]
    colnames(design)[5]=names(cand)[2]

  }else if (length(cand)==3){
    colnames(design)[4]=names(cand)[1]
    colnames(design)[5]=names(cand)[2]
    colnames(design)[6]=names(cand)[3]

  } else if (length(cand)==4){
    colnames(design)[4]=names(cand)[1]
    colnames(design)[5]=names(cand)[2]
    colnames(design)[6]=names(cand)[3]
    colnames(design)[7]=names(cand)[4]

  } else if (length(cand)==5){
    colnames(design)[4]=names(cand)[1]
    colnames(design)[5]=names(cand)[2]
    colnames(design)[6]=names(cand)[3]
    colnames(design)[7]=names(cand)[4]
    colnames(design)[8]=names(cand)[5]

  }else{
    print("error")
  }



  #dataset creation
  if (length(cand[1,])==5){
    if(attribute_type[1]=="NC"){
      a<-to.dummy(design[,4],prefix=names(design)[4])
      a<-as.data.frame(a[,-1])
    }else{
      a<-as.data.frame(design[,4])
    }
    if(attribute_type[2]=="NC"){
      b<-to.dummy(design[,5],prefix=names(design)[5])
      b<-as.data.frame(b[,-1])
    }else{
      b<-as.data.frame(design[,5])
    }
    if(attribute_type[3]=="NC"){
      c<-to.dummy(design[,6],prefix=names(design)[6])
      c<-as.data.frame(c[,-1])
    }else{
      c<-as.data.frame(design[,6])
    }
    if(attribute_type[4]=="NC"){
      d<-to.dummy(design[,7],prefix=names(design)[7])
      d<-as.data.frame(d[,-1])
    }else{
      d<-as.data.frame(design[,7])
    }
    if(attribute_type[5]=="NC"){
      e<-to.dummy(design[,8],prefix=names(design)[8])
      e<-as.data.frame(e[,-1])
    }else{
      e<-as.data.frame(design[,8])
    }

    dat<-cbind(a,b)
    dat<-cbind(dat,c)
    dat<-cbind(dat,d)
    dat<-cbind(dat,e)
    colnames(dat)<-names_dataset



  } else if(length(cand[1,])==4){
    if(attribute_type[1]=="NC"){
      a<-to.dummy(design[,4],prefix=names(design)[4])
      a<-as.data.frame(a[,-1])
    }else{
      a<-as.data.frame(design[,4])
    }
    if(attribute_type[2]=="NC"){
      b<-to.dummy(design[,5],prefix=names(design)[5])
      b<-as.data.frame(b[,-1])
    }else{
      b<-as.data.frame(design[,5])
    }
    if(attribute_type[3]=="NC"){
      c<-to.dummy(design[,6],prefix=names(design)[6])
      c<-as.data.frame(c[,-1])
    }else{
      c<-as.data.frame(design[,6])
    }
    if(attribute_type[4]=="NC"){
      d<-to.dummy(design[,7],prefix=names(design)[7])
      d<-as.data.frame(d[,-1])
    }else{
      d<-as.data.frame(design[,7])
    }

    dat<-cbind(a,b)
    dat<-cbind(dat,c)
    dat<-cbind(dat,d)
    colnames(dat)<-names_dataset
  } else if (length(cand[1,])==3) {
    if(attribute_type[1]=="NC"){
      a<-to.dummy(design[,4],prefix=names(design)[4])
      a<-as.data.frame(a[,-1])
    }else{
      a<-as.data.frame(design[,4])
    }
    if(attribute_type[2]=="NC"){
      b<-to.dummy(design[,5],prefix=names(design)[5])
      b<-as.data.frame(b[,-1])
    }else{
      b<-as.data.frame(design[,5])
    }
    if(attribute_type[3]=="NC"){
      c<-to.dummy(design[,6],prefix=names(design)[6])
      c<-as.data.frame(c[,-1])
    }else{
      c<-as.data.frame(design[,6])
    }

    dat<-cbind(a,b)
    dat<-cbind(dat,c)
    colnames(dat)<-names_dataset


  } else{
    if(attribute_type[1]=="NC"){
      a<-to.dummy(design[,4],prefix=names(design)[4])
      a<-as.data.frame(a[,-1])
    }else{
      a<-as.data.frame(design[,4])
    }
    if(attribute_type[2]=="NC"){
      b<-to.dummy(design[,5],prefix=names(design)[5])
      b<-as.data.frame(b[,-1])
    }else{
      b<-as.data.frame(design[,5])
    }



    dat<-cbind(a,b)

  }
  card<-as.numeric(design[,1])
  block<-as.numeric(design[,2])
  task<-as.numeric(design[,3])
  matrix_temp<-dat
  matrix_temp<-as.matrix(matrix_temp)
  dat<-cbind(dat,card)
  dat<-cbind(dat,block)
  dat<-cbind(dat,task)
  if(optout==TRUE){
    temp<-t(base)
    temp<-as.matrix(temp)
    k<-0
    q<-matrix(ncol=length(matrix_temp[1,]),nrow=0)

    for (i in 1:(sets*nblocks)){
      if (nalts==2){
        a<-matrix(nrow=3,ncol=length(matrix_temp[1,]))
        a<-rbind(matrix_temp[i+k,],matrix_temp[i+k+1,],temp)
        k<-k+1
        q<-rbind(q,a)
      }else if(nalts==3){
        a<-matrix(nrow=4,ncol=length(matrix_temp[1,]))
        a<-rbind(matrix_temp[i+k,],matrix_temp[i+k+1,],matrix_temp[i+k+2,],temp)
        k<-k+2

        q<-rbind(q,a)
      }else{
        a<-matrix(nrow=5,ncol=length(matrix_temp[1,]))
        a<-rbind(matrix_temp[i+k,],matrix_temp[i+k+1,],matrix_temp[i+k+2,],matrix_temp[i+k+3,],temp)
        k<-k+3
        q<-rbind(q,a)

      }
    }

    q<-as.data.frame(q)
  }
#experimental design combination with Google forms data
  if (nblocks==1){
    if(optout==TRUE){
    nalts=nalts+1
    dat<-q
    }
    resp1<-read_excel(dataset_forms[1])#step1
    resp1<-as.vector(t(resp1[,2:(sets+1)]))
    resp1<-to.dummy(resp1,"resp")
    resp1<-as.vector(t(resp1))
    dat<-do.call(rbind, replicate(nrespondents[1],dat[1:(sets*nalts),], simplify=FALSE))
    dat<-cbind(dat,resp)
    dat$ALT<-rep(c(1:nalts),sets*(nrespondents[1]))



  }
  if (nblocks==2){
    if(optout==TRUE){
      nalts=nalts+1
      dat<-q
    }
    resp1<-read_excel(dataset_forms[1])#step1
    resp1<-as.vector(t(resp1[,2:(sets+1)]))
    resp1<-to.dummy(resp1,"resp")
    resp1<-as.vector(t(resp1))
    resp2<-read_excel(dataset_forms[2])#step1
    resp2<-as.vector(t(resp2[,2:(sets+1)]))
    resp2<-to.dummy(resp2,"resp")
    resp2<-as.vector(t(resp2))
    resp<-append(resp1,resp2)
    dataset1<-do.call(rbind, replicate(nrespondents[1],dat[1:(sets*nalts),], simplify=FALSE))
    dataset2<-do.call(rbind, replicate(nrespondents[2],dat[((sets*nalts)+1):(sets*nalts*nblocks),], simplify=FALSE))
    dat<-rbind(dataset1,dataset2)
    dat<-cbind(dat,resp)
    dat$ALT<-rep(c(1:nalts),sets*(nrespondents[1]+nrespondents[2]))
    if(optout==TRUE){
      dat$ALT<-rep(c(1:nalts),sets*(nrespondents[1]+nrespondents[2]))
    }


  }
  if (nblocks==3){
    if(optout==TRUE){
      nalts=nalts+1
      dat<-q
    }
    resp1<-read_excel(dataset_forms[1])#step1
    resp1<-as.vector(t(resp1[,2:(sets+1)]))
    resp1<-to.dummy(resp1,"resp")
    resp1<-as.vector(t(resp1))
    resp2<-read_excel(dataset_forms[2])#step1
    resp2<-as.vector(t(resp2[,2:(sets+1)]))
    resp2<-to.dummy(resp2,"resp")
    resp2<-as.vector(t(resp2))
    resp3<-read_excel(dataset_forms[3])
    resp3<-as.vector(t(resp3[,2:(sets+1)]))
    resp3<-to.dummy(resp3,"resp")
    resp3<-as.vector(t(resp3))
    resp<-append(resp1,resp2)
    resp<-append(resp,resp3)
    dataset1<-do.call(rbind, replicate(nrespondents[1],dat[1:(sets*nalts),], simplify=FALSE))
    dataset2<-do.call(rbind, replicate(nrespondents[2],dat[((sets*nalts)+1):(sets*nalts*(nblocks-1)),], simplify=FALSE))
    dataset3<-do.call(rbind, replicate(nrespondents[3],dat[((sets*nalts*(nblocks-1))+1):(sets*nalts*nblocks),], simplify=FALSE))
    data<-rbind(dataset1,dataset2,dataset3)
    dat<-cbind(dat,resp)
    dat$ALT<-rep(c(1:nalts),sets*(nrespondents[1]+nrespondents[2]+nrespondents[3]))


  }
  if (nblocks==4){
    if(optout==TRUE){
      nalts=nalts+1
      dat<-q
    }
    resp1<-read_excel(dataset_forms[1])#step1
    resp1<-as.vector(t(resp1[,2:(sets+1)]))
    resp1<-to.dummy(resp1,"resp")
    resp1<-as.vector(t(resp1))
    resp2<-read_excel(dataset_forms[2])#step1
    resp2<-as.vector(t(resp2[,2:(sets+1)]))
    resp2<-to.dummy(resp2,"resp")
    resp2<-as.vector(t(resp2))
    resp3<-read_excel(dataset_forms[3])
    resp3<-as.vector(t(resp3[,2:(sets+1)]))
    resp3<-to.dummy(resp3,"resp")
    resp3<-as.vector(t(resp3))
    resp4<-read_excel(dataset_forms[4])
    resp4<-as.vector(t(resp4[,2:(sets+1)]))
    resp4<-to.dummy(resp4,"resp")
    resp4<-as.vector(t(resp4))
    resp<-append(resp1,resp2)
    resp<-append(resp,resp3)
    resp<-append(resp,resp4)
    dataset1<-do.call(rbind, replicate(nrespondents[1],dat[1:(sets*nalts),], simplify=FALSE))
    dataset2<-do.call(rbind, replicate(nrespondents[2],dat[((sets*nalts)+1):(sets*nalts*(nblocks-2)),], simplify=FALSE))
    dataset3<-do.call(rbind, replicate(nrespondents[3],dat[((sets*nalts*(nblocks-2))+1):(sets*nalts*(nblocks-1)),], simplify=FALSE))
    dataset4<-do.call(rbind, replicate(nrespondents[4],dat[((sets*nalts*(nblocks-1))+1):(sets*nalts*nblocks),], simplify=FALSE))
    dataset<-rbind(dataset1,dataset2,dataset3,dataset4)
    dat<-cbind(dataset,resp)
    dat$ALT<-rep(c(1:nalts),sets*(nrespondents[1]+nrespondents[2]+nrespondents[3]+nrespondents[4]))



  }
  if (nblocks==5){
    if(optout==TRUE){
      nalts=nalts+1
      dat<-q
    }
    resp1<-read_excel(dataset_forms[1])#step1
    resp1<-as.vector(t(resp1[,2:(sets+1)]))
    resp1<-to.dummy(resp1,"resp")
    resp1<-as.vector(t(resp1))
    resp2<-read_excel(dataset_forms[2])#step1
    resp2<-as.vector(t(resp2[,2:(sets+1)]))
    resp2<-to.dummy(resp2,"resp")
    resp2<-as.vector(t(resp2))
    resp3<-read_excel(dataset_forms[3])
    resp3<-as.vector(t(resp3[,2:(sets+1)]))
    resp3<-to.dummy(resp3,"resp")
    resp3<-as.vector(t(resp3))
    resp4<-read_excel(dataset_forms[4])
    resp4<-as.vector(t(resp4[,2:(sets+1)]))
    resp4<-to.dummy(resp4,"resp")
    resp4<-as.vector(t(resp4))
    resp5<-read_excel(dataset_forms[5])
    resp5<-as.vector(t(resp5[,2:(sets+1)]))
    resp5<-to.dummy(resp5,"resp")
    resp5<-as.vector(t(resp5))
    resp<-append(resp1,resp2)
    resp<-append(resp,resp3)
    resp<-append(resp,resp4)
    resp<-append(resp,resp5)
    dataset1<-do.call(rbind, replicate(nrespondents[1],dat[1:(sets*nalts),], simplify=FALSE))
    dataset2<-do.call(rbind, replicate(nrespondents[2],dat[((sets*nalts)+1):(sets*nalts*(nblocks-3)),], simplify=FALSE))
    dataset3<-do.call(rbind, replicate(nrespondents[3],dat[((sets*nalts*(nblocks-3))+1):(sets*nalts*(nblocks-2)),], simplify=FALSE))
    dataset4<-do.call(rbind, replicate(nrespondents[4],dat[((sets*nalts*(nblocks-2))+1):(sets*nalts*(nblocks-1)),], simplify=FALSE))
    dataset5<-do.call(rbind, replicate(nrespondents[5],dat[((sets*nalts*(nblocks-1))+1):(sets*nalts*nblocks),], simplify=FALSE))
    dat<-rbind(dataset1,dataset2,dataset3,dataset4,dataset5)
    dat<-cbind(dat,resp)
    dat$ALT<-rep(c(1:nalts),sets*(nrespondents[1]+nrespondents[2]+nrespondents[3]+nrespondents[4]+nrespondents[5]))



  }

  if (nblocks==6){
    if(optout==TRUE){
      nalts=nalts+1
      dat<-q
    }
    resp1<-read_excel(dataset_forms[1])#step1
    resp1<-as.vector(t(resp1[,2:(sets+1)]))
    resp1<-to.dummy(resp1,"resp")
    resp1<-as.vector(t(resp1))
    resp2<-read_excel(dataset_forms[2])#step1
    resp2<-as.vector(t(resp2[,2:(sets+1)]))
    resp2<-to.dummy(resp2,"resp")
    resp2<-as.vector(t(resp2))
    resp3<-read_excel(dataset_forms[3])
    resp3<-as.vector(t(resp3[,2:(sets+1)]))
    resp3<-to.dummy(resp3,"resp")
    resp3<-as.vector(t(resp3))
    resp4<-read_excel(dataset_forms[4])
    resp4<-as.vector(t(resp4[,2:(sets+1)]))
    resp4<-to.dummy(resp4,"resp")
    resp4<-as.vector(t(resp4))
    resp5<-read_excel(dataset_forms[5])
    resp5<-as.vector(t(resp5[,2:(sets+1)]))
    resp5<-to.dummy(resp5,"resp")
    resp5<-as.vector(t(resp5))
    resp6<-read_excel(dataset_forms[6])
    resp6<-as.vector(t(resp6[,2:(sets+1)]))
    resp6<-to.dummy(resp6,"resp")
    resp6<-as.vector(t(resp6))
    resp<-append(resp1,resp2)
    resp<-append(resp,resp3)
    resp<-append(resp,resp4)
    resp<-append(resp,resp5)
    resp<-append(resp,resp6)
    dataset1<-do.call(rbind, replicate(nrespondents[1],dat[1:(sets*nalts),], simplify=FALSE))
    dataset2<-do.call(rbind, replicate(nrespondents[2],dat[((sets*nalts)+1):(sets*nalts*(nblocks-4)),], simplify=FALSE))
    dataset3<-do.call(rbind, replicate(nrespondents[3],dat[((sets*nalts*(nblocks-4))+1):(sets*nalts*(nblocks-3)),], simplify=FALSE))
    dataset4<-do.call(rbind, replicate(nrespondents[4],dat[((sets*nalts*(nblocks-3))+1):(sets*nalts*(nblocks-2)),], simplify=FALSE))
    dataset5<-do.call(rbind, replicate(nrespondents[5],dat[((sets*nalts*(nblocks-2))+1):(sets*nalts*(nblocks-1)),], simplify=FALSE))
    dataset6<-do.call(rbind, replicate(nrespondents[6],dat[((sets*nalts*(nblocks-1))+1):(sets*nalts*nblocks),], simplify=FALSE))
    dat<-rbind(dataset1,dataset2,dataset3,dataset4,dataset5,dataset6)
    dat<-cbind(dat,resp)
    dat$ALT<-rep(c(1:nalts),sets*(nrespondents[1]+nrespondents[2]+nrespondents[3]+nrespondents[4]+nrespondents[5]+nrespondents[6]))


  }

  #creation of the mlogit dataset and creation of the formula fot the model
  for (i in 1:tot_col){
    dat[,i]=as.numeric(as.character(dat[,i]))
  }
  names<-names(dat[,1:tot_col])
  names<-append("resp",names)
  q<-as.formula(dat[,c(names)])

  dat<-mlogit_dataset<-mlogit.data(dat, shape="long",choice="resp",alt.var="ALT")
  mod<-mlogit(q,data=dat)
  res<-summary(mod)
  return(list(multinomial_logit_estimation=res,dataset=dat,names=names_dataset))


}

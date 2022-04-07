
SP_market_demand<-function(coefficients,optout=FALSE,nalts,simulations,c.var,base,xlab,ylab,type){
  library(varhandle)

#error control
  if (class(nalts)!="numeric"|class(c.var)!="numeric"|class(base)!="numeric"){
    stop("INSERT a numeric value of nalts/c.var/base")
  }
  if (nalts==1&optout==FALSE){
    stop("insert more than 1 alternative or set the parameter optout as TRUE")
  }
  if(length(simulations)!=length(base)){
  stop("the parameter simulations and the parameter base must have the same length")
  }
  if(class(xlab)!="character"|class(ylab)!="character"|class(type)!="character"){
    stop("the parameter xlab/ylab/type must be a character")
  }
  ###########################################################
  #set the base option adding the value that must be matched with the optout option
if (optout==TRUE){
  base<-append(1,base)
}
# set the coefficients
 if (optout==FALSE){
  coef=coefficients[-c(1:(nalts-1))]
  base_coef=coefficients[-c(1:(nalts-1))]
  }else{
   coef=coefficients[-c(1:nalts)]
   base_coef=coefficients[-c(1:(nalts-1))]
  }
  #simulate the values
  scen<-as.matrix(simulations)%*%coef
  ut_base<-base_coef*base
  ut_base<-sum(ut_base)
  prob_simulations<-((nalts-1)*exp(scen))/(((nalts-1)*exp(scen))+exp(ut_base))

  prob_simulations#8)probabilities with continuous variable
  scenario<-plot(simulations[,c.var],prob_simulations,xlab=xlab,ylab=ylab,type=type,main="scenario analysis")

  return(list(market_demand=prob_simulations))

  }

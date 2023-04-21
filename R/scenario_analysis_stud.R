
SP_market_demand<-function(design,coefficients,optout=FALSE,base,simulations,c.var=NULL,xlab=NULL,ylab=NULL,type=NULL){
  library(varhandle)

#error control
  if(length(simulations)!=length(base)){
  stop("the parameter simulations and the parameter base must have the same length")
  }

  dat_temp1<-subset(design,design$task==1&design$block==1)
    nalts<-length(dat_temp1[,1])


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
   coef=coefficients[-c(1:(nalts))]
   base_coef=coefficients[-c(1:(nalts-1))]
  }
  #simulate the values
  if(length(simulations[,1])>1){
  scen<-as.matrix(simulations)%*%coef
  }else{
  scen<-simulations*coef
  scen<-sum(scen)
  }
  ut_base<-base_coef*base
  ut_base<-sum(ut_base)
  if(optout==FALSE){
  prob_simulations<-((nalts-1)*exp(scen))/(((nalts-1)*exp(scen))+exp(ut_base))
  }else{
  prob_simulations<-((nalts)*exp(scen))/(((nalts)*exp(scen))+exp(ut_base))
  }

  prob_simulations#8)probabilities with continuous variable
  if(is.null(c.var)==FALSE){
  scenario<-plot(simulations[,c.var],prob_simulations,xlab=xlab,ylab=ylab,type=type,main="scenario analysis")
  }
  return(list(market_demand=prob_simulations*100,utility_status_quo=ut_base))

  }

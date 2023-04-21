\name{SP_market_demand}
\alias{SP_market_demand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Market demand simulation from a Stated preference(SP) survey
%%  ~~function to do ... ~~
}
\description{It provides market demand estimation obtained from a multinomial logit model based on a SP survey.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SP_market_demand(coefficients,optout,design,simulations,base,c.var=NULL,xlab=NULL,ylab=NULL,type=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{optout}{A logical vector that assumes the value TRUE if the optout option is present, FALSE otherwise.In this case the optout option can be considered a no choice option as well as a status quo option (with always the same attributes levels)
%%     ~~Describe \code{type_variable} here~~
}
\item{coefficients}{A numeric vector containing multinomial logit coefficients
%%     ~~Describe \code{type_variable} here~~
}
\item{simulations}{A simulation data.frame containing the varying attribute levels you wish to test for market demand analysis and the other fixed attributes levels.
%%     ~~Describe \code{type_variable} here~~
}
 \item{design}{The experimental design matrix
%%     ~~Describe \code{cand} here~~
}
\item{base}{A numeric vector indicating the attributes levels of the no choice or status quo option
%%     ~~Describe \code{type_variable} here~~
}
\item{c.var}{A numeric value indicating the column index of the attribute chosen for test in the parameter called simulations
%%     ~~Describe \code{type_variable} here~~
}
\item{xlab}{The label of the x axis  of the plot representing partworth utilities
%%     ~~Describe \code{type_variable} here~~
}
\item{ylab}{The label of the y axis of the plot representing partworth utilities
%%     ~~Describe \code{type_variable} here~~
}
\item{type}{The character vector indicating the graph tipology you want to draw (line ("l"),points ("p")etc..)
%%     ~~Describe \code{type_variable} here~~
}
%%     ~~Describe \code{x} here~~

}
\details{It provides market demand estimation obtained from a simple multinomial logit model based on a SP survey. In particular,it takes as input the coefficients of the multinomial logit model obtained through the function SP_mlogit.Then, according to the variable chosen, it estimates market share of the alternative created with respect to the freely selected variation range of the attribute levels (baseline alternative).
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{market_demand}{The estimated market demand vector
%%     ~~Describe \code{x} here~~
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Federov, V.V. (1972). Theory of optimal experiments. Academic Press, New York.

Wheeler, R.E. (2004). AlgDesign. The R project for statistical computing. (http://www.r-project.org).

Croissant, Y. (2012). Estimation of multinomial logit models in R: The mlogit Packages. R package version 0.2-2. URL: http://cran. r-project. org/web/packages/mlogit/vignettes/mlogit. pdf.
%% ~put references to the literature/web site here ~
}
\author{
Gabriele Iannaccone
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
optFederov, optBlock, dcm.design.cand, mlogit
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#Design creation
 attribute.names=list(delivery_cost = c(3,5,6,10),
                     delivery_time = c("Same day","2/3 days"),
                     delivery_location=c("Pick-up","Home"),
                     co2emissions=c(15,100,150,200,300),
                     carrier_drivers_benefits=c("Low","Medium","High"))
 design<-opt_design(condition=10,alt=2,set=10,block=2,
                    attribute.names=attribute.names,seed=635,
                    sign=c("-","-","+","-","+"))
 design<-design$design
 #coefficients calculation
 calculations<-SP_mlogit(attribute_type = c("C","NC","NC","C","NC"),
dataset_forms=c("C:/Users/utente/Downloads/Block 1 DELIVERY (Risposte).xlsx",
        "C:/Users/utente/Downloads/_Block 2 DELIVERY (Risposte).xlsx"),
        nrespondents=c(12,3),design=design,optout=FALSE)
#scenario 1 - market share
f<-SP_market_demand(coefficients=calculations$summary$coefficients,
    optout=FALSE,simulations=data.frame(5,1,0,100,0,0),
    base=c(5,1,1,300,0,0),design=design)
#scenario 2 - market share
 f2<-SP_market_demand(coefficients=calculations$summary$coefficients,
                      base=c(5,1,1,300,0,0),
                      simulations=data.frame(5,1,1,seq(15,300,by=0.1),0,0),
                      c.var=4,optout=FALSE,design=design,
                      xlab="co2emissions",ylab="market demand",type="l")



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

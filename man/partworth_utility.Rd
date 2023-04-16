\name{partworth_utility}
\alias{partworth_utility}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multinomial logit partworth utilities
%%  ~~function to do ... ~~
}
\description{It provides partworth utilities obtained from a simple multinomial logit model based on a Stated preference survey
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
partworth_utility(coefficients,design, nalts, optout, attribute_type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coefficients}{A numeric vector containing multinomial logit coefficients
%%     ~~Describe \code{coefficients} here~~
}
  \item{design}{The experimental design matrix
%%     ~~Describe \code{cand} here~~
}
  \item{nalts}{The number of alternatives per choice set in the experimental design matrix without considering the optout option if presents.
%%     ~~Describe \code{nalts} here~~
}
\item{optout}{A logical vector that assumes the value TRUE if the optout option is present, FALSE otherwise.In this case the optout option can be considered a no choice option as well as a status quo option (with always the same attributes levels)
%%     ~~Describe \code{type_variable} here~~
}
\item{attribute_type}{A character vector indicating if each attribute must be considered quantitative ("C") or qualitative ("NC")}
%%     ~~Describe \code{type_variable} here~~
}
}
\details{
It provides partworth utilities obtained from a simple multinomial logit model based on a SP survey. In particular,it takes as input the coefficients of the multinomial logit model of the function SP_mlogit.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{partworth_utility}{The partworth utilities vector
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
##estimate the multinomial logit coefficients from a SP questionnaire made via google form
design<-des_function(alt=2,set=10,block=2,typology=0,attribute.names=list(delivery_cost = c(3,5,6,10),delivery_time = c("Same day","2/3 days"),delivery_location=c("Home","Pick-up"),
co2emissions=c(15,100,150,200,300),carrier_drivers_benefits=c("Low","Medium","High")),seed=5)

 attribute.names=list(delivery_cost = c(3,5,6,10,15),
                      delivery_time = c("Same day","2/3 days"),
                      delivery_location=c("Pick-up","Home"),
                      co2emissions=c(15,100,150,200,300),
                      carriers_conditions=c("Low","Medium","High"))

 design<-opt_design(condition=10,alt=2,set=10,block=2,
                    attribute.names=attribute.names,seed=635,
                    sign=c("-","-","+","-","+"))

 design<-design$design


calculations<-SP_mlogit(sets=10,nalts=2,nblocks=2,
             attribute_type = c("C","NC","NC","C","NC"),
             dataset_forms=c("C:/Users/utente/Downloads/Block 1 DELIVERY (Risposte).xlsx",
            "C:/Users/utente/Downloads/_Block 2 DELIVERY (Risposte).xlsx"),
             nrespondents=c(12,3),optout=FALSE,design=design)


k<-partworth_utility(coefficients=calculations$summary$coefficients,
  design=design,
  nalts=2,optout=FALSE,
  attribute_type=c("C","NC","NC","C","NC"))




% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

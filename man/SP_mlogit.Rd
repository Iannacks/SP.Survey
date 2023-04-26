\name{SP_mlogit}
\alias{SP_mlogit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multinomial logit analysis of a Stated Preference survey
%%  ~~function to do ... ~~
}
\description{It provides a multinomial logit summary of a Stated Preference questionnaire made through Google Forms.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SP_mlogit(attribute_type,dataset_forms,nrespondents,optout,design
base=NULL,conf=FALSE,conf_level=NULL,other_attributes=NULL,sub_sample=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{dataset_forms}{A character vector containing the file path of each questionnaire made on google form
%%     ~~Describe \code{dataset_forms} here~~
}
  \item{nrespondents}{A numeric vector containing respondents' number for each questionnaire.
%%     ~~Describe \code{nrespondents} here~~
}
  \item{design}{The experimental design used
%%     ~~Describe \code{design} here~~
}
  \item{attribute_type}{
 A character vector indicating if each attribute must be considered quantitative ("C") or qualitative ("NC")
%%     ~~Describe \code{x} here~~
}
 \item{optout}{A logical vector that assumes the value TRUE if the optout option is present, FALSE otherwise.In this case the optout option can be considered a no choice option as well as a status quo option (with always the same attributes levels)
%%     ~~Describe \code{type_variable} here~~
}

 \item{base}{A numeric vector indicating the attributes of the no choice or the status quo option
%%     ~~Describe \code{type_variable} here~~
}
\item{conf}{A logical value that assumes the value TRUE if the coefficients have to take the value O if not statistically significant, FALSE otherwise
%%     ~~Describe \code{type_variable} here~~
}
\item{conf_level}{A numeric vector indicating the threshold above which a single coefficient assume the value 0 because not statistically significant
%%     ~~Describe \code{type_variable} here~~
}
\item{other_attributes}{A character vector indicating the variable that you want to use as a filter for the multinomial logit model

%%     ~~Describe \code{type_variable} here~~
}
\item{sub_sample}{A character vector indicating the level of the filtered variable
%%     ~~Describe \code{type_variable} here~~
}
}
\details{It provides  multinomial logit model estimation.It is derived from a Stated Preference(SP)questionnaire made through Google Forms. One can create a SP experimental design and use the choice tasks found to administer a google form questionnaire. The function takes as input the responses dataset of each questionnaire and the original experimental design from which it creates the final dataset and estimates the multinomial logit model.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{multinomial_logit_estimation}{The multinomial logit model summary
%%     ~~Describe \code{x} here~~
}
\item{dataset}{The multinomial logit dataset
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
#design creation
attribute.names=list(delivery_cost = c(3,5,6,10),
                     delivery_time = c("Same day","2/3 days"),
                     delivery_location=c("Pick-up","Home"),
                     co2emissions=c(15,100,150,200,300),
                     carrier_drivers_benefits=c("Low","Medium","High"))
 design<-opt_design(condition=10,alt=2,set=10,block=2,
                    attribute.names=attribute.names,seed=635,
                    sign=c("-","-","+","-","+"))
 design<-design$design
 #Coefficients calculation, don't RUN!!!!
 calculations<-SP_mlogit(attribute_type = c("C","NC","NC","C","NC"),
dataset_forms=c("C:/Users/utente/Downloads/Block 1 DELIVERY (Risposte).xlsx",
        "C:/Users/utente/Downloads/_Block 2 DELIVERY (Risposte).xlsx"),
        nrespondents=c(12,3),design=design,optout=FALSE)
#example with filter
calculations_filter<-SP_mlogit(attribute_type = c("C","NC","NC","C","NC"),
            dataset_forms=c("C:/Users/utente/Downloads/Block 1 DELIVERY (Risposte).xlsx",
            "C:/Users/utente/Downloads/_Block 2 DELIVERY (Risposte).xlsx"),
            nrespondents=c(12,3),design=design,optout=FALSE,
            other_attributes = "Are you a Roma Tre University member?",sub_sample="No")

f2<-SP_market_demand(coefficients=calculations_filter$summary$coefficients,
                     base=c(5,1,1,300,0,0),
                     simulations=data.frame(5,1,1,seq(15,300,by=0.1),0,0),
                     c.var=4,optout=FALSE,design=design,
                     xlab="co2emissions",ylab="market demand",type="l")


f<-SP_market_demand(coefficients=calculations_filter$summary$coefficients,
                    optout=FALSE,simulations=data.frame(5,1,0,100,0,0),
                    base=c(5,1,1,300,0,0),design=design)

k<-partworth_utility(coefficients=calculations_filter$summary$coefficients,
                     design=design,optout=FALSE,
                     attribute_type=c("C","NC","NC","C","NC"))

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

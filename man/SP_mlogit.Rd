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
SP_mlogit(attribute_type,seed,cand,nalts,nblocks,sets,dataset_forms,nrespondents,optout,
base=NULL,conf=FALSE,conf_level=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{dataset_forms}{A character vector containing the file path of each questionnaire made on google form
%%     ~~Describe \code{dataset_forms} here~~
}
  \item{sets}{The number of choice sets for each block in the experimental design matrix
%%     ~~Describe \code{sets} here~~
}
  \item{nalts}{The number of alternatives per choice set in the final design without considering the optout option if presents
%%     ~~Describe \code{nalts} here~~
}
  \item{nblocks}{The number of blocks(questionnaires)in the experimental design matrix
%%     ~~Describe \code{nblocks} here~~
}
  \item{nrespondents}{A numeric vector containing respondents' number for each questionnaire.
%%     ~~Describe \code{nrespondents} here~~
}
  \item{cand}{The full factorial matrix used to create the experimental design matrix
%%     ~~Describe \code{design} here~~
}
  \item{attribute_type}{
 A character vector indicating if each attribute must be considered quantitative ("C") or qualitative ("NC")
%%     ~~Describe \code{x} here~~
}
 \item{optout}{A logical vector that assumes the value TRUE if the optout option is present, FALSE otherwise.In this case the optout option can be considered a no choice option as well as a status quo option (with always the same attributes levels)
%%     ~~Describe \code{type_variable} here~~
}
 \item{seed}{Seed for a random number generator
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
}
\details{It provides  multinomial logit model estimation.It is derived from a Stated Preference(SP)questionnaire made through Google Forms. One can create a SP experimental design and use the choice tasks found to administer a google form questionnaire. The function takes as input the responses dataset of each questionnaire and the original experimental design from which it creates the final dataset and estimates the multinomial logit model. The function dcm.design.cand of the choiceDes package is mandatory to create the experimental design matrix. Furthermore, it also compulsory the usage of the function expand.grid to create the starting full factorial design.
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
#create a google form with the following attributes, choice sets and alts, then use the proper file path.Remember to set n-1 alternatives if the optout option is presents.
z<-SP_mlogit(sets=10,nalts=2,nblocks=2,seed=1,cand=expand.grid(delivery_cost = c(3,5,6,10),
   delivery_time = c("Same day","2/3 days"),delivery_location=c("Home","Pick-up"),co2emissions=c(15,100,150,200,300),
  carrier_drivers_benefits=c("Low","Medium","High")),attribute_type = c("C","NC","NC","C","NC"),
             dataset_forms=c("C:/Users/utente/Downloads/Block 1 DELIVERY (Risposte) (5).xlsx",
                             "C:/Users/utente/Downloads/_Block 2 DELIVERY (Risposte) (5).xlsx"),
             nrespondents=c(3,1),optout=FALSE)
#other example with the no choice option
z<-SP_mlogit(sets=10,nalts=2,nblocks=2,seed=1,cand=expand.grid(delivery_cost = c(3,5,6,10),
            delivery_time = c("Same day","2/3 days"),delivery_location=c("Home","Pick-up"),
            co2emissions=c(15,100,150,200,300),
            carrier_drivers_benefits=c("Low","Medium","High")),attribute_type = c("C","NC","NC","C","NC"),
            dataset_forms=c("C:/Users/utente/Downloads/Block 1 DELIVERY_optout (Responses).xlsx",
            "C:/Users/utente/Downloads/Block 2 DELIVERY_optout (Responses).xlsx"),
             nrespondents=c(1,1),optout=TRUE,base=c(0,0,0,0,0,0))
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

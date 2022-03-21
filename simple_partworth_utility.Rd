\name{partworth_utility_basic}
\alias{partworth_utility_basic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multinomial logit partworth utilities
%%  ~~function to do ... ~~
}
\description{It provides partworth utilities obtained from a simple multinomial logit model based on a Stated preference survey
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
partworth_utility_basic(coefficients, data, nalts, optout, attribute_type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coefficients}{A numeric vector containing multinomial logit coefficients
%%     ~~Describe \code{coefficients} here~~
}
  \item{data}{The SP dataset obtained from a SP Survey containing attributes, alternatives and responses columns
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
\details{It provides partworth utilities obtained from a simple multinomial logit model based on a SP survey. In particular,it takes as input the coefficients of the multinomial logit model of the function SP_mlogit_basic.In the dataset, attributes columns must stay before the other columns.Furthermore, alternatives column must be called "alt" while responses "resp".
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
 #multinomial logit coefficients estimation
 z<-SP_mlogit_basic(dataset=retailers_freight,attribute_type = c("NC","C","C"))
#partworth utilities
partworth_utility_basic(coefficients=z$coefficients,data=data,nalts=2,optout=TRUE,attribute_type=c("NC","C","C"))
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

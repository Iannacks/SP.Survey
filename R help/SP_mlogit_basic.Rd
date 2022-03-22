\name{SP_mlogit_basic}
\alias{SP_mlogit_basic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multinomial logit analysis of a Stated Preference(SP) survey
%%  ~~function to do ... ~~
}
\description{It provides a multinomial logit summary of a dataset based on a SP questionnaire
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SP_mlogit_basic(dataset,attribute_type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{attribute_type}{
 A character vector indicating if each attribute must be considered quantitative ("C") or qualitative ("NC")
%%     ~~Describe \code{x} here~~

%%     ~~Describe \code{x} here~~
}
 \item{dataset}{The SP dataset obtained from a SP Survey containing attributes,alternatives and responses columns
%%     ~~Describe \code{cand} here~~
}
}
\details{It provides  multinomial logit model estimation based on a SP dataset.The dataset must contain attributes, alternatives and responses columns. Furthermore, the attributes columns must be the first in order of appearance (reading from left to right), alternatives column must call "alt" while responses column "resp".
%%  ~~ If necessary, more details than the description above ~~
}
\value{\item{multinomial_logit_estimation}{The multinomial logit model summary
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
 #multinomial logit coefficients estimation
z<-SP_mlogit_basic(dataset=retailers_freight,attribute_type = c("NC","C","C"))
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

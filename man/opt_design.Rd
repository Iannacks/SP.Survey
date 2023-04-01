\name{opt_design}
\alias{opt_design}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fractional factorial design optimization
%%  ~~function to do ... ~~
}
\description{It provides optimized fractional factorial based on dominance control
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
opt_design(condition,attribute.names,alt,set,block,seed,sign)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{condition}{The percentage threshold of dominated choice tasks under which the function stops and provide the experimental design matrix
%%     ~~Describe \code{x} here~~
}
\item{attribute.names}{The list of attributes names and levels
%%     ~~Describe \code{x} here~~
}
\item{alt}{Alternatives number of the experimental design matrix
%%     ~~Describe \code{x} here~~
}
\item{set}{Choice task number for each block
%%     ~~Describe \code{x} here~~
}
\item{block}{The number of questionnaires to be administered
%%     ~~Describe \code{x} here~~
}
\item{seed}{The starting value for the algorithm
%%     ~~Describe \code{x} here~~
}
\item{sign}{ A character vector of length equal to the attributes number. A value can assume value "-" if the utility function decrease and value "+" for the opposite. Sign can be also a vector of length 0, if one wants to consider only total dominance without a-priori considerations. In this case sign=NULL
%%     ~~Describe \code{x} here~~
}
}
\details{
The function optimize a fractional factorial design with respect to a percentage of dominated questions selected by the user. The parameter sign defines the type of dominance that the function has to check. If sign is NULL, the function provides the choice tasks completely equal apart from an attribute level. If a-priori coefficients are provided, dominance are calculated based on these assumptions.
A fondamental assumption on coefficients is that the utility function for each attribute, considered as an ordinal attribute, is monotone (increasing or decreasing function)
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{design}{The optimized fractional factorial design
%%     ~~Describe \code{x} here~~
}
\item{dominance_level}{The percentage of dominated task
%%     ~~Describe \code{x} here~~
}
\item{task_dominated}{The index of the dominated choice tasks, observed on the final experimental design
%%     ~~Describe \code{x} here~~
}
\item{iterations}{Iterations number of the algorithm
%%     ~~Describe \code{x} here~~
}
\item{seed}{A numeric vector containing the final seed for the fractional factorial design
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
Aizaki H (2012). Basic Functions for Supporting an Implementation of Choice Experiments in R.Journal of Statistical Software, 50(C2). doi:10.18637/jss.v050.c02.
Wheeler, R.E. (2004). AlgDesign. The R project for statistical computing. (http://www.r-project.org).
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#example with a-priori coefficients
 opt_design2<-opt_design(condition=20,alt=2,set=3,block=2,seed=456,attribute.names=list(
  prezzo = c("1.5","3","4.5"),tempo = c("30m","1h"),
  comfort=c("basso","alto")),sign=c("-","-","+"))
#example without a-priori coefficients
opt_design1<-opt_design(condition=10,alt=2,set=3,block=2,seed=456,attribute.names=list(
   prezzo = c("1.5","3","4.5"),tempo = c("30m","1h"),
   comfort=c("basso","alto")),sign=NULL)
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

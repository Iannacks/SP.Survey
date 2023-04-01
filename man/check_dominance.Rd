\name{check_dominance}
\alias{check_dominance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dominance control
%%  ~~function to do ... ~~
}
\description{It provides the dominated choice tasks of the experimental design based on a-priori beta assumptions.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check_dominance(sign,design,attribute.names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sign}{ A character vector of length equal to the attributes number. A value can assume value "-" if the utility function decrease and value "+" for the opposite. Sign can be also a vector of length 0, if one wants to consider only total dominance without a-priori considerations. In this case sign=NULL
%%     ~~Describe \code{x} here~~
}
  \item{design}{An experimental design obtained from the function des_function
%%     ~~Describe \code{x} here~~
}
  \item{attribute.names}{A list containing attributes names and levels
%%     ~~Describe \code{x} here~~
}

}
\details{
The function provides the dominated choice tasks of an ortogonal or fractional factorial design obtained with the function des_function. The parameter sign defines the type of dominance that the function has to check. If sign is NULL, the function provides the choice tasks completely equal apart from an attribute level. If a-priori coefficients are provided, dominance are calculated based on these assumptions.
A fondamental assumption on coefficients is that the utility function for each attribute, considered as an ordinal attribute, is monotone (increasing or decreasing function)
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{starting_des}{The experimental design data.frame
%%     ~~Describe \code{x} here~~
}
\item{task_dominated}{The index of the dominated choice tasks, observed on the starting_des object
%%     ~~Describe \code{x} here~~
}
\item{dominance}{The percentage of dominated task
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
#create the experimental design
design1<-des_function(alt=2,typology=0,attribute.names=list(prezzo = c("1.5","3","4.5"),
        tempo = c("30m","1h"),comfort=c("basso","alto")),set=3,block=2,seed=802)
#check the dominance
dom_design_type2<-check_dominance(sign=c("-","-","+"),design=design1,attribute.names=list(
   prezzo = c("1.5","3","4.5"),tempo = c("30m","1h"),
   comfort=c("basso","alto")))

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

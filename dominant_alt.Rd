\name{dominant_alt}
\alias{dominant_alt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Optimal fractional factorial SP design
%%  ~~function to do ... ~~
}
\description{Choose the best optimal fractional factorial SP design using the minimum dominant alternatives criteria.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dominant_alt(condition,nb,sets,cand)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{condition}{a character vector of four elements expressing if expected marginal utility of contiguous attribute has the same sign.It can assume value yes, no or NA
%%     ~~Describe \code{x} here~~
}
 \item{nb}{The number of blocks or versions in the final design.
%%     ~~Describe \code{x} here~~
}
 \item{sets}{The number of choice sets in each version of the final design.
%%     ~~Describe \code{x} here~~
}
 \item{cand}{A data frame of columns representing factors in the design.
%%     ~~Describe \code{x} here~~
}
}
\details{Generates a set of balanced and blocked choice sets from a pre-generated candidate set and pick the design with minumum number of dominant alternatives. These are created considering the expected effect of utility on every single attribute comparing  expected signs of every contiguous attribute couple. In particular, if signs of expected utility of the first two attribute are the same, user will put the value "yes". Instead, if contiguous attribute (for example attribute 2 and 3 in column order) have opposite utility signs the value will be "no". If attribute number 4 and 5 are not present in the design the value will be "NA." The function works for experimental design with 2 alternatives and no more than 5 attributes.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 \item{Seeds}{The seed (one or more) of the design/s minimizing dominant alternatives
%%     ~~Describe \code{x} here~~
}
 \item{dominant_alternative_founded}{The number of dominant alternatives
%%     ~~Describe \code{x} here~~
}
 \item{design}{The design
%%     ~~Describe \code{x} here~~
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
optFederov, optBlock, dcm.design.cand
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
 create the candidate : combinations = expand.grid(price = c("10", "20", "30", "40","50"),
                           travel_time = c("75", "160","210","240"),
                           comfort = c("Low", "medium","high"))
## in this case price and travel_time have an expected decreasing utility, for this reason  the first

## element of the parameter condition is "yes". Instead, comparing travel time and comfort, in one case the utility decrease while in the other it increase, so the second element of the vector condition has

#value "no".Attribute 4 and 5 are not present so element 3 and 4 of the parameter condition have value "NA"
#don't forget to set the vector value to NA if attribute are not present.

dominant_alt(nb=2,sets=10,cand=combinations,condition=c("yes","no","NA","NA"))
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

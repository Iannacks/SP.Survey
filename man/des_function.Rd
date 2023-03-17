\name{des_function}
\alias{des_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Experimental design creation
%%  ~~function to do ... ~~
}
\description{It provides fractional factorial or ortogonal design
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
des_function(attribute.names,alt=2,set=NULL,block=NULL,typology,seed=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{attribute.names}{A list containing all the attributes and attributes levels
%%     ~~Describe \code{x} here~~
}
\item{typology}{A numeric vector that has value 0 if design is fractional factorial, 1 ortogonal
%%     ~~Describe \code{x} here~~
}
\item{alt}{A numeric vector containing the number of design alternatives
%%     ~~Describe \code{x} here~~
}
\item{set}{A numeric vector containing the choice tasks number
%%     ~~Describe \code{x} here~~
}
\item{block}{A numeric vector containing the blocks number
%%     ~~Describe \code{x} here~~
}
\item{seed}{A numeric vector containing the seed for the fractional factorial design
%%     ~~Describe \code{x} here~~
}
}
\details{
It provides fractional factorial or ortogonal design. For the ortogonal design, according to the total number of attributes levels chosen by the user, the function will ask the respondents the number of total questions and choice task.

%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{design}{The experimental design data.frame
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

\seealso{optFederov, optBlock, dcm.design.cand,support.CEs,DoE.base
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#in this example a fractional factorial design is created
design<-des_function(alt=2,set=10,block=2,typology=0,attribute.names=list(delivery_cost = c(3,5,6,10),delivery_time = c("Same day","2/3 days"),delivery_location=c("Home","Pick-up"),
co2emissions=c(15,100,150,200,300),carrier_drivers_benefits=c("Low","Medium","High")),seed=5)
  }
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

\name{SP_optout}
\alias{SP_optout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Optout option in a Stated Preference (SP) dataset
%%  ~~function to do ... ~~
}
\description{
Add in a SP dataset the optout alternative
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SP_optout(design,sets,nalts,nblocks,nattr,attribute_tpe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{
  The experimental design used to create the SP questionnaire/s through Google form
%%     ~~Describe \code{x} here~~
}
 \item{sets}{
 The number of choice sets for each block in the final design
%%     ~~Describe \code{x} here~~
}
 \item{nalts}{
 The number of alternatives per choice set in the final design
%%     ~~Describe \code{x} here~~
}
 \item{nblocks}{The number of blocks(questionnaires)in the final design
%%     ~~Describe \code{x} here~~
}
 \item{nattr}{
 The number of attributes in the final design
%%     ~~Describe \code{x} here~~
}
\item{attribute_type}{
 a character vector indicating if each attribute must be considered quantitative (C),qualitative (NC)
%%     ~~Describe \code{x} here~~
}
}
\details{
This function add to the the SP design created with the function dcm.design.cand the optout option.It modifies the dataset accordingly by setting the parameter attribute_type.

%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{final design}{The final design modified
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
Gabriele Iannaccone
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
## create a full factorial design
combinations = expand.grid(price = c("10", "20", "30", "40","50"),
                           travel_time = c("75", "160","210","240"),
                           comfort = c("Low", "medium","high"),color=c("red","blue"))
## create an experiment design
set.seed(1)
design <- dcm.design.cand(combinations, sets=5, nb=2, alts=2)#step3
design$levels
colnames(design$levels)<-c("card","vers","task","price","travel time","comfort","green")#set colnames
#use the function
#z<-SP_optout(design=design$levels,nalts=2,sets=5,nblocks=2,nattr=4,attribute_type=c("NC","NC","C","NC"))
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

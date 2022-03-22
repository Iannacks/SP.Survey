\name{retailers_freight}
\alias{retailers_freight}
\docType{data}
\title{retailers_freight
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("retailers_freight")}
\format{
  A data frame with 2661 observations on the following 3 variables.
  \describe{
    \item{\code{piazz}}{a numeric vector containing the number of loading/unloading bays (L/U) for freight commercial vehicles}
    \item{\code{prob}}{a numeric vector indicating the probability to find the L/U free }
    \item{\code{tariffa}}{a numeric vector indicating the entrance fee to the restricted traffic zone in Rome}
    \item{\code{alt}}{a numeric vector that indicate the alternative considered in a specific choice set}
    \item{\code{resp}}{a numeric vector that assume the value 1, if the alternative have been chosen, 0 otherwise}
  }
}
\details{ This dataset is a part of a broader research on Roman LTZ concerning the behaviour of urban freight stakeholders' in Rome.
The respondents was asked to choose between a set of freight policies characterized by the three attributes "piazza","prob" and "tariffa" and a status quo option, corresponding to the urban freight situation at that time.
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
1)Marcucci, E., Stathopoulos, A., Gatta, V., & Valeri, E. (2012). A Stated Ranking Experiment to Study Policy Acceptance: the Case of Freight Operators in Rome's LTZ. A Stated Ranking Experiment to Study Policy Acceptance: the Case of Freight Operators in Rome's LTZ., 11-30.
2)Marcucci, E., Gatta, V., Stathopoulos, A., & Valeri, E. (2011). Designing an efficient stated ranking experiment for ex-ante urban freight policy evaluation in a three agent type context: retailers, own-account and carriers. Available from Internet: http://www. sietitalia. org/wpsiet/WP-SIET-EMarucci-Vgatta-AStathopoulos-EValeri2011. pdf.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{

%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(retailers_freight)

}
\keyword{datasets}

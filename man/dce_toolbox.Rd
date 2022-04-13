\name{dce_toolbox}
\alias{dce_toolbox}
\title{Generate Efficient Optimal and Bayesian DCEs}
\description{Generates experimental designs for DCEs. (Backend of DCEtool) .}

\usage{
  dce_toolbox(attributes, csets, alts, nochoice, priors, alg)
}

\arguments{
  \item{attributes}{A vector where each number represents an attribute and its values the number of levels.}
  \item{csets}{An integer indicating the number of sets in the DCE.}
  \item{alts}{An integer indicating the number of alternatives in each set.}
  \item{nochoice}{A boolean indicating whether there is an opt-out option (TRUE) or not (FALSE)}
  \item{priors}{A vector indicating the prior parameters of the conditional logit model.}
  \item{alg}{A string indicating the optimization algorithm: "cea" or "fedorov".}
}

\value{
  \item{design}{The design matrix}
  \item{DB-error}{The Bayesian D-error if the optimization algorithm is "cea"}
  \item{D-error}{The D-error if the optimization algorithm is "fedorov"}
  \item{details}{A string compiling the details of the procedure}
}

\examples{
  dce_toolbox(c(2,3,3), 12, 2, 
  TRUE, c(0,0,0,0,0,0), "fedorov")
  
  dce_toolbox(c(2,3,3), 12, 2,
  TRUE, mvtnorm::rmvnorm(100,
                        c(0,0,0,0,0,0),
                        diag(length(c(0,0,0,0,0,0))))
      , "cea")
}
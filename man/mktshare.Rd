\name{mktshare}
\alias{mktshare}
\docType{data}
\title{Synthetic Market Share Data
}
\description{This data set is a synthetic data set generated from a random utility model with linear preferences for attributes of a product.
}
\usage{data(mktshare)}
\format{
  A data frame with 100 observations on the following 10 variables.
  \describe{
    \item{\code{product}}{a numeric vector whose values indicate the product number within market}
    \item{\code{market}}{a numeric vector whose values indicate the market number (should be a factor)}
    \item{\code{y}}{a numeric vector equal to the log(share/outshare) to be used as the response variable (as suggested by Berry 1994).}
    \item{\code{share}}{a numeric vector equal to the market share that product i has in market m.}
    \item{\code{outshare}}{a numeric vector equal to the fraction of the individuals who buy the outside good.}
    \item{\code{x1}}{a numeric vector whose values represent attribute one.}
    \item{\code{x2}}{a numeric vector whose values represent attribute two.}
    \item{\code{p}}{a numeric vector whose values represent the product's price.}
    \item{\code{z1}}{a numeric vector that is a potential instrument}
    \item{\code{z2}}{a numeric vector that is another potential instrument}
  }
}
\details{This is a simulated data set that I use to show how to estimate IV regression.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(mktshare)
}
\keyword{datasets}

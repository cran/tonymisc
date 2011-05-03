\name{laptop}
\alias{laptop}
\docType{data}
\title{Laptop Data / Observational
}
\description{This is an example data set from an online price comparison website on laptop prices.  I selected the top 10 brands according to the website and I extracted the first 20 prices in the list sorted by "relevance."  
}
\usage{data(laptop)}
\format{
  A data frame with 200 observations on the following 3 variables.
  \describe{
    \item{\code{Brand}}{a factor with levels \code{Acer} \code{Apple} \code{ASUS} \code{Dell} \code{HP} \code{Lenovo} \code{MSI} \code{Panasonic} \code{Sony} \code{Toshiba}}
    \item{\code{Product}}{a numeric vector indicating the position in the search by "relevance."}
    \item{\code{Price}}{a numeric vector indicating the price in US dollars for the laptop computer.}
  }
}
\details{This data set is a convenience sample of laptop prices.  It should not be used for any purpose other than demonstrating methods and trying out functions in R.
}
\source{http://www.bizrate.com/
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(laptop)
## maybe str(laptop) ; plot(laptop) ...
}
\keyword{datasets}

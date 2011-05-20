\name{sum_iv}
\alias{sum_iv}
\title{ Summary Printing Command for iv() objects
}
\description{This command is the equivalent of a summary command for IV regression objects created using iv() from the tonymisc library.
}
\usage{
sum_iv(reg_iv, first = FALSE, ftest = FALSE, second = TRUE, cc = FALSE)
}
\arguments{
  \item{reg_iv}{A iv()-created IV regression object.
}
  \item{first}{If TRUE, display the first stage regression output. Default FALSE.
}
  \item{ftest}{If TRUE, display the F-test for relevance.  Default FALSE.
}
  \item{second}{If TRUE, display the second-stage regression output.  Default TRUE.
}
  \item{cc}{If TRUE, display the Anderson (2003) Canonical Correlation test for first-stage relevance.  Default FALSE.
}
}
\details{
}
\value{The method just prints output to the screen.
}
\references{
}
\author{ J. Anthony Cookson
}
\note{
}

\seealso{
\code{\link{iv}},\code{\link{mtable}}
}
\examples{
data(mktshare)
myiv   = iv(y~x1+x2+p, p~z1+z2, data=mktshare)
sum_iv(myiv) 
sum_iv(myiv, ftest=TRUE)
sum_iv(myiv, ftest=TRUE, first=TRUE)
sum_iv(myiv, ftest=TRUE, first=TRUE, cc=TRUE)

myiv   = iv(y~x1+x2+p, x2+p~z1+z2, data=mktshare)
sum_iv(myiv) 
sum_iv(myiv, ftest=TRUE)
sum_iv(myiv, ftest=TRUE, first=TRUE)
sum_iv(myiv, ftest=TRUE, first=TRUE, cc=TRUE)
}
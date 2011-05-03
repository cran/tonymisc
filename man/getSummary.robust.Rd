\name{getSummary.robust}
\alias{getSummary.robust}
\alias{getSummary.ivreg}
\alias{getSummary.gls}
\alias{getSummary.tonyiv}
\title{getSummary Methods for Extending mtable()
}
\description{These functions are getSummary methods for extending the mtable() to report robust standard errors (.robust), IV regression estimates (.ivreg) and summary output from GLS fits (.gls).  The robust() command allows the user not only to specify the type of robust standard error correction, but also to specify the set of variables to be included in the summary output.  Of note, the IV regression method reports additional statistics to output from summary.ivreg (first-stage F-stat, overidentifying restrictions).
}
\usage{\method{getSummary}{robust}(obj, alpha = 0.05, ...)}
\arguments{
  \item{obj}{An object of type that matches the method. These functions provide support for ivreg and gls objects as well as objects coerced to be of class "robust."
}
  \item{alpha}{Significance Level
}
  \item{\dots}{Arguments passed to getSummary}
}
\details{
}
\value{Returns a list of objects to be referenced by mtable.  Do not use these methods directly. Use mtable, which will automatically call these methods for class objects that have been implemented.
}
\references{}
\author{J. Anthony Cookson}
\note{}

\seealso{\code{\link{toLatex}} \code{\link{mtable}} \code{\link{robust}} \code{\link{setTabDefault}} }

\examples{

## --------------------------------- ##
## Example 1: Extensions for Robust  ##
## --------------------------------- ##

library(memisc)
library(tonymisc)

data(mktshare)

mkt.lm = lm(y~x1+x2+p+z1, data=mktshare)

## Given an "lm" object robust formats options for mtable's .robust extension ##
setTabDefault()

mkt.rob = robust(mkt.lm)                ## Default is hc3 heteroskedasticity corrrection
mkt.rob2 = robust(mkt.lm, type = "hc1") ## Can specify hc0, hc1, hc2, hc3
mkt.rob3 = robust(mkt.lm, type = "no")  ## Can specify type = "no" for not corrected SEs

mtable(mkt.rob, mkt.rob2, mkt.rob3)     ## Coding Kludge: no correction is reported as type = hc9

mkt.rob4= robust(mkt.lm, keep="p")          ## keeps intercept by default
mkt.rob5= robust(mkt.lm, keep=c("x1","p"))  ## can keep more than one
mkt.rob6= robust(mkt.lm, keep=c("x1", "p"), drop="(Intercept)") ## Drop intercept.

mtable(mkt.rob4, mkt.rob5, mkt.rob6)

mkt.rob7= robust(mkt.lm, type="no", drop=c("x1","z1"))   ## type="no": extends selective output to lm
mkt.rob8= robust(mkt.lm, drop="(Intercept)", keep=c("x1", "x2")) ## Can drop intercept and specify keep

mtable(mkt.rob7, mkt.rob8)

## ------------------------------- ##
## Example 2: Extensions for ivreg ##
## ------------------------------- ##
library(memisc)
library(tonymisc)
library(AER)

data(mktshare)

mkt.aer  = ivreg(y~x1+x2+p|x1+x2+z1+z2,data=mktshare)
mkt.aer2  = ivreg(y~x1+x2+p|x1+z1+z2,data=mktshare)

mtable(mkt.aer,mkt.aer2)


## ----------------------------- ##
## Example 3: Extensions for gls ##
## ----------------------------- ##
library(memisc)
library(tonymisc)
library(nlme)

data(mktshare)

mkt.gls1  = gls(y~x1, data=mktshare,correlation=corAR1(form=~1))
mkt.gls2  = gls(y~x1+x2, data=mktshare,correlation=corAR1(form=~1))
mkt.gls3  = gls(y~x1, data=mktshare,correlation=corARMA(p=2,q=2,form=~1))

mtable(mkt.aer,mkt.aer2)
}
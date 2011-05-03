\name{robust}
\alias{robust}
\title{Robust Objects / Selective Variable Reporting for mtable()}
\description{This function coerces lm objects to type robust and allows the user to specify the type of heteroskedasticity correction.  The function is intented to be used with summaryR to interface with mtable() to produce nicely-formatted output.}
\usage{robust(model, type = "hc3", keep=NULL, drop=NULL)}
\arguments{
  \item{model}{A linear models object.}
  \item{type}{The type of heteroskedasticity correction you want to specify.  Options include "no", "hc0", "hc1", "hc2", "hc3" and "hc4" with "hc3" as the default. For more information on these corrections see the help file for hccm.  A user would specify "no" to selectively display output from a lm() fit using mtable().}
  \item{keep}{A vector of variable names you would like to display using mtable().  Defaults to NULL.}
  \item{drop}{A vector of variable names you would like to not display using mtable().  Defaults to NULL.  If both keep and drop are NULL, the default is to print output for all variables in the model.  If keep and drop are specified, keep takes precedence, except in the case of dropping an intercept.  See examples below.}
}
\details{}
\value{Returns an object of type "robust." }
\references{}
\author{J. Anthony Cookson}
\note{}

\seealso{\code{\link{hccm}} \code{\link{summaryR}} \code{\link{getSummary.robust}} \code{\link{mtable}}
}
\examples{
library(memisc)
library(tonymisc)

data(mktshare)

mkt.lm = lm(y~x1+x2+p+z1, data=mktshare)

## Given an "lm" object robust formats options for mtable's .robust extension ##
setRobustDefault()

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
}
\name{iv}
\alias{iv}
\title{Another Two-Stage Least Squares Command
}
\description{ This command performs 2SLS.  Relative to other IV regression commands (like ivreg() in AER or tsls() in sem), this function automatically conducts a test for instrument relevance and provides easy access to the first stage regression estimates.
}
\usage{ iv(second, first, data) }

\arguments{
  \item{second}{A formula specifying the second stage regression.  This should include all regressors (endogenous and exogenous).}
  \item{first}{A formula statement specifying the first stage regression(s).  The LHS of this formula is the set of endogenous regressors separated by + signs.  The RHS of the formula contains the excluded instruments; the function automatically adds exogenous regressors to the RHS of the first st age.}
  \item{data}{A data frame object.}
}
\details{ After creating an IV regression object with iv(), you have two options to produce output: (1) Apply mtable() to the iv object, (2) Apply sum_iv() to the iv object.  The former method makes it slightly easier to port the results into TeX tables while the latter method behaves more like a summary command (and provides more detail).
}
\value{
  \item{second}{ A summary.lm object that contains the second stage estimation results with the appropriately-adjusted standard errors }
  \item{first}{ A summary.lm object that contains the first stage of the IV regression }
  \item{ftest}{ An anova object containing the result of an F-test for instrument relevance in the IV regression }
  \item{cc}{ First-stage test of relevance that can handle testing for relevance with multiple endogenous regressors.  The null hypothesis of this test is that the minimum canonical correlation equals zero (see Anderson 2003).}
%% ...
}
\references{T. W. Anderson (2003). An Introduction to Multivariate Statistical Analysis, Third Edition, Wiley. 
}
\author{ J. Anthony Cookson }
\note{
%%  ~~further notes~~
}


\seealso{ 
\code{\link[sem]{tsls} }, \code{\link[AER]{ivreg} }, \code{\link[memisc]{mtable} } 
}
\examples{

library(tonymisc)

data(mktshare)

## Create the object ##
myiv = iv(y~x1+x2+p, p~z1+z2, data=mktshare)

sum_iv(myiv)        ## Summarize Organically
mtable(myiv)        ## mtable() summary

## An example with multiple instruments ##
myiv2 = iv(y~x1+x2+p, x2+p~z1+z2, data=mktshare)

sum_iv(myiv2, cc=TRUE)
mtable(myiv, myiv2)

}
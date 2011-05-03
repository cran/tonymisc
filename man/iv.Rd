\name{iv}
\alias{iv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Another Two-Stage Least Squares Command
}
\description{ This command performs 2SLS for the case of one endogenous variable and potentially many instruments.  Relative to other IV regression commands (like ivreg() in AER or tsls() in sem), this function automatically conducts a test for instrument relevance and provides easy access to the first stage regression estimates.  For whatever reason, this function also works better on large data sets like that used in Angrist in Krueger (1991).  The disadvantage of this command is that it does not work for multiple endogenous regressors.  If your problem has this feature, use your favorite method among ivreg(), tsls(), systemfit() or plm().
}
\usage{ iv(second, first, data) }

\arguments{
  \item{second}{A formula specifying the second stage regression.  This should include all regressors (endogenous and exogenous).}
  \item{first}{A formula statement specifying the first stage regression.  The LHS of this formula is the endogenous regressor.  The RHS of the formula contains instruments.}
  \item{data}{A data frame object.}
}
\details{ After creating an IV regression object with iv(), you have two options to produce output: (1) Apply mtable() to the tonyiv object, (2) Apply sum_iv() to the tonyiv object.  The former method makes it slightly easier to port the results into TeX tables while the latter method behaves more like a summary command.
}
\value{
  \item{second}{ A summary.lm object that contains the second stage estimation results with the appropriately-adjusted standard errors }
  \item{first}{ A summary.lm object that contains the first stage of the IV regression }
  \item{ftest}{ An anova object containing the result of an F-test for instrument relevance in the IV regression }
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{ J. Anthony Cookson }
\note{
%%  ~~further notes~~
}


\seealso{ 
\code{\link[sem]{tsls} }, \code{\link[AER]{ivreg} }, \code{\link[memisc]{mtable} } 
}
\examples{

library(memisc)
library(tonymisc)

data(mktshare)


## Create the object ##
myiv = iv(y~x1+x2+p, p~z1+z2, data=mktshare)

sum_iv(myiv)        ## Summarize Organically

setTabDefault()     ## Summarize using mtable(), easy to port to LaTeX
mtable(myiv)

}
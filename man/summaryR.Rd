\name{summaryR}
\alias{summaryR}
\title{Summary Function for Robust Standard Errors
}
\description{This function allows the user to summarize regression output using heteroskedasticity-consistent standard errors.  The function is just an adaptation of summaryHCCM.lm written by John Fox, but some details have been changed to allow the function to interface with mtable through getSummary.robust.
}
\usage{summaryR(model, type = c("hc3", "hc2", "hc1", "hc0", "hc4"), ...)}
\arguments{
  \item{model}{A linear models object.
}
  \item{type}{The type of heteroskedasticity correction you want to specify.  Options include hc0, hc1, hc2, hc3 and hc4. The default is hc3. For more information on these corrections see the help file for hccm.
}
  \item{\dots}{
}
}
\value{The function returns a linear models object with heteroskedasticity-consistent standard errors.
}
\author{J. Anthony Cookson (adapted the code from John Fox)
}

\seealso{\code{\link{hccm}} \code{\link{linearHypothesis}} 
}
\examples{
data(mktshare)   ## Synthetic Market Share Data
mkt.lm = lm(y~x1+x2, data=mktshare)
summary(mkt.lm)   ## For comparison
summaryR(mkt.lm)  ## For demonstration (SEs are slightly different)

}
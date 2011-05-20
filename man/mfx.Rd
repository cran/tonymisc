\name{mfx}
\alias{mfx}
\alias{print.mfx}
\title{A Marginal Effects Post Estimation Command for glm
}
\description{This function returns Stata-like probit and logit marginal effects when applied to a glm object that was created using a probit or logit binomial link function.  This function and its S3 print method are for printing summary()-like output to the screen as one might do in interactive mode.  If you wish to port marginal effects output to a LaTeX table, use \code{\link{mfx_me}} to coerce the glm object to "mfx" and then apply mtable() directly.
}
\usage{
mfx(x)
}
\arguments{
  \item{x}{A .glm fitted object created using a binomial family with a probit or logit link function.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a standard glm list (see \code{\link{glm}} for details), but with marginal effects in place of ecoefficients and an object of class "mfx." 
}
\references{
}
\author{J. Anthony Cookson (adaptation of code from a blog post by TimeSeriesIreland)
}
\note{
}

\seealso{\code{\link{mfx_me}} \code{\link{glm}} \code{\link{getSummary.mfx}}
}
\examples{

library(Ecdat)
data(Yogurt)

yop <- glm(I(choice=="yoplait")~price.yoplait+feat.yoplait, family=binomial, data=Yogurt)
yo_mfx <-mfx(yop)    ## Creates "mfx" object
yo_mfx               ## Calls print.mfx to print to screen

## Alternative Method to use with mtable() ##
# yo_me <- mfx_me(yop) ## Create "mfx" object to trick mtable()
# mtable(yop, yo_me)   ## produces a table with nice output
}
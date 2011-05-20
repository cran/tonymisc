\name{mfx_me}
\alias{mfx_me}
\title{Marginal Effects }
\description{This function coerces glm objects to type mfx.}
\usage{mfx_me(x)}
\arguments{
  \item{x}{A glm object using binomial link function probit or logit.  The getSummary.mfx method doesn't know what to do with other types of glm objects.}
}
\details{}
\value{Returns an object of type "mfx." }
\references{}
\author{J. Anthony Cookson}
\note{}

\seealso{\code{\link{getSummary.mfx}} \code{\link{mtable}}
}
\examples{
library(tonymisc)
library(Ecdat)
data(Yogurt)

yop <- glm(I(choice=="yoplait")~price.yoplait+feat.yoplait, family=binomial, data=Yogurt)
yo_me <- mfx_me(yop) ## Create "mfx" object to trick mtable()

mtable(yop, yo_me) ## produces a table with nice output
toLatex(mtable(yop, yo_me)) ## Produces LaTeX code
}
\name{setTabDefault}
\alias{setTabDefault}
\alias{setGLSDefault}
\alias{setIVDefault}
\alias{setRobustDefault}
\title{Setting Summary Defaults for Extending mtable()
}
\description{These functions provide a convenient method to set the summary template for mtable() -- as opposed to using the setSummaryTemplate() command directly.  Each command establishes a sensible template for objects of a particular class: setGLSDefault() is for gls objects (gls() from the nlme library), setIVDefault() is for ivreg objects (ivreg() from the AER library), setRobustDefault() is for robust objects (robust() from the tonymisc library).  Finally, setTabDefault() calls all of these other commands.
}
\usage{
setTabDefault()
setGLSDefault()
setIVDefault()
setRobustDefault()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
}
\references{
%% ~put references to the literature/web site here ~
}
\author{J. Anthony Cookson
}
\note{
%%  ~~further notes~~
}

\seealso{ \code{\link{mtable}}  \code{\link{gls}}  \code{\link{ivreg}}
}
\examples{}
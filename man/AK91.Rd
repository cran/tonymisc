\name{AK91}
\alias{AK91}
\docType{data}
\title{Angrist and Krueger (1991)'s Compulsory Schooling Data (1970 Census Cohort Only)
}
\description{This data set is a reformatted version of Angrist and Krueger's compulsory schooling data.
}
\usage{data(AK91)}
\format{
  A data frame with 247199 observations on the following 11 variables.
  \describe{
    \item{\code{age}}{Age in years}
    \item{\code{ageq}}{Age in quarter-years}
    \item{\code{educ}}{Years of education}
    \item{\code{lwklywge}}{logged-weekly wage}
    \item{\code{married}}{indicator variable == 1 if married}
    \item{\code{census}}{1970 census (left over from original data set). This is a constant ==70}
    \item{\code{qob}}{Quarter of Birth, a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{race}}{an indicator variable == 1 if black}
    \item{\code{smsa}}{an indicator variable == 1 if center city}
    \item{\code{yob}}{Year of birth, a factor with levels \code{1920} \code{1921} \code{1922} \code{1923} \code{1924} \code{1925} \code{1926} \code{1927} \code{1928} \code{1929}.  Technically, there are other levels, but those don't occur within this subsample of Angrist and Krueger (1991)}
    \item{\code{region}}{Region of Residence, a factor with levels \code{enocent} \code{esocent} \code{midatl} \code{mt} \code{neweng} \code{other} \code{soatl} \code{wnocent} \code{wsocent}}
  }
}

\source{http://econ-www.mit.edu/faculty/angrist/data1/data/angkru1991
}
\references{ Does Compulsory School Attendance Affect Schooling and Earnings? Joshua D. Angrist and Alan B. Krueger. The Quarterly Journal of Economics. Vol. 106, No. 4 (Nov., 1991), pp. 979-1014 
}
\examples{

## Reproduce the First Two Columns of Table IV, Angrist and Krueger (1991) ##
library(tonymisc)
library(memisc)
data(AK91)
ak.lm1 = lm(lwklywge~yob+educ, data=AK91)
ak.iv1 = iv(lwklywge~yob+educ,educ~yob:qob, data=AK91)
setTabDefault()
mtable(ak.lm1, ak.iv1)
}
\keyword{datasets}

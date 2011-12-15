\name{lagger}
\alias{lagger}
\title{ Lagger Function for Lagging Time Series data
}
\description{This command lags (or leads) variables within a data frame that contains variable named "date" that has class "Date".
}

\usage{
lagger(data, by.var, lag.vars, num.lags, direction, freq = "daily")
}
\arguments{
  \item{data}{A data.frame object with a panel structure.
}
  \item{by.var}{character string with a unique identifier for each time period.
}
  \item{lag.vars}{character vector of variable names (must be defined within data) of variables to lag or lead.
}
  \item{num.lags}{numeric vector that indicates the number of lags to generate for each variable within lag.vars.
}
  \item{direction}{Specify "lag" or "lead" to indicate whether to generate lags or leads.
}
  \item{freq}{Specify "daily", "monthly" or "quartly" to indicate the frequency of the data.
}
}
\value{The function returns a data frame with new columns for the lags and leads generated.
}
\author{ Sean Golden and Ian Muir (with adaptations by J. Anthony Cookson)
}

\seealso{
\code{\link{as.Date}},\code{\link{Date}}
}
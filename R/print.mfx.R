
print.mfx = function(x, ...){ 
      if (qq<-x$link=="logit"){ cat("This is a Logit Model","\n")
  	}
		else if (qq<-x$link=="probit"){ cat("This is a Probit Model","\n")
		}
		else {cat("","\n")}
	cat("", "\n")
	cat("Reporting Marginal Effects, Evaluated at the Mean", "\n")
	cat("", "\n")
	printCoefmat(x$coefficients[-1,], P.values=TRUE, has.Pvalue=TRUE)
	cat("", "\n")
	cat("Observations:", x$obs, "\n")
	cat("", "\n")
	cat("Likelihood-Ratio Test:", "\n")
	printCoefmat(x$LRTest, P.values=TRUE, has.Pvalue=TRUE)
	cat("", "\n")
  
  }
getSummary.tonyiv= function (obj, alpha = 0.05, ...) 
{
    setTabDefault()
    smry <- obj$second
    coef <- smry$coef    
    numdf <- unname(smry$fstatistic[2])
    dendf <- unname(smry$fstatistic[3])
    lower <- coef[, 1] + coef[, 2]*qt(p = alpha/2, df = dendf)
    upper <- coef[, 1] + coef[, 2]*qt(p = 1 - alpha/2, df = dendf)
    coef <- cbind(coef, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    sigma <- smry$sigma
    r.squared <- smry$r.squared
    adj.r.squared <- smry$adj.r.squared
    F <- unname(smry$fstatistic[1])
    if(class(obj$ftest)[1]=="list"){
      fame = NULL
      for(i in 1:length(obj$ftest)){
        tempo = obj$ftest[[i]]
        fame  = c(fame,tempo[2,5])
      }
      Ffirst <- mean(fame)
      pfirst <- -1
      cat("Multiple Instruments: Look at CC p-value for test of relevance \n")
    }
    else{
    Ffirst <-obj$ftest[2,5]
    pfirst <-obj$ftest[2,6]
    }
    CC_pval <-as.numeric(obj$CC_Test[1,3])
    Sarg_pval <- as.numeric(obj$Sargan[1,2])
    
    p <- pf(F, numdf, dendf, lower.tail = FALSE)
    N <- sum(smry$df[1:2])
    sumstat <- c(sigma = sigma, r.squared = r.squared, adj.r.squared = adj.r.squared, Sarg_pval = Sarg_pval,
        F = F, Ffirst=Ffirst,pfirst=pfirst, CC_pval = CC_pval, numdf = numdf, dendf = dendf, p = p, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = obj$second$call)
}
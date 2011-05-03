getSummary.tonyiv= function (obj, alpha = 0.05, ...) 
{
   
   class(obj) = "lm"
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
    Ffirst <-obj$ftest[2,5]
    pfirst <-obj$ftest[2,6]
    p <- pf(F, numdf, dendf, lower.tail = FALSE)
    N <- sum(smry$df[1:2])
    type <- ifelse(obj$type=="no", 9,as.numeric(substring(obj$type, 3, 3)))
    sumstat <- c(sigma = sigma, r.squared = r.squared, adj.r.squared = adj.r.squared, 
        F = F, Ffirst=Ffirst,pfirst=pfirst, numdf = numdf, dendf = dendf, p = p, N = N, type = type)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = obj$second$call)
}
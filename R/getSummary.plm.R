getSummary.plm = function (obj, alpha = 0.05, ...) 
{
    setTabDefault()
    smry <- summary(obj)
    coef <- smry$coef
    numdf <- unname(smry$fstatistic[3]$parameter[1])
    dendf <- unname(smry$fstatistic[3]$parameter[2])
    lower <- coef[, 1] + coef[, 2] * qt(p = alpha/2, df = dendf)
    upper <- coef[, 1] + coef[, 2] * qt(p = 1 - alpha/2, df = dendf)
    coef <- cbind(coef, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    r.squared <- r.squared(obj)
    adj.r.squared <- r.squared(obj, dfcor=TRUE)
    F <- unname(smry$fstatistic[2]$statistic)
    p <- pf(F, numdf, dendf, lower.tail = FALSE)
    N <- length(obj$residuals)
    
    sumstat <- c(r.squared = r.squared, adj.r.squared = adj.r.squared, 
        F = F, numdf = numdf, dendf = dendf, p = p, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = obj$call)
}
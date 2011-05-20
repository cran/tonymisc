getSummary.coeftest = function (obj, alpha = 0.05, ...) 
{
    setTabDefault()
    coef <- obj
    lower <- coef[, 1] + coef[, 2] * qnorm(p = alpha/2)
    upper <- coef[, 1] + coef[, 2] * qnorm(p = 1 - alpha/2)
    coef <- cbind(coef, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
        
    sumstat <- NULL
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = obj$call)
}
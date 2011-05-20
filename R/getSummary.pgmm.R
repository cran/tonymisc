getSummary.pgmm = function (obj, alpha = 0.05, ...) 
{
    setTabDefault()
    smry <- summary(obj)
    coef <- smry$CoefTable
    lower <- coef[, 1] + coef[, 2] * qnorm(p = alpha/2)
    upper <- coef[, 1] + coef[, 2] * qnorm(p = 1 - alpha/2)
    coef <- cbind(coef, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    Wald.coef <- as.numeric(summary(obj)$wald.coef[1]$statistic)
    p.coef <- as.numeric(summary(obj)$wald.coef[2])
    p.ar1  <- as.numeric(mtest(obj,1)[2])
    p.ar2  <- as.numeric(mtest(obj,2)[2])
    p.sargan <- as.numeric(sargan(obj)[2])
    N <- length(obj$residuals)
    
    sumstat <- c(Wald.coef=Wald.coef, p.coef=p.coef, p.ar1=p.ar1, p.ar2=p.ar2, p.sargan=p.sargan, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = obj$call)
}
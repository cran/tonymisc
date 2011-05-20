getSummary.gls <- function (obj, alpha = 0.05, ...) 
{
    setTabDefault()
    smry <- summary(obj)
    coef <- smry$tTable
    confints <- intervals(obj,level=1-alpha)$coef[,c(1,3)]
    coef <- cbind(coef, confints)
       
    if(!is.null(intervals(obj)$corStruct)){
       corconfits <- matrix(intervals(obj)$corStruct[,c(2,2,2,2,1,3)],ncol=6)
       corconfits[,c(2:4)] <- NA
       rownames(corconfits) <- rownames(intervals(obj)$corStruct)
    
       coef <- rbind(coef,corconfits)
    }
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    
    sigma <- smry$sigma
    N <- length(smry$fitted)
    AIC <- summary(obj)$AIC
    BIC <- summary(obj)$BIC
    ll  <- obj$logLik
    
    resp   = all.vars(summary(obj)$call$model)[1]
    llnull = summary(obj)$logLik
    
    pseudo.r.squared <- 1 - (ll/llnull)
    sumstat <- c(sigma = sigma, AIC = AIC, BIC = BIC, ll=ll, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = obj$call)
}
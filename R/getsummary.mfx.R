getSummary.mfx = function (obj, alpha = 0.05, ...) 
{
    cat("Caveat: Standard Errors are Approximate \n")
    cat("They are based on an assumption that the \n")
    cat("z-score does not change under the marginal \n")
    cat("effects transformation. \n\n")
    class(obj) = "glm"
    smry <- summary(obj)
    N <- length(obj$residuals)
    coef <- smry$coef
    means <- colMeans(model.matrix(obj))
    M3    <- means%*%coef[,1]
    
    if(obj$family$link=="probit")
    { 
      mfx    <-dnorm(M3)*coef[,1]
    }
    if(obj$family$link=="logit")
    {
      mfx    <-  dlogis(M3)*coef[,1]
    }
    Znew   <- coef[,1]/coef[,2]   ## Old and New Z are equal (approx.)
    mfx_se <- mfx/Znew
    
    coef[,1] <- mfx
    coef[,2] <- mfx_se
        
    lower <- coef[, 1]-coef[, 2]*qnorm(p = alpha/2)
    upper <- coef[, 1]+coef[, 2]*qnorm(p = 1 - alpha/2)
    coef <- cbind(coef, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    phi <- smry$dispersion
    LR <- smry$null.deviance - smry$deviance
    df <- smry$df.null - smry$df.residual
    ll <- logLik(obj)
    deviance <- deviance(obj)
    if (df > 0) {
        p <- pchisq(LR, df, lower.tail = FALSE)
        L0.pwr <- exp(-smry$null.deviance/N)
        McFadden <- 1 - smry$deviance/smry$null.deviance
        Cox.Snell <- 1 - exp(-LR/N)
        Nagelkerke <- Cox.Snell/(1 - L0.pwr)
    }
    else {
        LR <- NA
        df <- NA
        p <- NA
        McFadden <- NA
        Cox.Snell <- NA
        Nagelkerke <- NA
    }
    AIC <- AIC(obj)
    BIC <- AIC(obj, k = log(N))
    sumstat <- c(phi = phi, LR = LR, df = df, p = p, logLik = ll, 
        deviance = deviance, McFadden = McFadden, Cox.Snell = Cox.Snell, 
        Nagelkerke = Nagelkerke, AIC = AIC, BIC = BIC, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = obj$call)
}
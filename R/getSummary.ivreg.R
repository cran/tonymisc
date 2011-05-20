getSummary.ivreg <-
function (obj, alpha = 0.05, ...) 
{
    setTabDefault()
    smry <- summary(obj)
    coef <- smry$coef
    numdf <- unname(smry$df[3])
    resdf <- unname(smry$df[2])
    lower <- coef[, 1] + coef[, 2] * qt(p = alpha/2, df = resdf)
    upper <- coef[, 1] + coef[, 2] * qt(p = 1 - alpha/2, df = resdf)
    coef <- cbind(coef, lower, upper)
    colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")
    sigma <- smry$sigma
    r.squared <- smry$r.squared
    adj.r.squared <- smry$adj.r.squared
    Wald <- unname(smry$waldtest[1])
    
    sec   = model.matrix(obj, "regressors")
    fir   = model.matrix(obj, "instruments")
    res   = residuals(obj)
    
    dat   = data.frame(res,sec,fir)
    
    explan   = colnames(sec)[-1]
    instr    = colnames(fir)[-1]
    
    exog   = intersect(instr,explan) 
    endog  = setdiff(explan,exog)
    exclud = setdiff(instr,exog)
    
    if(length(endog)==1)
    {
      first.form = reformulate(instr, response=endog) 
      first.lm   = lm(first.form, dat)
      ftest      = linearHypothesis(first.lm, exclud, rep(0,length(exclud)))
      firstF     = ftest[2,5]
      probF      = ftest[2,6]
    }
    else
    {
      firstF = NA
      probF  = NA
    }
    if(length(instr)>1)
    {
        J.form = reformulate(instr, response="res")
        J.lm = lm(J.form, data=dat)

        f.test = linearHypothesis(J.lm,exclud, rep(0,length(exclud)))
        Jstat = length(exclud)*f.test$F[2]
        probJ = 1-pchisq(Jstat, length(exclud)-1)
    }
    else
    {
      Jstat=NA
      probJ=NA
    }

        
    p <- unname(smry$waldtest[2])
    N <- sum(smry$df[1:2])
    sumstat <- c(sigma = sigma, r.squared = r.squared, adj.r.squared = adj.r.squared, 
        Wald = Wald, resdf = resdf, firstF=firstF, probF=probF,Jstat = Jstat, probJ =probJ, p = p, N = N)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = obj$call)
}


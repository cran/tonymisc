getSummary.robust= function (obj, alpha = 0.05, ...) 
{
   setTabDefault()
   allnames = names(obj$coefficients)
   keep = obj$keep
   drop = obj$drop
   keepnames = c("(Intercept)",keep)
   dropint=0
   if(!is.null(drop)){
      dropint   = sum(ifelse(drop=="(Intercept)", 1,0))
      drop      = ifelse(dropint>0, setdiff(drop, "(Intercept)"), drop)
   }
   
   if(is.null(keep)){
     keepnames = setdiff(allnames, drop)
   }
   if(dropint>0){
     keepnames = setdiff(keepnames, "(Intercept)")
   }
   
   
   if(length(keepnames)==1){
     RHS = ifelse(keepnames=="(Intercept)", "1", paste(c(keepnames,"0"),collapse="+") )
   }
   if(length(keepnames)>1){
     RHS = ifelse(keepnames[1]=="(Intercept)", paste(keepnames[-1],collapse="+"), paste(c("0",keepnames),collapse="+"))
   }

   resp  = tony.vars(formula(obj$call))[1]
   
   callio= call("lm",resp~RHS,data=obj$call$data)
   class(obj) = "lm"
    smry <- summaryR(obj, type = obj$type)
    coef <- smry$coef
    coef <- matrix(coef[keepnames,],ncol = 4)
    rownames(coef)=keepnames
    
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
    p <- pf(F, numdf, dendf, lower.tail = FALSE)
    N <- sum(smry$df[1:2])
    type <- ifelse(obj$type=="no", 9,as.numeric(substring(obj$type, 3, 3)))
    sumstat <- c(sigma = sigma, r.squared = r.squared, adj.r.squared = adj.r.squared, 
        F = F, numdf = numdf, dendf = dendf, p = p, N = N, type = type)
    list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
        xlevels = obj$xlevels, call = callio)
}
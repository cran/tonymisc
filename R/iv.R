iv = function (second, first, data) 
{
    s.names = tony.vars2(second)$vars
    f.names = tony.vars2(first)$vars
    n_endog = tony.vars2(first)$lhs
    N = length(data[,1])
        
    all.names = c(s.names, f.names)
    resp = s.names[1]
    sel_endog = 1:n_endog
    endog = f.names[sel_endog]
    inst = f.names[-sel_endog]
    explan = s.names[-1]
    exog = setdiff(explan,endog)
    exog.f = ifelse(length(exog)==0,1,paste(exog, collapse = "+"))
    inst.f = paste(inst, collapse = "+")
    RHS = paste(exog.f, inst.f, sep = "+")
    num_endog = length(endog)
    if(num_endog==1){
       first.form = as.formula(paste(endog, "~", RHS))
       first.lm = lm(first.form, data)
       first.form.red = as.formula(paste(endog, "~", exog.f))
       first.red = lm(first.form.red, data)
       ftest = anova(first.red, first.lm)
       x.hat = fitted(first.lm)
       data2 = cbind(data, x.hat)
       iname = paste(endog, ".hat", sep = "")
       names(data2) = c(names(data), iname)
       num_inst = ftest[2,3]
    }
    if(num_endog>1){
      data2 = data
      inamef = rep("ahhhh", num_endog)
      ftest  = list(rep("0", num_endog))
      for(i in 1:num_endog){
         first.form = as.formula(paste(endog[i], "~", RHS))
         first.lm = lm(first.form, data)
         first.form.red = as.formula(paste(endog[i], "~", exog.f))
         first.red = lm(first.form.red, data)
         ftestr = anova(first.red, first.lm)
         ftest[[i]] = ftestr
         num_inst = ftestr[2,3]
         x.hat = fitted(first.lm)
         data2 = cbind(data2, x.hat)
         inamed = paste(endog[i], ".hat", sep = "")
         inamef[i] = inamed
       }
       names(data2) = c(names(data),inamef)
       iname = paste(inamef, collapse = "+")
       endog = paste(endog, collapse = "+")
    }
    
    RHS2 = paste(exog.f, iname, sep = "+")
    second.form = as.formula(paste(resp, "~", RHS2))
    second.lm = lm(second.form, data2)
    RHS3 = paste(exog.f, endog, sep = "+")
    uninst.form = as.formula(paste(resp, "~", RHS3))
    uninst.lm = lm(uninst.form, data)
    X = model.matrix(uninst.lm)
    X2 = model.matrix(second.lm)
    
    Tsize = length(data[, 1])
    cc = cancor(as.matrix(X),as.matrix(X2))$cor
    mincc = min(cc)
    mincc_test = ((num_endog+num_inst)/2 - Tsize)*sum(log((1-(mincc)^2)))
    mincc_df   = num_inst - num_endog +1
    mincc_pval = 1-pchisq(mincc_test, mincc_df)
    
    CC_test = cbind(mincc_test, mincc_df, mincc_pval)
    
    z = summary(second.lm)
    Y = as.vector(data[resp])
    fit = as.vector(X %*% second.lm$coefficients)
    res = Y - fit
    
    ## Tests for overidentifying restrictions ##
    data3 = cbind(data2, res)
    names(data3) = c(names(data2), "res")

    ## Sargan's Test
    S.test     = as.data.frame(matrix(c(NA,NA ),nrow=1))
    names(S.test) = c("S.stat","P[S > S.stat ]")

    if(num_inst>num_endog){
        S.form = as.formula(paste("res", "~", RHS))
        S.lm = lm(S.form, data3)
        S.stat = N*summary(S.lm)$r.squared
        S.test[1,1] = S.stat
        S.test[1,2] = 1-pchisq(S.stat, length(inst)-1)
    }

    
    
    SSE = sum(res^2)
    SST = sum((Y - colMeans(Y))^2)
    R2 = (SST - SSE)/SST
    z$r.squared = R2
    xPx = t(X2) %*% X2
    xPx.inv = solve(xPx)
    z$cov.unscaled = xPx.inv
    z$residuals = res
    z$sigma = sqrt(colMeans(res^2))
    varcovmat = z$cov.unscaled * z$sigma
    coef = z$coefficients[, 1]
    IV.SE = t(z$sigma * sqrt(diag(xPx.inv)))
    t.iv = t(coef)/IV.SE
    p.val = 2 * (1 - pnorm(abs(t.iv)))
    z$coefficients[, 2] = IV.SE
    z$coefficients[, 3] = t.iv
    z$coefficients[, 4] = p.val
    result = list(summary(first.lm), z, ftest, coef, cc, num_endog, num_inst, CC_test, res, fit, S.test)
    names(result) = c("first", "second", "ftest", "coefficients", "cc", "endog", "inst", "CC_Test", "residuals", "fitted.values", "Sargan")
    class(result) = c("tonyiv")
    return(invisible(result))
}



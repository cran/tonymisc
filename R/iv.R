iv <-
function(second, first, data){
   s.names = tony.vars(second)
   f.names = tony.vars(first)
   N = length(data[,1])
   all.names = c(s.names,f.names)
   resp = s.names[1]
   endog = f.names[1]
   inst = f.names[-1]
   explan = s.names[-1]
   exog = explan[explan!=endog]
   exog.f = paste(exog,collapse="+")
   inst.f = paste(inst, collapse="+")
   RHS = paste(exog.f, inst.f, sep="+")
   
   cat("Exogenous: ", exog, "\n")
   cat("Endogenous: ", endog, "\n")
   cat("Instruments: ", inst, "\n")
   cat("Response: ", resp, "\n")
   
   first.form = as.formula( paste(endog, "~", RHS))
   first.lm = lm(first.form, data)

   first.form.red = as.formula( paste(endog, "~", exog.f))
   first.red = lm(first.form.red, data)
   
   ftest = anova(first.red, first.lm)

   cat("\n", "First Formula: ", "\n")
   print(first.form)

   x.hat = fitted(first.lm)
   data2 = cbind(data,x.hat)
   iname = paste(endog,".hat",sep="")
   names(data2) = c(names(data), iname)

   RHS2 = paste(exog.f,iname,sep="+")

   second.form = as.formula(paste(resp, "~", RHS2))
   second.lm = lm(second.form, data2)




   cat("\n", "Second Formula: ", "\n")
   print(second.form)

   RHS3 = paste(exog.f,endog,sep="+")
   uninst.form = as.formula(paste(resp, "~", RHS3))
   uninst.lm = lm(uninst.form, data)


   cat("\n", "Uninstrumented Formula: ", "\n")
   print(uninst.form)

   X  = model.matrix(uninst.lm)
   X2 = model.matrix(second.lm)

   z = summary(second.lm)

  # X = as.matrix(cbind(1,Xmat))
  # X2 = as.matrix(cbind(1,Xmat2))

   Y = data[resp]
   
   fit = X%*%second.lm$coefficients
   res = Y - fit
   
   SSE = sum(res^2)
   SST = sum((Y - mean(Y))^2)
   R2  = (SST-SSE)/SST
   z$r.squared = R2

   xPx = t(X2)%*%X2
   xPx.inv = solve(xPx)
   z$cov.unscaled = xPx.inv
   z$residuals = res
   z$sigma = sqrt(mean(res^2))
   varcovmat = z$cov.unscaled*z$sigma
   coef = z$coefficients[,1]
   IV.SE = z$sigma*sqrt(diag(xPx.inv))
   t.iv = coef/IV.SE
   p.val = 2*(1-pnorm(abs(t.iv)))

   z$coefficients[,2] = IV.SE
   z$coefficients[,3] = t.iv
   z$coefficients[,4] = p.val

   result = list(summary(first.lm),z,ftest)
   class(result) = c("tonyiv")
   names(result) = c("first", "second","ftest")

cat("\n \n","IV object successfully created. Use mtable() on object", "\n", "to learn about your 2SLS Regression")
cat("\n", "**mtable() is available in the memisc library. Also need the extensions in the tonymisc library.")

return(invisible(result))

}


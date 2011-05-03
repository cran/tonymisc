setGLSDefault = function(){
    temp.gls = c("($sigma:f#)", "($AIC:f#)","($BIC:f#)","($ll:f#)", "($N:d)")
    names(temp.gls) = c("sigma", "AIC","BIC","Log-Likelihood" , "N")
    setSummaryTemplate(gls=temp.gls)

}
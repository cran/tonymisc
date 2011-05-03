setTabDefault = function(){
  ## Sets GLS Default Reporting      ##
    temp.gls = c("($sigma:f#)", "($AIC:f#)","($BIC:f#)","($ll:f#)", "($N:d)")
    names(temp.gls) = c("sigma", "AIC","BIC","Log-likelihood" , "N")
    setSummaryTemplate(gls=temp.gls)
    
  ## Sets "Robust" Default Reporting ##
  ## Sets lm() default reporting too ##
    temp.rob = c("($r.squared:f)", "($adj.r.squared:f)", "($sigma:f)", "($F:f)", 
                  "($p:f)", "($N:d)", "hc($type:d)")
    names(temp.rob) = c("R-squared", "adj. R-squared", "sigma", "F (omnibus)", "p-val (omnibus)", "N","type")
    setSummaryTemplate(robust=temp.rob)
    
temp.lm = c("($r.squared:f)", "($adj.r.squared:f)", "($sigma:f)", "($F:f)", 
                  "($p:f)", "($N:d)")
    names(temp.lm) = c("R-squared", "adj. R-squared", "sigma", "F (omnibus)", "p-val (omnibus)", "N")
    setSummaryTemplate(lm=temp.lm)
    
  ## Sets IV Regression Default Reporting ##
    temp = c("($r.squared:f#)", "($Wald:f#)", "($p:#)", "($firstF:f#)", "($probF:#)", 
             "($Jstat:f#)", "($probJ:#)", "($N:d)")
    names(temp) = c("R-squared", "Wald Stat (2nd Stage)", "p-value (Wald)", 
                    "F-stat (1st Stage, relevance)", "p-value relevance", "J-Test Stat", "p-value (J-Test)", "N")
    setSummaryTemplate(ivreg=temp)

    
  ## Sets IV Regression Default Reporting from TonyIV ##
    temp = c("($r.squared:f#)", "($Ffirst:f#)", "($pfirst:#)", "($F:f#)", "($p:#)", 
             "($N:d)")
    names(temp) = c("R-squared","F (relevance)", "p-val (relevance)", "F (omnibus)", "p-val (omnibus)", 
                     "N")
    setSummaryTemplate(tonyiv=temp)
  
}
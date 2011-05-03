setRobustDefault <-
function(){
    temp.rob = c("($r.squared:f)", "($adj.r.squared:f)", "($sigma:f)", "($F:f)", 
                  "($p:f)", "($N:d)", "hc($type:d)")
    names(temp.rob) = c("R-squared", "adj. R-squared", "sigma", "F", "p", "N","type")
    setSummaryTemplate(robust=temp.rob)

    temp.lm = temp.rob[1:6]
    setSummaryTemplate(lm =temp.lm)
}


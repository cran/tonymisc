setIVDefault <-
function(){
   temp = c("($r.squared:f#)", "($Wald:f#)", "($p:#)", "($firstF:f#)", "($probF:#)", "($Jstat:f#)", "($probJ:#)", "($N:d)")
   names(temp) = c("R-squared", "Wald Stat (2nd Stage)", "p-value (Wald)", "F-stat (1st Stage, relevance)", "p-value relevance", "J-Test Stat", "p-value (J-Test)", "N")
   setSummaryTemplate(ivreg=temp)
}


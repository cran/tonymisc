summaryR <-
function(model, type=c("hc3","hc2","hc1","hc0", "hc4"), ...){

   if (!require(car)) stop("Required car package is missing.")
   if(type!="no"){
      type <- match.arg(type)
      V <- hccm(model, type=type)
      sumry <- summary(model)
      table <- coef(sumry)
      table[,2] <- sqrt(diag(V))
      table[,3] <- table[,1]/table[,2]
      table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
      
      sumry$coefficients <- table
      p <- nrow(table)
      hyp <- cbind(0, diag(p - 1))
      sumry$fstatistic[1] <- linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
      sumry$type <- type
   }
   if(type=="no"){ 
     sumry = summary(model)
   }

return(sumry)
   
   
   }


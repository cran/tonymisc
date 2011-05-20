mfx<-function(x){  
  result <- summary(x)
  result$fitted.values <- x$fitted.values
  result$residuals <- x$residuals

  coefs <-data.frame(x$coefficients)
  K    <- length(coefs)
  varmeans<-colMeans(model.matrix(x))
	varnames<-names(varmeans)
	#   
  M3<-varmeans%*%as.matrix(coefs)
	#
  coefs <- as.data.frame(coefs)

	if (ll<-x$family$link=="probit"){
		probitmfx <- data.frame(mfx=dnorm(M3)*coefs)
	}
		else{probitmfx <- data.frame(mfx=dlogis(M3)*coefs)}
	#
  zorig <- result$coef[,3]
  
	bbse<-coefs/zorig
	mfxse<-probitmfx/bbse
	#
  coef.table <-data.frame(mfx=probitmfx,SE=mfxse,result$coef[,3],result$coef[,4])
	dimnames(coef.table) <- list(varnames, c("mfx","SE","z","Pr(>|z|)"))
  
  result$logl <- 0.5*(-x$aic + 2*K)
  
  result$coefficients <- coef.table
  
  datem <- model.frame(x)
  names(datem)[1] = "resp"
  
  datem <- model.frame(x)
  names(datem)[1] = "resp"
    
	depenglm <- glm(resp~1, family=binomial(link=x$family$link),data=datem)
	logldepen <- 0.5*(-depenglm$aic + 2)
	result$psr2<- 1 - (result$logl/logldepen)
	#
  result$link <- x$family$link
	result$obs <-nrow(datem)
	#SBC/BIC
	result$sbc<- -2*result$logl + log(result$obs)*K
	#HQIC
	result$HIC<- -2*result$logl + 2*log(log(result$obs))*K
	#CDF of Mean Model
		if (ll == TRUE){
		BigProb<-pnorm(M3)
		}
		else {BigProb<-plogis(M3)}
	
  result$CDF = BigProb
  #LR Test
	LRTest<- 2*(result$logl - logldepen)
	dfLR<-K-1
	LRp<-dchisq(LRTest,df=dfLR)
	LRdata <- data.frame(LRTest, dfLR,LRp)
	colnames(LRdata) <- c("Test Statistic","DF","P-Value")

	rownames(LRdata) <- "LR Test"
	result$LRTest <- LRdata
  class(result) <- c("mfx")
  return(result)
  
}
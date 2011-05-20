sum_iv <-
function(reg_iv, first=FALSE, ftest=FALSE,
      second=TRUE, cc = FALSE) {

x= rep(0,10)
if(first==TRUE) x[1] = 1
if(second==TRUE) x[2]= 2
if(ftest==TRUE) x[3]= 3
if(cc   ==TRUE) x[8]= 8
print(reg_iv[x])

}
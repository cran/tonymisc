sum_iv <-
function(reg_iv, first=FALSE, ftest=FALSE,
      second=TRUE, cc = FALSE, sargan = FALSE) {

x= rep(0,11)
if(first==TRUE) x[1] = 1
if(second==TRUE) x[2]= 2
if(ftest==TRUE) x[3]= 3
if(cc   ==TRUE) x[8]= 8
if(sargan == TRUE) x[11] = 11
print(reg_iv[x])

}
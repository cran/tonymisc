tony.vars <-
function(x){
  rhs    = as.character(x)[3]
  rhs.sp = strsplit(rhs," \\+ ")[[1]]
  resp   = as.character(x)[2]
  
  c(resp, rhs.sp)
}


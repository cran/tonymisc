tony.vars2 = function (x) 
{
    rhs = as.character(x)[3]
    rhs.sp = strsplit(rhs, " \\+ ")[[1]]
    resp = as.character(x)[2]
    resp.sp = strsplit(resp, " \\+ ")[[1]]
    result=list(c(resp.sp, rhs.sp),length(resp.sp))
    names(result) = c("vars", "lhs")
    return(result)
}
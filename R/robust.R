robust= function (model, type = "hc3" ,keep=NULL,drop=NULL,vcov=NULL, se=NULL) 
{
    mod = model
    class(mod) = c("robust")
    mod$type = type
    mod$keep = keep
    mod$drop = drop
    mod$uvcov = vcov
    mod$use   = se
    return(invisible(mod))
}
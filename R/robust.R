robust= function (model, type = "hc3" ,keep=NULL,drop=NULL) 
{
    mod = model
    class(mod) = c("robust")
    mod$type = type
    mod$keep = keep
    mod$drop = drop
    return(invisible(mod))
}
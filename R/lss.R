lss <-
function (graph = FALSE, env = .GlobalEnv, ...) 
{
    enam <- deparse(substitute(env))
    nam <- ls(envir = env, ...)
    cls <- sapply(nam, function(x) class(get(x, envir = env)))
    cls <- sapply(cls, paste, collapse = ", ")
    s <- sapply(ls(envir = env), function(x) object.size(get(x, 
        envir = env)))
    rval <- data.frame(class = cls, size = s)
    if (graph) 
        dotchart(sort(s), main = paste("Objects in environment:", 
            enam), xlab = "Size [bytes]")
    return(rval)
}

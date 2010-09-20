# These two functions override the two originals found in the 'base' package
# and put the working directory string in the name of the window containing the
# R console

setwd <- function(dir)
{
    if(.Platform$OS.type == "windows" & .Platform$GUI == "Rgui")
    {
        r <- try(.Internal(setwd(dir)))
        # Add the working directory to the window title
        if( !inherits(r, "try-error") )
        {
        utils:::setWindowTitle( paste("[", getwd(), "]", sep="") )
        }
    } else
    {
        base::setwd(dir)
    }
}



getwd <- function()
{
    if(.Platform$OS.type == "windows" & .Platform$GUI == "Rgui")
    {
        rval <- .Internal(getwd())
        # Add the working directory to the window title
        if( !inherits(rval, "try-error") )
        {
        utils:::setWindowTitle( paste("[", rval, "]", sep="") )
        }
        rval
    } else
    {
        base::getwd()
    }
}


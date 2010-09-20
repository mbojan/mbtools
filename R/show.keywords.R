show.keywords <- function( file=NULL )
{
    if( is.null(file) )
    {
        rhome <- Sys.getenv("R_HOME")
        file <- file.path( rhome, "doc", "KEYWORDS" )
    }
    file.show( file, header="Keywords for R documentation")
}


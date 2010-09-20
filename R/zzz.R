.onLoad <- function(libname, pkgname)
{
    addMbtoolsMenus()
}

.onUnload <- function(libpath)
{
    removeMbtoolsMenus()
}

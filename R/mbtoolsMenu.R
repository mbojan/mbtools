# add menus for bojan package
addMbtoolsMenus <- function()
{
    # only for Windows
    if(.Platform$OS.type == "windows" & .Platform$GUI == "Rgui")
    {	
	winMenuAdd("mbtools")
	winMenuAddItem("mbtools", "Show keywords", "show.keywords()")
    }
}

removeMbtoolsMenus <- function()
{
    # only for Windows
    if(.Platform$OS.type == "windows" & .Platform$GUI == "Rgui")
    {	
	winMenuDel("mbtools")
    }
}
    

# Function to return version number
#
#
gmr.version <- function()
{
    # return("Version: 1.00\nCompile date: 2018-09-02\n")
    return(paste0("Version: ",packageVersion("gmr"),"\nCompile date: ",Sys.Date(),"\n", sep=""))
}

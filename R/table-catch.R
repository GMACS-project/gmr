#' Create a table of catches
#'
#' @param M List object(s) created by read_admb function
#' @return kable of catches per year and fleet
#' @author SJD Martell, DN Webber
#' @export
#'
table_catch<-function(M)
{
#==get catch
mdf <- .get_catch_df(M)

#==massage catch
casted = reshape2::dcast( mdf , year~fleet )

#==table catch
knitr::kable(casted)
}



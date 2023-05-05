#' @title DSS_Con
#' @description Establish DSS Connection
#' @import RDCOMClient
#' @import purrr
#' @examples
#' \dontrun{
#' RDSS::DSS_Con()
#' }
#' @export

DSS_Con <- function(){
  if(!require("RDCOMClient")) install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
  safecon <- purrr::safely(COMCreate)
  dssClient <- safecon("SIM.ServerInterface.ClientApi")

  if(!is.null(dssClient$error)){
  if( !any(grepl("Rscript.exe", list.files(paste0( "C:/Program Files/R/R", "-", R.version$major, ".", R.version$minor, "/bin/x64/")))) ) stop(paste0( "Seems like you need to add the Rscript.exe files into the Rstudio bin folder. Please check and add here: ", paste0( "C:/Program Files/R/R", "-", R.version$major, ".", R.version$minor, "/bin/x64/")))
  if( !any(grepl( "rsession.exe", list.files("C:/Program Files/RStudio/bin/") ))) stop("Seems like you need to add the rsession.exe files into the Rstudio bin folder. Please check. Please check and add here: C:/Program Files/RStudio/bin")
  stop("RDComclient connection issue.")
  }

  dssClient$result

}

#' @title DSS_Con
#' @description Establish DSS Connection
#' @import RDCOMClient
#' @examples
#' \dontrun{
#' RDSS::DSS_Con()
#' }
#' @export

DSS_Con <- function(){
  if(!require("RDCOMClient")) install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
  dssClient <- COMCreate("SIM.ServerInterface.ClientApi")
  dssClient
}

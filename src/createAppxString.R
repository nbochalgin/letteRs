#' Create appendix info
#'
#' Creates appendix string according to recipient and sequencing platform
#' 
#' @param region_key ISO-key of the reported region
#' @param if_frag TRUE/FALSE
#'
#' @return A character vector with platform info
#' @export
#'
#' @examples
#' createAppxString(BU, if_frag = TRUE)

createAppxString <- function(region_key, if_frag = FALSE) {
    if (region_key == "UENFS") {
        appendix <- " "
    } else {
        if (if_frag) {
            appendix <- "Приложение: Результаты фрагментного секвенирования… – на стр."
        } else {
            appendix <- "Приложение: Результаты полногеномного секвенирования… – на стр."
        }
    }
    return(appendix)
}
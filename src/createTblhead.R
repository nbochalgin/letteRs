#' Appendix table heading
#'
#' @param if_frag TRUE/FALSE. A flag to mark sequencing method. FALSE for WGS.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' createTblhead(if_frag = FALSE)


createTblhead <- function(if_frag) {
    if (if_frag) {
        tblhead <- "Результаты фрагментного секвенирования образцов биологического материала от больных COVID-19"
    } else {
        tblhead <- "Результаты полногеномного секвенирования образцов биологического материала от больных COVID-19"
    }
    return(tblhead)
}
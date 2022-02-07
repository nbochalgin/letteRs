#' Add empty rows to docx
#' 
#' Add n blank paragraphs into an rdocx object
#'
#' @param doc A docx device
#' @param n A number of paragraphs to add
#'
#' @return
#' @export
#'
#' @examples
#' body_add_par_n(doc, 3)

body_add_par_n <- function(doc, n) {
    i <- 1
    repeat {
        doc <- body_add_par(doc, "", style = "Normal")
        i <- i+1
        if (i > n) {break}
    }
    return(doc)
}
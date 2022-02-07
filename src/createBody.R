#' Letter body creation
#'
#' Create letter body with appropriate variants and regions in place
#' 
#' @param region_key ISO-key of the region to form body for
#' @param i_dict ISO dictionary
#' 
#' @return A character
#' @export
#'
#' @examples
#' createBody(IRK, iso_dict = iso_dict)

createBody <- function(region_key, i_dict = iso_dict) {
    region <- i_dict$region[i_dict$key == region_key]
    
    body_blank <- filter(body_text, stri_detect_fixed(key, region_key)) %>% 
        pull(body) %>% 
        .[[1]] %>% 
        as.list()
    
    body <- list(
        paste(unique(full_table$variant), collapse = ", "),
        paste(i_dict$rp[i_dict$region == unique(full_table$region)], collapse = ", "),
        NA) %>% 
        Map(c, body_blank, .) %>% 
        unlist() %>% 
        .[!is.na(.)] %>% 
        paste(collapse = "")
    
    return(body)
}
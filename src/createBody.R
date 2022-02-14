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

createBody <- function(region_key, iso_dict, raw_data, body_template) {
    if (region_key %in% c("CNIIE", "UENFS")) {
        regs <- paste(iso_dict$rp[iso_dict$region %in% unique(raw_data$region)],
                      collapse = ", ")
        vars <- paste(unique(raw_data$variant), collapse = ", ")
    } else {
        regs <- paste(iso_dict$rp[iso_dict$key == region_key],
                      collapse = ", ")
        vars <- paste(
            unique(
                raw_data$variant[raw_data$region == iso_dict$region[iso_dict$key == region_key]]
            ),
            collapse = ", ")
    }
        
    body <- Map(c,
                as.list(body_template$body[body_template$key == region_key][[1]]),
                list(vars, regs, NA)) %>% 
        unlist() %>% 
        na.omit() %>% 
        paste(collapse = "")
    
    return(body)
}
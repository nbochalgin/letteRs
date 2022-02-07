#' Standard nipchi letter heading generation
#' 
#' Формирование заголовочной таблицы стандартного бланка на конкретного получателя
#'
#' @param key Sender key. Must be equal to one of the values in recipient_info
#' @param nipchi_info A character vector with the legal details of the sending organization
#' @param recipient_info A table with recipients' information (key, greeting, organization, etc.)
#'
#' @return Table with standard heading info
#' @export
#'
#' @examples
#' createHeading(BU, nipchi_info = sender_info, recipient_info = recipient_info)

createHeading <- function(key,
                          nipchi_info = sender_info,
                          rec_info = recipient_info) {
    heading <- tibble(nipchi_info,
                      rec_info[rec_info$key == key, 2]) %>%
        flextable() %>% 
        delete_part(part = "header") %>% 
        align(align = "center", part = "body") %>% 
        width(j = 1:2,
              width = c(4, 3),
              unit = "in") %>% 
        font(i = NULL, j = NULL,
             fontname = "Liberation Serif",
             part = "body") %>% 
        fontsize(i = 1, j = 1,
                 size = 10,
                 part = "body") %>%
        fontsize(i = 1, j = 2,
                 size = 13,
                 part = "body") %>%
        bold(i = 1, j = 2,
             bold = TRUE,
             part = "body") %>% 
        border_remove()
    return(heading)
}
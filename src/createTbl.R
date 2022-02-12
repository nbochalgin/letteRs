#' Title
#'
#' @param full_table 
#' @param region_key 
#' @param if_frag 
#'
#' @return
#' @export
#'
#' @examples
#' 

createTbl <- function (full_table, region_key, if_frag) {
    if ((region_key == "CNIIE")) {
        tbl <- full_table %>%
            select(" " = nipchi_id,
                   "Идентификационный номер образца (изолята)" = number,
                   "Субъект, направивший пробу" = region,
                   "Заключение о генетическом варианте SARS-CoV-2" = variant) %>% 
            flextable() %>% 
            align(align = "center", part = "all") %>% 
            width(j = 1:4,
                  width = c(.5, 2.25, 2, 2.3),
                  unit = "in") %>% 
            font(i = NULL, j = NULL,
                 fontname = "Liberation Serif",
                 part = "all") %>% 
            fontsize(i = NULL, j = NULL,
                     size = 12,
                     part = "all") %>% 
            border(i = NULL, j = NULL,
                   border = fp_border(),
                   part = "all")
    } else if (!(region_key %in% c("UENFS", "CNIIE")) && !if_frag) {
        tbl <- full_table %>% 
            filter(stri_detect_fixed(region, iso_dict$region[iso_dict$key == region_key])) %>% 
            select("Идентификационный номер образца (изолята)" = number,
                   "Субъект, направивший пробу" = region,
                   "Идентифицированная генетическая линия" = pango_lineage,
                   "Заключение о генетическом варианте SARS-CoV-2" = variant) %>% 
            flextable() %>% 
            align(align = "center", part = "all") %>% 
            width(j = 1:4,
                  width = c(1.9, 1.7, 1.75, 2.2),
                  unit = "in") %>% 
            font(i = NULL, j = NULL,
                 fontname = "Liberation Serif",
                 part = "all") %>% 
            fontsize(i = NULL, j = NULL,
                     size = 12,
                     part = "all") %>% 
            border(i = NULL, j = NULL,
                   border = fp_border(),
                   part = "all")
    } else if (!(region_key %in% c("UENFS", "CNIIE")) && if_frag) {
        tbl <- full_table %>% 
            filter(stri_detect_fixed(region, iso_dict$region[iso_dict$key == region_key])) %>% 
            select(" " = nipchi_id,
                   "Идентификационный номер образца (изолята)" = number,
                   "Субъект, направивший пробу" = region,
                   "Заключение о генетическом варианте SARS-CoV-2" = variant) %>% 
            flextable() %>% 
            align(align = "center", part = "all") %>% 
            width(j = 1:4,
                  width = c(.5, 1.9, 1.7, 2.2),
                  unit = "in") %>% 
            font(i = NULL, j = NULL,
                 fontname = "Liberation Serif",
                 part = "all") %>% 
            fontsize(i = NULL, j = NULL,
                     size = 12,
                     part = "all") %>% 
            border(i = NULL, j = NULL,
                   border = fp_border(),
                   part = "all")
    }
    return(tbl)
}
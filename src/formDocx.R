#' Docx assembly
#' 
#' Сборка, форматирование и сохранение документа под конкретного адресата.
#' 
#' @param full_table 
#' @param region_key 
#'
#' @return
#' @export
#'
#' @examples
#' 

formDocx <- function(full_table, region_key) {
    if_else(
        if_frag,
        docFileName <- glue("{report_date}_{recipient_info$rec[recipient_info$key == region_key]}_wgs.docx"),
        docFileName <- glue("{report_date}_{recipient_info$rec[recipient_info$key == region_key]}_frag.docx"))
    
    doc <- read_docx(here("templates/template.docx")) %>%
        body_add_flextable(createHeading(region_key)) %>% 
        body_add_par_n(2) %>%
        body_add_par(
            recipient_info$greeting[stri_detect_fixed(recipient_info$key, region_key)],
            style = "my_heading"
        ) %>% 
        body_add_par("", style = "Normal") %>% 
        body_add_par(createBody(region_key), style = "my_body") %>% 
        body_add_par("", style = "Normal") %>% 
        body_add_par(createAppxString(region_key, if_frag), style = "my_body") %>% 
        body_add_par_n(3) %>%
        body_add_par(
            glue("Директор института {paste(rep(' ', 74), collapse = '')} С.В. Балахонов"),
            style = "my_body"
        ) %>% 
        body_add_par_n(16) %>% 
        body_add_par("Сидорова Е.А. +7 (3952) 220-139", style = "my_exec")
    
    
    if(region_key != "UENFS") {
        doc <- doc %>% 
            body_add_break(pos = "after") %>% 
            body_add_par("Приложение", style = "my_appx") %>% 
            body_add_par(createTblhead(), style = "my_tblhead") %>%
            body_add_flextable(createTbl(full_table, region_key, if_frag))
    }
    
    print(doc, target = glue("./output/{report_date}/{docFileName}"))
}
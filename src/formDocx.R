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

formDocx <- function(full_table, region_key = region_key, recipient_info = rec_info) {
    docFileName <- if_else(
        if_frag,
        docFileName <- glue("{report_date}_{region_key}_{recipient_info$rec[recipient_info$key == region_key]}_frag.docx"),
        docFileName <- glue("{report_date}_{region_key}_{recipient_info$rec[recipient_info$key == region_key]}_wgs.docx"))
    
    doc <- read_docx(here("templates/template.docx")) %>%
        body_add_flextable(createHeading(key = region_key,
                                         nipchi_info = sender_info,
                                         rec_info = recipient_info)) %>% 
        body_add_par_n(2) %>%
        body_add_par(
            recipient_info$greeting[recipient_info$key == region_key],
            style = "my_heading"
        ) %>% 
        body_add_par("", style = "Normal") %>% 
        body_add_par(createBody(region_key = region_key,
                                iso_dict = iso_dict,
                                raw_data = full_table,
                                body_template = body_text),
                     style = "my_body") %>% 
        body_add_par("", style = "Normal") %>% 
        body_add_par(createAppxString(region_key,
                                      if_frag = if_frag),
                     style = "my_body") %>% 
        body_add_par_n(3) %>%
        body_add_par(
            glue("Директор института {paste(rep(' ', 74), collapse = '')} С.В. Балахонов"),
            style = "my_body"
        ) %>% 
        body_add_par_n(16) %>% 
        body_add_par("Шаракшанов М.Б. +7 (3952) 220-137", style = "my_exec")
    
    
    if(region_key != "UENFS") {
        doc <- doc %>% 
            body_add_break(pos = "after") %>% 
            body_add_par("Приложение", style = "my_appx") %>% 
            body_add_par(createTblhead(if_frag), style = "my_tblhead") %>%
            body_add_flextable(createTbl(full_table = full_table,
                                         region_key = region_key,
                                         if_frag = if_frag))
    }
    
    print(doc, target = glue("./output/{report_date}/{docFileName}"))
}
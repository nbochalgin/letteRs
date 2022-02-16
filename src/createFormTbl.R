
#' Title
#'
#' @param raw_data 
#' @param iso_dict 
#' @param region_key 
#'
#' @return
#' @export
#'
#' @examples
#' 

createFormTbl <- function(raw_data, reg) {
    out_table <- filter(raw_data,
                        region == reg) %>% 
        mutate("Социальный статус (род занятий / место работы)" = NA,
               "Выезжал за пределы РФ" = NA,
               "Дата возвращения/прибытия в РФ" = NA,
               "Город, район, населенный пункт (в случае выезда за перделы РФ)" = NA,
               .after = "age") %>% 
        mutate("ОРВИ" = if_else(stri_detect_fixed(clinic, "орви"), "Да", NA_character_),
               "ВП" = if_else(stri_detect_fixed(clinic, "вп"), "Да", NA_character_),
               "бессимптомно" = if_else(stri_detect_fixed(clinic, "бессимптомно"), "Да", NA_character_),
               "Госпитализация" = NA,
               .after = "disease_date",
               .keep = "unused") %>% 
        mutate("\"британский\"" = NA,
               "\"южноафриканский\"" = NA,
               "\"бразильский\"" = NA,
               "др. указать" = variant,
               .keep = "unused") %>% 
        mutate("Общее число контактных с заболевшим" = NA,
               "Из них выявлено лиц с COVID-19" = NA,
               "из них выявлены мутации SARS CoV-2" = NA,
               "Эпиданамнез" = anamnesis,
               .after = everything(),
               .keep = "unused") %>%
        rename(" " = "nipchi_id",
               "№" = "number",
               "ФИО" = "fio",
               "Субъект РФ" = "region",
               "Пол" = "sex",
               "Возраст" = "age",
               "Дата заболевания" = "disease_date",
               "лаборатория выявившая мутацию" = "lab",
               "Дата выявления мутации" = "date_end")
    
    header <- data.frame(matrix(ncol = 25, nrow = 0))
    colnames(header) <- c(colnames(out_table)[1:11],
                          rep("Клиническая форма заболевания", 3),
                          colnames(out_table)[15:17],
                          rep("вид мутации SARS CoV-2 (штамм)", 4),
                          colnames(out_table)[22:25])
    
    out_list <- list("form" = out_table,
                     "header" = header)
    
    return(out_list)
}
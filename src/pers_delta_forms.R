suppressPackageStartupMessages({
  library("dplyr")
  library("glue")
  library("magrittr")
  library("openxlsx")
  library("purrr")
  library("RPostgres")
  library("stringi")
})

region <- c("Республика Тыва",
            "Республика Бурятия",
            "Республика Хакасия",
            "Забайкальский край",
            "Алтайский край",
            "Иркутская область",
            "Красноярский край",
            "Республика Алтай")

########## ФУНКЦИИ ##########

#дёргать запрос из БД
SitGetQuery <- function(query) {
  con <- dbConnect(RPostgres::Postgres(),
                   dbname = 'frag_seq_db', 
                   host = '192.168.0.145',
                   port = 5432,
                   user = 'postgres',
                   password = Sys.getenv("frag_seq_db"))
  db_output <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(db_output)
}

#формировать текст запроса в БД на регион
CreateQuery <- function(region) {
  glue(
  "    SELECT income.nipchi_id AS nipchi_id
            , income.income_id AS number
            , income.fio AS fio
            , income.region AS region
            , income.sex AS sex
            , income.age AS age
            , income.last_disease_start_date AS disease_date
            , LOWER(income.clinic) AS clinic
            , 'ФКУЗ Иркутский НИПЧИ' AS lab
            , frag_results.variant AS variant
            , frag_results.date_end AS date_end
            , income.epid_anamnesis AS anamnesis
         FROM income_probes AS income
    LEFT JOIN frag_seq_results AS frag_results
           ON frag_results.nipchi_id = income.nipchi_id
   RIGHT JOIN wgs_results AS wgs
           ON wgs.nipchi_id = income.nipchi_id
        WHERE wgs.run_id = 'ch_ON_42'
          AND income.region = '{region}'
          AND frag_results.variant IN ('Delta', 'Omicron', 'Probable Omicron', 'Иной')
     ORDER BY nipchi_id")
}

#подготовка табличных данных
CreateForm <- function(region) {
  out_table <- SitGetQuery(CreateQuery(region)) %>% 
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

#упаковка и форматирование xlsx
PackXLSX <- function(region) {
  wb <- createWorkbook()
  sh <- addWorksheet(wb, "Таблица  1 (персонифиц)")
  
  writeData(wb, sh, "Персонинифицированный учет случаев выявления мутаций SARS CoV-2",
            startCol = 2, startRow = 1)
  writeData(wb, sh, total_forms[[region]]$header,
            startCol = 1, startRow = 2)
  writeData(wb, sh, total_forms[[region]]$form,
            startCol = 1, startRow = 3)
  
  vert_merge <- c(1:11, 15:17, 22:25)
  horiz_merge <- list(c(12:14), c(18:21))
  
  mergeCells(wb, sh, cols = 2:25, rows = 1)
  map(horiz_merge, function(x) mergeCells(wb, sh, cols = x, rows = 2))
  map(vert_merge, function(x) mergeCells(wb, sh, cols = x, rows = 2:3))
  
  headerStyle <- createStyle(
    fontName = "Carlito",
    fontSize = 12,
    textDecoration = "bold",
    halign = "center",
    wrapText = TRUE,
    border = "TopBottomLeftRight",
    borderColour = c("black", "black", "black", "black"),
    borderStyle = "thin",
    indent = 1
  )
  
  bodyStyle <- createStyle(
    fontName = "Carlito",
    fontSize = 12,
    halign = "center",
    border = "TopBottomLeftRight",
    borderColour = c("black", "black", "black", "black"),
    borderStyle = "thin"
  )
  
  dateStyle <- createStyle(
    numFmt = "DD.MM.YYYY"
  )
  
  addStyle(wb, sh, headerStyle, rows = 1:3, cols = 1:25, gridExpand = TRUE)
  addStyle(
    wb, sh, bodyStyle,
    rows = 4:(max(unlist(rlist::list.map(total_forms[[region]], length(.$`ФИО`))))+3),
    cols = 1:25,
    gridExpand = TRUE
  )
  
  addStyle(
    wb, sh, dateStyle,
    rows = 4:(max(unlist(rlist::list.map(total_forms[[region]], length(.$`ФИО`))))+3),
    cols = c(11, 17),
    gridExpand = TRUE,
    stack = TRUE
  )
  
  setColWidths(wb, sh, cols = 1:25, widths = "auto", ignoreMergedCells = TRUE)
  
  dir.create(glue("output/{Sys.Date()}"), showWarnings = FALSE)
  
  saveWorkbook(
    wb,
    glue("output/{Sys.Date()}/ФОРМА_перс_учета_Дельта_вар_{Sys.Date()}_{region}.xlsx"),
    overwrite = TRUE)
}

########## MAIN FLOW ##########

total_forms <- map(region, CreateForm)
names(total_forms) <- region

total_forms <- rlist::list.filter(total_forms, length(form$ФИО) > 0)

map(names(total_forms), PackXLSX)
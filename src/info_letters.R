suppressPackageStartupMessages({
  library("glue")
  library("here")
  library("readr")
  library("magrittr")
  library("dplyr")
  library("stringi")
  library("officer")
  library("flextable")
  library("RPostgres")
})

source("src/CreateQuery.R")
source("src/db_connector.R")

if_frag <- FALSE
# report_date <- as.Date('2022-01-26')
report_date <- Sys.Date()

########## ФУНКЦИИ ##########

#добавлять много абзацев
body_add_par_n <- function(doc, n) {
  i <- 1
  while (i <= n) {
    doc <- body_add_par(doc, "", style = "Normal")
    i <- i+1
  }
  return(doc)
}

#создание шапки письма по форме
createHeading <- function(key) {
  heading <- tibble(sender_info,
                    recipient_info[recipient_info$key == key, 2]) %>%
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

#создание тела письма
createBody <- function(region_key) {
  region <- iso_dict$region[iso_dict$key == region_key]
  
  body_blank <- filter(body_text, stri_detect_fixed(key, region_key)) %>% 
    pull(body) %>% 
    .[[1]] %>% 
    as.list()
  
  body <- list(
    paste(unique(full_table$variant), collapse = ", "),
    paste(iso_dict$rp[iso_dict$region == unique(full_table$region)], collapse = ", "),
    NA) %>% 
    Map(c, body_blank, .) %>% 
    unlist() %>% 
    .[!is.na(.)] %>% 
    paste(collapse = "")
  
  return(body)
}

#создание боращения
createGreeting <- function(region_key) {
  greeting <- recipient_info$greeting[stri_detect_fixed(recipient_info$key, region_key)]
  return(greeting)
}

#создание строки о приложении
createAppx <- function(region_key) {
  if (region_key == "UENFS") {
    appendix <- " "
  } else {
    if (if_frag) {
      appendix <- "Приложение: Результаты фрагментного секвенирования… – на стр."
    } else {
      appendix <- "Приложение: Результаты полногеномного секвенирования… – на стр."
    }
  }
  return(appendix)
}

#создание строки с подписью
createSignature <- function() {
  sig <- paste("Директор института",
               paste(rep(" ", 76), collapse = ""),
               "С.В. Балахонов",
               collapse = "")
  return(sig)
}

#создание строки с исполнителем
createExec <- function() {
  executor <- c("Сидорова Е.А. +7 (3952) 220-139")
  return(executor)
}

#создание заголовка таблицы 
createTblhead <- function() {
  if (if_frag) {
    tblhead <- "Результаты фрагментного секвенирования образцов биологического материала от больных COVID-19"
  } else {
    tblhead <- "Результаты полногеномного секвенирования образцов биологического материала от больных COVID-19"
  }
  return(tblhead)
}

#создание таблицы
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

#сборка документа
formDocx <- function(full_table, region_key) {
  if_else(
    if_frag,
    docFileName <- glue("{Sys.Date()}_{recipient_info$rec[recipient_info$key == region_key]}_wgs.docx"),
    docFileName <- glue("{Sys.Date()}_{recipient_info$rec[recipient_info$key == region_key]}_frag.docx"))
  
  doc <- read_docx(here("templates/template.docx")) %>%
    body_add_flextable(createHeading(region_key)) %>% 
    body_add_par("", style = "Normal") %>% 
    body_add_par("", style = "Normal") %>% 
    body_add_par(createGreeting(region_key), style = "my_heading") %>% 
    body_add_par("", style = "Normal") %>% 
    body_add_par(createBody(region_key), style = "my_body") %>% 
    body_add_par("", style = "Normal") %>% 
    body_add_par(createAppx(region_key), style = "my_body") %>% 
    body_add_par("", style = "Normal") %>% 
    body_add_par("", style = "Normal") %>% 
    body_add_par("", style = "Normal") %>% 
    body_add_par(createSignature(), style = "my_body") %>% 
    body_add_par_n(16) %>% 
    body_add_par(createExec(), style = "my_exec")
    
  
  if(region_key != "UENFS") {
    doc <- doc %>% 
      body_add_break(pos = "after") %>% 
      body_add_par("Приложение", style = "my_appx") %>% 
      body_add_par(createTblhead(), style = "my_tblhead") %>%
      body_add_flextable(createTbl(full_table, region_key, if_frag))
  }
  
  print(doc, target = glue("./output/{Sys.Date()}/{docFileName}"))
}

########## MAIN FLOW ##########

## Создание папки с результатами
dir.create(glue("./output/{Sys.Date()}"), showWarnings = FALSE, recursive = TRUE)

## Загрузка шаблонных данных
iso_dict <- read_csv(here("templates/iso_dict.csv"))
recipient_info <- read_csv(here("templates/recipient_info.csv"))
sender_info <- paste(read_lines(here("templates/nipchi.txt")), collapse = "\n")
body_text <- read_csv(here("templates/body.csv")) %>% 
  mutate(body = stri_split_fixed(body, "\\n"))

## Выгрузка информации по полногеномным данным за отчётную дату
full_table <- SitGetQuery(CreateQuery(report_date, if_frag))

#на какие территории выдавать письма
region_key <- c("CNIIE",
                "UENFS",
                iso_dict$key[iso_dict$region %in% unique(full_table$region)])

## Формировани и сохранение информационных писем
l <- list(full_table = full_table)
purrr::map2(l, region_key, formDocx)


# Загрузка стандартных пакетов --------------------------------------------

suppressPackageStartupMessages({
    library("glue")
    library("here")
    library("readr")
    library("magrittr")
    library("dplyr")
    library("stringi")
    library("officer")
    library("openxlsx")
    library("flextable")
    library("RPostgres")
    library("purrr")
})

# Загрузка пользовательских функций ---------------------------------------

funcs <- c("src/body_add_par_n.R",
           "src/createAppxString.R",
           "src/createBody.R",
           "src/createHeading.R",
           "src/CreateQuery.R",
           "src/createTbl.R",
           "src/createTblhead.R",
           "src/formDocx.R",
           "src/SitGetQuery.R",
           "src/createFormTbl.R",
           "src/packXLSX.R")

for (i in seq_along(funcs)) {
    source(funcs[i], encoding = "utf-8")
}

# Параметры ---------------------------------------------------------------

if_frag <- TRUE # Переключатель выдачи фрагментных/полногеномных рез-тов
report_date <- as.Date('2022-02-12')
# report_date <- Sys.Date()

# Загрузка типовых данных -------------------------------------------------

iso_dict <- read_csv(here("templates/iso_dict.csv"))
recipient_info <- read_csv(here("templates/recipient_info.csv"))
sender_info <- paste(read_lines(here("templates/nipchi.txt")), collapse = "\n")
body_text <- read_csv(here("templates/body.csv")) %>% 
    mutate(body = stri_split_fixed(body, "\\n"))

# Создание папки с результатами
dir.create(glue("./output/{report_date}"), showWarnings = FALSE, recursive = TRUE)

# Выдача писем ------------------------------------------------------------

# Выгрузка информации по данным секвенирования за отчётную дату
full_table <- SitGetQuery(CreateQuery(report_date = report_date,
                                      if_frag = if_frag,
                                      aim = "letter"))

# Определение списка территорий для выдачи писем
region_key <- c("CNIIE",
                "UENFS",
                iso_dict$key[iso_dict$region %in% unique(full_table$region)])

# Формировани и сохранение информационных писем
l <- list(full_table = full_table)
map2(l, region_key, formDocx)

# Выдача персонифицированных форм -----------------------------------------

# Выгрузка причёсанной информации по данным секвенирования за отчётную дату
total_forms <- SitGetQuery(CreateQuery(report_date = report_date,
                                       if_frag = if_frag,
                                       aim = "form"))

# Определение списка территорий для выдачи форм
rk_forms <- unique(total_forms$region)

# Формирование и сохранение персонифицированных форм
map(rk_forms,
    function(x) packXLSX(raw_data = total_forms,
                         regn = x,
                         report_date = report_date,
                         if_frag = if_frag))
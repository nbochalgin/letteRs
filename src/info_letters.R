
# Загрузка стандартных пакетов --------------------------------------------

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

# Загрузка пользовательских функций ---------------------------------------

funcs <- c("src/body_add_par_n.R",
           "src/createAppxString.R",
           "src/createBody.R",
           "src/createHeading.R",
           "src/CreateQuery.R",
           "src/createTbl.R",
           "src/createTblhead.R",
           "src/formDocx.R",
           "src/SitGetQuery.R")

for (i in seq_along(funcs)) {
  source(funcs[i], encoding = "utf-8")
}

# Параметры ---------------------------------------------------------------

if_frag <- FALSE # Переключатель выдачи фрагментных/полногеномных рез-тов
report_date <- as.Date('2022-02-12')
# report_date <- Sys.Date()

# Загрузка типовых данных -------------------------------------------------

iso_dict <- read_csv(here("templates/iso_dict.csv"))
recipient_info <- read_csv(here("templates/recipient_info.csv"))
sender_info <- paste(read_lines(here("templates/nipchi.txt")), collapse = "\n")
body_text <- read_csv(here("templates/body.csv")) %>% 
  mutate(body = stri_split_fixed(body, "\\n"))

# Main Flow ---------------------------------------------------------------

# Создание папки с результатами
dir.create(glue("./output/{report_date}"), showWarnings = FALSE, recursive = TRUE)

# Выгрузка информации по данным секвенирования за отчётную дату
full_table <- SitGetQuery(CreateQuery(report_date = report_date,
                                      if_frag = if_frag))

# Определение списка территорий для выдачи писем
region_key <- c("CNIIE",
                "UENFS",
                iso_dict$key[iso_dict$region %in% unique(full_table$region)])

# Формировани и сохранение информационных писем
l <- list(full_table = full_table)
purrr::map2(l, region_key, formDocx)

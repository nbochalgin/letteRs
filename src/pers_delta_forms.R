
# Загрузка стандартных пакетов --------------------------------------------

suppressPackageStartupMessages({
  library("dplyr")
  library("glue")
  library("here")
  library("magrittr")
  library("openxlsx")
  library("purrr")
  library("readr")
  library("RPostgres")
  library("stringi")
})

# Загрузка пользовательских функций ---------------------------------------

funcs <- c("src/CreateQuery.R",
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

# Main Flow ---------------------------------------------------------------

# Выгрузка причёсанной информации по данным секвенирования за отчётную дату
total_forms <- SitGetQuery(CreateQuery(report_date = report_date,
                                       if_frag = if_frag,
                                       aim = "form"))

# Определение списка территорий для выдачи форм
rk_forms <- unique(total_forms$region)

# Формирование и сохранение персонифицированных форм
purrr::map(rk_forms,
           function(x) packXLSX(raw_data = total_forms,
                                regn = x,
                                report_date = report_date,
                                report_date = report_date))
#' Query creator
#' 
#' @param report_date Date (YYYY-MM-DD) of report.
#' @param if_frag TRUE/FALSE. A flag to mark sequencing method. Default FALSE for WGS.
#'
#' @return A query text
#' @export
#'
#' @examples
#' CreateQuery(report_date = 2022-02-06, if_frag = TRUE)

CreateQuery <- function(report_date = Sys.Date(), if_frag = FALSE, aim = "letter") {

  if (if_frag) {wgs_id_mark <- "IS NULL"} else {wgs_id_mark <- "IS NOT NULL"}
  
  if (aim == "letter") {
    query <- glue(
"    SELECT income.nipchi_id::int AS nipchi_id
          , income.income_id AS number
          , income.region AS region
          , frag_res.variant AS variant
          , wgs_res.pango_lineage AS pango_lineage
       FROM income_probes AS income
  LEFT JOIN frag_seq_results AS frag_res
         ON frag_res.nipchi_id = income.nipchi_id
  LEFT JOIN wgs_results AS wgs_res
         ON wgs_res.nipchi_id = frag_res.nipchi_id
      WHERE frag_res.variant != 'Не определено'
        AND frag_res.date_end = '{report_date}'
        AND wgs_res.nipchi_id {wgs_id_mark}
        AND frag_res.variant IN ('Delta', 'Omicron', 'Probable Omicron', 'Иной')
   ORDER BY nipchi_id;")
  
  } else if (aim == "form") {
    query <- glue(
"    SELECT income.nipchi_id::int AS nipchi_id
          , income.income_id AS number
          , income.fio AS fio
          , income.region AS region
          , income.sex AS sex
          , income.age AS age
          , income.last_disease_start_date AS disease_date
          , LOWER(income.clinic) AS clinic
          , 'ФКУЗ Иркутский НИПЧИ' AS lab
          , frag_res.variant AS variant
          , frag_res.date_end AS date_end
          , income.epid_anamnesis AS anamnesis
       FROM income_probes AS income
  LEFT JOIN frag_seq_results AS frag_res
         ON frag_res.nipchi_id = income.nipchi_id
  LEFT JOIN wgs_results AS wgs_res
         ON wgs_res.nipchi_id = frag_res.nipchi_id
      WHERE wgs_res.nipchi_id {wgs_id_mark}
        AND frag_res.date_end = '{report_date}'
        AND frag_res.variant IN ('Delta', 'Omicron', 'Probable Omicron', 'Иной')
        AND frag_res.variant != 'Не определено'
   ORDER BY nipchi_id")
  }

  return(query)
}


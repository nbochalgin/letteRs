#формировать текст запроса в БД на дату отчёта
CreateQuery <- function(report_date, if_frag) {
  if (if_frag) {wgs_id_mark <- "IS NULL"} else {wgs_id_mark <- "IS NOT NULL"}
  
  glue(
    "    SELECT income.nipchi_id AS nipchi_id
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
  
}
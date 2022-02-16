#упаковка и форматирование xlsx
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

packXLSX <- function(raw_data, regn, report_date, if_frag) {
    
    region_form <- createFormTbl(raw_data = raw_data,
                                 reg = regn)
    
    wb <- createWorkbook()
    sh <- addWorksheet(wb, "Таблица  1 (персонифиц)")
    
    writeData(wb, sh, "Персонинифицированный учет случаев выявления мутаций SARS CoV-2",
              startCol = 2, startRow = 1)
    writeData(wb, sh, region_form$header,
              startCol = 1, startRow = 2)
    writeData(wb, sh, region_form$form,
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
        rows = 4:(max(unlist(rlist::list.map(region_form, length(.$`ФИО`))))+3),
        cols = 1:25,
        gridExpand = TRUE
    )
    
    addStyle(
        wb, sh, dateStyle,
        rows = 4:(max(unlist(rlist::list.map(region_form, length(.$`ФИО`))))+3),
        cols = c(11, 17),
        gridExpand = TRUE,
        stack = TRUE
    )
    
    setColWidths(wb, sh, cols = 1:25, widths = "auto", ignoreMergedCells = TRUE)
    
    saveWorkbook(
        wb,
        if_else(
            if_frag,
            glue("output/{report_date}/ФОРМА_перс_учета_Дельта_вар_{report_date}_{regn}_frag.xlsx"),
            glue("output/{report_date}/ФОРМА_перс_учета_Дельта_вар_{report_date}_{regn}_wgs.xlsx")),
        overwrite = TRUE)
}















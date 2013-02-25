# library stringr is used: function str_c
library(stringr)
# libray xtable used in creating the table
library(xtable)

DataTableJSTags <- function()
{
  list(
    tags$link(href="css/DT_bootstrap.css",rel="stylesheet",type="text/css"),
    tags$link(href="css/TableTools.css",rel="stylesheet",type="text/css"),
    tags$script(src="js/jquery.dataTables.js",type="text/javascript"),
    tags$script(src="js/dataTables.numericCommaSort.js",type="text/javascript"),
    tags$script(src="js/dataTables.numericCommaTypeDetect.js",type="text/javascript"),
    tags$script(src="js/DT_bootstrap.js",type="text/javascript"),
    tags$script(src="js/TableTools.min.js",type="text/javascript")   
  )                
}

htmlTableHead <- function(df)
{
  cNames = colnames(df)
  res = paste('<TR> <TH> ', str_c(cNames, collapse='</TH> \n <TH> '), '</TH> \n </TR>')
  thead = paste(c('<thead>', res, '</thead>'), sep='\n', collapse=' ')
  return(thead)
}

htmlTable <- function(df,table_id)
{
  thead = htmlTableHead(df)
  res = capture.output(print(xtable(df), type='html', only.contents=TRUE,include.rownames=FALSE, include.colnames=FALSE, sep='\n'))
  tbody = paste(c('<tbody>', res, '</tbody>'), sep='\n', collapse=' ')
  
  tblHtmlS = paste('<table cellpadding="0" cellspacing="0" border="0" class="table table-striped table-bordered" id="',table_id,'">',sep="")
  tblHtmlE = '</table>'
  
  tbl = paste(c(tblHtmlS, thead, tbody, tblHtmlE), sep='\n', collapse=' ')
  
  return(tbl)
}

DataTableScript <- function(table_id) {
  return(paste('<script>
                $(function() {$("#', table_id, '").dataTable({"sDom": \'T<"clear">lfrtip\',
                                                              "oTableTools": { "sSwfPath": "swf/copy_csv_xls_pdf.swf" }
                }); });
               </script>', sep=""))
}

reactiveDataTable <- function(table_id,func) 
{
  reactive(function() 
  {
    data <- func()
    if (is.null(data) || is.na(data))
      return(paste(em("Preparing table ...")))
    return(paste(htmlTable(data,table_id),DataTableScript(table_id)))
  })
}
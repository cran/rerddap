#' Get information on an ERDDAP dataset.
#'
#' @export
#'
#' @param datasetid Dataset id
#' @param url A URL for an ERDDAP server. Default:
#' https://upwell.pfeg.noaa.gov/erddap/ - See [eurl()] for 
#' more information
#' @param ... Further args passed on to [crul::verb-GET] (must be a
#' named parameter)
#' @param x A datasetid or the output of `info`
#' @return Prints a summary of the data on return, but you can index to
#' various information.
#'
#' The data is a list of length two with:
#' 
#' - variables - Data.frame of variables and their types
#' - alldata - List of data variables and their full attributes
#' 
#' Where `alldata` element has many data.frame's, one for each variable,
#' with metadata for that variable. E.g., for griddap dataset
#' `noaa_pfeg_696e_ec99_6fa6`, `alldata` has:
#' 
#' - NC_GLOBAL
#' - time
#' - latitude
#' - longitude
#' - sss
#' 
#' @references https://upwell.pfeg.noaa.gov/erddap/index.html
#' @examples \dontrun{
#' # grid dap datasets
#' info('erdATastnhday')
#'
#' (out <- ed_search(query='temperature'))
#' info(out$info$dataset_id[5])
#' info(out$info$dataset_id[15])
#' info(out$info$dataset_id[25])
#' info(out$info$dataset_id[150])
#' info(out$info$dataset_id[400])
#' info(out$info$dataset_id[678])
#'
#' out <- info(datasetid='erdMBchla1day')
#' ## See brief overview of the variables and range of possible values, if given
#' out$variables
#' ## all information on longitude
#' out$alldata$longitude
#' ## all information on chlorophyll
#' out$alldata$chlorophyll
#'
#' # table dap datasets
#' (out <- ed_search(query='temperature', which = "table"))
#' info(out$info$dataset_id[1])
#' info(out$info$dataset_id[2])
#' info(out$info$dataset_id[3])
#' info(out$info$dataset_id[4])
#'
#' info('erdCinpKfmBT')
#' out <- info('erdCinpKfmBT')
#' ## See brief overview of the variables and range of possible values, if given
#' out$variables
#' ## all information on longitude
#' out$alldata$longitude
#' ## all information on Haliotis_corrugata_Mean_Density
#' out$alldata$Haliotis_corrugata_Mean_Density
#'
#' # use a different ERDDAP server
#' ## Marine Institute (Ireland)
#' info("IMI_CONN_2D", url = "http://erddap.marine.ie/erddap/")
#' }

info <- function(datasetid, url = eurl(), ...){
  url <- sub("/$", "", url)
  json <- erdddap_GET(sprintf(file.path(url, 'info/%s/index.json'), datasetid),
                      NULL, ...)
  colnames <- vapply(tolower(json$table$columnNames),
                     function(z) gsub("\\s", "_", z), "", USE.NAMES = FALSE)
  dfs <- lapply(json$table$rows, function(x){
    tmp <- data.frame(x, stringsAsFactors = FALSE)
    names(tmp) <- colnames
    tmp
  })
  lists <- lapply(json$table$rows, stats::setNames, nm=colnames)
  names(lists) <- vapply(lists, function(b) b$variable_name, "",
                         USE.NAMES = FALSE)
  outout <- list()
  for (i in seq_along(lists)) {
    outout[[names(lists[i])]] <-
      unname(lists[ names(lists) %in% names(lists)[i] ])
  }

  df <- data.frame(rbindlist(dfs))
  vars <- df[ df$row_type == 'variable', names(df) %in%
                c('variable_name','data_type')]
  actual <- vapply(split(df, df$variable_name), function(z){
    tmp <- z[ z$attribute_name %in% 'actual_range' , "value"]
    if(length(tmp)==0) "" else tmp
  }, "")
  actualdf <- data.frame(variable_name=names(actual),
                         actual_range=unname(actual))
  vars <- merge(vars, actualdf, by="variable_name")
  oo <- lapply(outout, function(x) data.frame(rbindlist(x)))
  structure(list(variables=vars, alldata=oo, base_url=url),
            class="info",
            datasetid=datasetid,
            type=table_or_grid(datasetid, url))
}

#' @export
print.info <- function(x, ...) {
  global <- x$alldata$NC_GLOBAL
  tt <- global[ global$attribute_name %in%
                  c('time_coverage_end','time_coverage_start'), "value", ]
  dims <- x$alldata[dimvars(x)]
  vars <- x$alldata[x$variables$variable_name]
  cat(sprintf("<ERDDAP info> %s", attr(x, "datasetid")), "\n")
  cat(paste0(" Base URL: ", x$base_url), "\n")
  cat(paste0(" Dataset Type: ", attr(x, "type")), "\n")
  if(attr(x, "type") == "griddap") cat(" Dimensions (range): ", "\n")
  for(i in seq_along(dims)){
    if(names(dims[i]) == "time"){
      cat(sprintf("     time: (%s, %s)", tt[2], tt[1]), "\n")
    } else {
      cat(sprintf("     %s: (%s)", names(dims[i]), foo(dims[[i]],
                                                       "actual_range")), "\n")
    }
  }
  cat(" Variables: ", "\n")
  for(i in seq_along(vars)){
    cat(sprintf("     %s:", names(vars[i])), "\n")
    ar <- foo(vars[[i]], "actual_range")
    if(!length(ar) == 0) cat("         Range:",
                             foo(vars[[i]], "actual_range"), "\n")
    un <- foo(vars[[i]], "units")
    if(!length(un) == 0) cat("         Units:", foo(vars[[i]], "units"), "\n")
  }
}

foo <- function(x, y){
  x[ x$attribute_name == y, "value"]
}

#' @export
#' @rdname info
as.info <- function(x, url) {
  UseMethod("as.info")
}

#' @export
as.info.info <- function(x, url) {
  x
}

#' @export
as.info.character <- function(x, url) {
  info(x, url)
}

table_or_grid <- function(datasetid, url) {
  url <- sub("/$", "", url)
  table_url <- file.path(url, 'tabledap/index.json')
  tab <- toghelper(table_url)
  if (datasetid %in% tab) "tabledap" else "griddap"
}

toghelper <- function(url) {
  out <- erdddap_GET(url, list(page = 1, itemsPerPage = 10000L))
  nms <- out$table$columnNames
  lists <- lapply(out$table$rows, stats::setNames, nm = nms)
  vapply(lists, "[[", "", "Dataset ID")
}

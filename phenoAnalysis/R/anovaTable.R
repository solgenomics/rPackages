#' Generates the anova table
#'
#' @param modelOut the anova model output of merModLmerTest class
#' @param tableType a character vector indicating type of anova table output. Defaults to 'html' for web display and 'text' for saving in a file (pass the file name to the 'out' argument to save output in a file) .
#' @param traitName the trait name to be used in the table title, optional.
#' @param ... arguments to be passed to \code{\link[stargazer]{stargazer}}.
#' @seealso see \code{\link[stargazer]{stargazer}} to modify the table output.
#' @return a stargazer formatted anova table of chosen type, html type by default.
#' @export
#'

getAnovaTable <- function(modelOut, tableType=NULL, traitName=NULL, ...) {

  if (class(modelOut)[1] == 'lmerModLmerTest') {
    anovaTable <- anova(modelOut)

    if (is.null(tableType)) {
      tableType <- 'text'
    }

   title <- paste0('ANOVA result: ', traitName)

    table <- capture.output(stargazer(anovaTable,
              type= tableType,
              title=title,
              summary=FALSE,
              digits=2,
              header=FALSE,
              ...))

    return(table)

    }
}

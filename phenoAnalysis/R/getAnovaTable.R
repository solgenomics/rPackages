#' Generates the anova table
#'
#' @param modelOut the anova model output of merModLmerTest class
#' @param tableType a character vector indicating type of anova table output. Defaults to 'html' for web display and 'text' for saving in a file (pass the file name to the 'out' argument to save output in a file) .
#' @param traitName the trait name to be used in the table title, optional.
#' @param ... arguments to be passed to \code{\link[stargazer]{stargazer}}.
#' @seealso see \code{\link[stargazer]{stargazer}} to modify the table output.
#' @return a stargazer formatted anova table of chosen type, html type by default.
#'   The values have 4 significant digits and the p value 3, so a small p value
#'   shows as 1.3e-06, not 0, and a trait with small values keeps them.
#' @export
#'

getAnovaTable <- function(modelOut, tableType=NULL, traitName=NULL, ...) {

  if (class(modelOut)[1] == 'lmerModLmerTest') {
    anovaTable <- formatAnovaTable(anova(modelOut))

    if (is.null(tableType)) {
      tableType <- 'text'
    }

   title <- paste0('ANOVA result: ', traitName)

    table <- capture.output(stargazer(anovaTable,
              type= tableType,
              title=title,
              summary=FALSE,
              header=FALSE,
              ...))

    return(table)

    }
}

# The ANOVA table as text: 4 significant digits for the sums of squares, mean
# squares, degrees of freedom and F value, and the p value as R prints it.
formatAnovaTable <- function(anovaTable) {

  anovaTable <- as.data.frame(anovaTable)
  for (column in names(anovaTable)) {
    values <- anovaTable[[column]]
    if (column == 'Pr(>F)') {
      anovaTable[[column]] <- format.pval(values, digits=3)
    } else {
      anovaTable[[column]] <- vapply(values, format, character(1), digits=4)
    }
  }

  anovaTable
}

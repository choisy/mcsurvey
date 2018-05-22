#' Checks for replicates in a nested design
#'
#' Check for replicates in a nested design where the different levels can be
#' strata and/or clusters.
#'
#' All the strata combinations should be combined into one stratum.
#'
#' @param df a data frame
#' @param ... variables of the data frame specifying the survey design. Should
#' be the combination of strata first, then clusters from outer to inner level
#' (i.e. primary sampling unit, then secondary sampling unit, etc...)
#'
#' @examples
#' data(tz7)
#' ## in this data set the combination of strata is defined by the variable v023,
#' ## and the primary and secondary sampling units are defined by the variables
#' ## v001 and v002 respectively.
#' check_replicates(tz7, v023, v001, v002)
#' ## we can see here that the primary sampling unit 142 of the combination of
#' ## strata 13 lacks a replication of secondary sampling unit.
#'
#' @author Marc Choisy
#'
#' @export
check_replicates <- function(df, ...) {
  sel <- 1:2 # the first two column of a data frame.
  vars <- sapply(substitute(list(...))[-1], deparse) # retrieving the ... variables
  df <- df[, vars]
# dealing with first level:
  tmp <- table(df[, sel])
  out <- rownames(tmp)[which(rowSums(tmp > 0) < 2)]
# dealing with the other levels if there are more than 1:
# here the idea is that we first merge the first 2 columns and then work on the
# new first 2 columns and we repeat the process until there is nothing left to
# merge.
  if (ncol(df) > 2) for(i in seq_along(ncol(df) - 2)) {
    df <- data.frame(apply((df[, sel]), 1, paste, collapse = "-"), df[, -sel])
    tmp <- table(df[, sel])
    out <- c(out, rownames(tmp)[which(rowSums(tmp > 0) < 2)])
  }
# putting in shape: splitting the strings by "-", transforming into integers,
# and adding names to the vectors according to the variables that define the
# levels:
  lapply(out, function(x) {
    x <- as.integer(unlist(strsplit(x, "-")))
    setNames(x, vars[seq_along(x)])
  })
}

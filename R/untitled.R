# record(df) -> tape
# play(newdata, tape) -> playb
# playb

# playb$detailed_checks$new_level

# record(df) -> tape
#
# play(newdata, tape) -> playback

#' import data.table
check_matrix <- function(x) {
  # convert checks to data.tables and bind them.
  dts <- lapply(x, as.data.table)
  do.call(cbind, dts)
}

write_violations <- function (violations) {
  # create violation matrix with colnames as entries.
  vm  <- matrix(data  = t(rep(    x = paste0(colnames(violations), ";"),
                                 times = nrow(violations))),
               ncol  = ncol(violations),
               byrow = TRUE)
  # replace FALSE with empty string.
  vm[violations == FALSE] <- ""
  # concatenate warnings to one string pr. row.
  do.call(what = paste0, args = data.frame(vm))
}


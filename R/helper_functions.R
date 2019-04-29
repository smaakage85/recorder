compress_detailed_checks <- function(dc) {

  # order list by checks (in stead of columns).
  dc <- order_by_checks(dc)

  # compress checks.
  lapply(dc, compress_checks)

}

order_by_checks <- function(dc) {
  # extract unique check names.
  checks <- unique(do.call(c, lapply(dc, names)))
  # extract checks.
  by_checks <- lapply(checks, function (x) {lapply(dc, '[[', x)})
  # set names.
  names(by_checks) <- checks
  by_checks
}

compress_checks <- function(x) {
  x[vapply(x, any, logical(1))]
}

paste_colnames <- function(x) {
  if (length(x) > 0) {
    paste0(names(x), collapse = ", ")
  } else {"None."}
}

print_checks_collevel <- function(x, name) {
  
  cat("> '", 
      name,
      "': ",
      paste_colnames(x$checks[[name]]), 
      "\n",
      sep = "")
  
  # return invisibly.
  invisible()
  
}

print_checks_rowlevel <- function(x, name, first = 10) {
  
  cat("> '", 
      name,
      "': ",
      paste_all_cols_with_rows(x$checks[[name]], first = first), 
      "\n",
      sep = "")
  
  # return invisibly.
  invisible()
  
}

paste_all_cols_with_rows <- function(x, first = 10) {

  if (length(x) == 0) {
    return("None.")
  }

  single_cols <- mapply(paste_col_with_rows, names(x), x, first = first, SIMPLIFY = FALSE)
  # paste results for all columns to one string.
  paste0(single_cols, collapse = ", ")

}

paste_col_with_rows <- function(name, x, first = 10) {
  x <- which(x)
  exceed_length <- max(length(x) - first)
  paste0(name, "[rows: ", paste0(x[seq_len(min(first, length(x)))],
                                 collapse = ", "),
         if (exceed_length > 0) {
           paste0(" and ", exceed_length, " more rows")
         } else {""},
         "]")
}

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

print_test_description <- function(pb, name) {
  
  cat("'", 
      name,
      "': ",
      pb$checks_metadata[[name]]$description, 
      "\n",
      sep = "")
  
  # return invisibly.
  invisible()
  
}





compress_detailed_checks <- function(dc) {
  
  # order list by checks (in stead of columns).
  dc <- order_by_checks(dc)
  
  # column-level checks.
  checks_col <- c("mismatch_levels")
  # compress column-level checks.
  dc[checks_col] <- lapply(dc[checks_col], compress_checks_col)
  
  # row-level checks.
  checks_row <- c("new_NA", "outside_range", "new_level")
  # compress row-level checks.
  dc[checks_row] <- lapply(dc[checks_row], compress_checks_row)
  
  dc

}

order_by_checks <- function(dc) {
  checks <- c("mismatch_levels", "new_NA", "outside_range", "new_level")
  by_checks <- lapply(checks, function (x) {lapply(dc, '[[', x)})
  names(by_checks) <- checks
  by_checks
}

compress_checks_col <- function(x) {
  names(x)[vapply(x, isTRUE, logical(1))]
}

compress_checks_row <- function(x) {
  x[vapply(x, has_pos_length, logical(1))]  
}

has_pos_length <- function(x) {
  length(x) > 0
}

paste_vector <- function(x) {
  if (length(x) != 0) {
    paste0(x, collapse = ", ")
  } else {"None."}
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
  exceed_length <- max(length(x) - first)
  paste0(name, "[rows: ", paste0(x[seq_len(min(first, length(x)))], 
                                 collapse = ", "), 
         if (exceed_length > 0) {
           paste0(" and ", exceed_length, " more rows")
         } else {""},
         "]")
}



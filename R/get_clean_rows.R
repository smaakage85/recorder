get_rows <- function(x, 
                     ignore_checks = NULL,
                     ignore_cols = NULL,
                     ignore_combo = NULL) {
  
  # column-wise checks.
  check_cols <- c(playback$aggregated_checks, 
                  playback$detailed_checks["mismatch_levels"])
  # ignore certain checks.
  check_cols <- check_cols[!names(check_cols) %in% ignore_checks]
  violations <- do.call(c, check_cols)
  # ignore certain columns
  violations <- violations[!violations %in% ignore_cols]
  # any failure in one of these checks makes all rows invalid.
  if (length(violations) > 0) {
      return(NULL)
  }
  
  # # row-wise checks.
  # check_rows <- playback$detailed_checks[!names(playback$detailed_checks) %in% "mismatch_levels"]
  # # ignore certain checks.
  # check_rows <- check_rows[!names(check_rows) %in% ignore_checks]
  # violations <- do.call(c, check_rows)
  # # ignore certain columns
  # violations <- violations[!violations %in% ignore_cols]
  
  }
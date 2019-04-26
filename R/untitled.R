# record(df) -> tape
# play(newdata, tape) -> playb
# playb

# playb$detailed_checks$new_level

record(df) -> tape

play(newdata, tape) -> playback

dc <- playback$detailed_checks[c("outside_range", "new_level", "new_NA")]
library(data.table)
check_matrix <- function(x) {
  dts <- lapply(dc, data.table::as.data.table)
  m <- do.call(cbind, dts)
}
dd <- check_matrix(dc)

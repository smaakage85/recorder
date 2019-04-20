df <- data.frame(
  a = c(2,3,4,5,8),
  b = c(NA_real_, 5,4,3,2),
  c = factor(letters[1:5]),
  d = factor(c(letters[1:4], NA_character_)),
  e = as.numeric(1:5),
  f = as.Date("2018-11-01"),
  g = 6:10,
  j = letters[1:5],
  x = letters[1:5]
)

newdata <- data.frame(
  a = c(3, 4, 4, 4, 10),
  b = c(5,4,4, 2, NA_real_),
  c = factor(letters[2:6]),
  d = factor(letters[1:5]),
  e = c(1:4, NA_real_),
  f = as.Date("2018-11-01"),
  h = 9:13,
  x = 1:5
)


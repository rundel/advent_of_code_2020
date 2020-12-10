library(tidyverse)


## Part 1

f = function(file) {
  file %>%
    readLines() %>%
    as.integer() %>%
    sort() %>%
    { c(0, ., max(.)+3) } %>%
    {. - dplyr::lag(.)} %>%
    .[-1]
}

counts = function(diffs) {
  c(sum(diffs == 1), sum(diffs == 3))
}

f("day10/test1.txt") %>% counts()
f("day10/test2.txt") %>% counts()

f("day10/input.txt") %>% counts() %>% prod()


## Part 2
g = function(file) {
  r = rle(f(file))

  purrr::map2_dbl(
    r$lengths, r$values,
    function(len, val) {
      if      (val == 1 & len == 2) 2
      else if (val == 1 & len == 3) 4
      else if (val == 1 & len == 4) 7
      else                          1
    }
  ) %>%
    prod() %>%
    formatC(digits = 14)
}

g("day10/test1.txt")
g("day10/test2.txt")

g("day10/input.txt")

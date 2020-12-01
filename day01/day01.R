library(tidyverse)

test = readLines("day01/test.txt") %>%
  as.integer()

input = readLines("day01/input.txt") %>%
  as.integer()

## Part 1

solve = function(d) {
  tidyr::expand_grid(x=d, y=d) %>%
    distinct() %>%
    mutate(
      sum = x+y,
      prod = x * y
    ) %>%
    filter(sum == 2020)
}

solve(test)
solve(input)


## Part 2

solve2 = function(d) {
  tidyr::expand_grid(x=d, y=d) %>%
    filter(x != y) %>%
    mutate(
      x2 = purrr::map2_int(x, y, min),
      y2 = purrr::map2_int(x, y, max)
    ) %>%
    select(x = x2, y = y2) %>%
    distinct() %>%
    filter(x+y < 2020 - min(d)) %>%
    tidyr::expand_grid(z = d) %>%
    mutate(
      sum = x + y + z,
      prod = x * y * z
    ) %>%
    filter(sum == 2020)
}

solve2(test)
solve2(input)

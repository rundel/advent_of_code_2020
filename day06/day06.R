library(tidyverse)

test = "day06/test.txt"
input = "day06/input.txt"

## Part 1

f = function(file) {
  d = readr::read_file(file) %>%
    str_trim() %>%
    strsplit("\n\n") %>%
    .[[1]] %>%
    strsplit("\n") %>%
    purrr::map( ~unlist(strsplit(.x, ""))) %>%
    purrr::map(table)

  purrr::map_int(d, length)
}

sum(f(test))
sum(f(input))


## Part 2

g = function(file) {
  d = readr::read_file(file) %>%
    str_trim() %>%
    strsplit("\n\n") %>%
    .[[1]] %>%
    strsplit("\n")

  n = purrr::map_int(d, length)

  purrr::map2_int(
    d, n,
    ~ sum( table(unlist(strsplit(.x, ""))) == .y )
  )
}


sum(g(test))
sum(g(input))

library(tidyverse)

# Part 1

f = function(file) {
  readLines(file) %>%
    stringr::str_split("-|: | ") %>%
    purrr::map_lgl(
      function(x) {
        a = as.integer(x[1])
        b = as.integer(x[2])
        p = x[3]
        s = x[4]
        s = stringr::str_remove_all(s, paste0("[^",p,"]"))
        (nchar(s) >= a) & (nchar(s) <= b)
      }
    )
}

sum( f("day02/test.txt") )
sum( f("day02/input.txt") )


# Part 2

g = function(file) {
  readLines(file) %>%
    stringr::str_split("-|: | ") %>%
    purrr::map_lgl(
      function(x) {
        a = as.integer(x[1]); b = as.integer(x[2])
        p = x[3]; s = x[4]

        char1 = substr(s, a, a)
        char2 = substr(s, b, b)

        (char1 == p | char2 == p) & (char1 != char2)
      }
    )
}

sum( g("day02/test.txt") )
sum( g("day02/input.txt") )


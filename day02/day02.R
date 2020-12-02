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

        char1 = substr(p, a, a)
        char2 = substr(p, b, b)

        print(s)
        s = stringr::str_remove_all(s, paste0("[^",p,"]"))
        cat(nchar(s),a ,b, (nchar(s) >= a) & (nchar(s) <= b), "\n")
        (nchar(s) >= a) & (nchar(s) <= b)
      }
    )
}

sum( f("day02/test.txt") )
sum( f("day02/input.txt") )

556

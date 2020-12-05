library(tidyverse)



bin_part = function(s, low) {
  n = length(s)
  r = c(0, 2^n-1)

  for(l in s) {
    n = n-1
    mid = 2^n-1

    #cat(r)

    if (l == low) {
      r[2] = r[1] + mid
    } else {
      r[1] = r[2] - mid
    }

    #cat(" ->", r, "\n")
  }

  unique(r)
}

f = function(file) {
  readLines(file) %>%
    strsplit("") %>%
    purrr::map(
      function(x) {
        n = length(x)
        c(
          bin_part(x[1:(n-3)], "F"),
          bin_part(x[(n-2):n], "L")
        )
      }
    )
}

## Part 1

f("day05/test.txt") %>% map_dbl(~ .x[1]*8 + .x[2])

f("day05/input.txt") %>% map_dbl(~ .x[1]*8 + .x[2]) %>% max()


## Part 2

ids = f("day05/input.txt") %>% map_dbl(~ .x[1]*8 + .x[2]) %>% sort()
ids[which( (ids - lag(ids)) == 2 ) + -1:0]

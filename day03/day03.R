library(tidyverse)

test = "day03/test.txt"
input = "day03/input.txt"

f = function(file = test, right, down) {
  m = readLines(file) %>%
    strsplit("") %>%
    do.call(rbind, .)

  nr = nrow(m)
  nc = ncol(m)

  x = 1
  y = 1

  trees = 0

  while (y <= nr) {
    #print(c(x,y))
    #print(c(m[y,x], m[y,x] == "#"))
    if (m[y,x] == "#")
      trees = trees + 1

    x = (x + right - 1) %% nc + 1
    y = (y + down)
  }

  trees
}

f(test, 3, 1)
f(input, 3, 1)

### Part 2


g = function(file, rights, lefts) {
  purrr::map2_dbl(
    rights, lefts, f, file = file
  ) %>%
    purrr::reduce(`*`)
}

g(test, c(1,3,5,7,1), c(1,1,1,1,2))
g(input, c(1,3,5,7,1), c(1,1,1,1,2))

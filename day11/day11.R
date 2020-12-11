library(tidyverse)

file = "day11/test.txt"

parse = function(file) {
  file %>%
    readLines() %>%
    str_split("") %>%
    do.call(rbind, .)
}

## Part 1

neighbors = function(x, y, m) {
  nc = ncol(m)
  nr = nrow(m)

  n_x = x + c(-1,0,1,-1,1,-1,0,1)
  n_y = y + c(-1,-1,-1,0,0,1,1,1)

  out = n_x < 1 | n_y < 1 | n_x > nc | n_y > nr

  n_y[!out] + nc * (n_x[!out]-1)
}

f = function(file, nb_func = neighbors, n_leave = 4, debug = FALSE) {

  m = parse(file)
  inds = which(m != ".", arr.ind = TRUE) %>%
    as_tibble() %>%
    select(x = col, y = row) %>%
    mutate(
      neighbors = purrr::map2(x, y, nb_func, m = m)
    )

  repeat {
    new_m = m

    for(i in seq_len(nrow(inds))) {
      y = inds$y[[i]]
      x = inds$x[[i]]

      val = m[y, x]
      nb_i = inds$neighbors[[i]]
      nb_val = m[nb_i]

      # If a seat is empty (L) and there are no occupied seats adjacent to it,
      # the seat becomes occupied.

      if (val == "L" & !any(nb_val == "#")) {
        new_m[y, x] = "#"
      }

      # If a seat is occupied (#) and `n_leave` or more seats adjacent to it are also
      # occupied, the seat becomes empty.
      else if (val == "#" & (sum(nb_val == "#") >= n_leave)) {
        new_m[y, x] = "L"
      }
    }

    if (identical(m, new_m))
      break

    m = new_m

    if (debug)
      print(m)
  }

  print(m)
  sum(m == "#")
}

f("day11/test.txt")
f("day11/input.txt")

## Part 2

neighbors2 = function(x, y, m) {
  nc = ncol(m)
  nr = nrow(m)

  dirs = list(
    c(-1,-1), c(-1, 0), c(-1, 1),
    c( 0,-1),           c( 0, 1),
    c( 1,-1), c( 1, 0), c( 1, 1)
  )

  n_x = c()
  n_y = c()

  for (dir in dirs) {
    cx = x
    cy = y

    repeat {
      cx = cx + dir[2]
      cy = cy + dir[1]

      if (cx < 1 | cy < 1 | cx > nc | cy > nr)
        break

      if (m[cy, cx] != ".") {
        n_x = c(n_x, cx)
        n_y = c(n_y, cy)
        break
      }
    }
  }

  n_y + nc * (n_x-1)
}

neighbors2(10,1,m)
neighbors2(1,2,m)


f("day11/test.txt", nb_func = neighbors2, n_leave=5)
f("day11/input.txt", nb_func = neighbors2, n_leave=5)

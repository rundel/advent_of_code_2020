library(tidyverse)

file = 'day12/test.txt'

parse = function(file) {
file %>%
  readLines()%>%
  purrr::map_dfr(
    ~ list(dir = substr(., 1,1), val = substr(.,2, nchar(.)) %>% as.numeric() )
  )
}

# Part 1

f = function(file, debug = FALSE) {
  m = parse(file)

  d = 0
  x = 0
  y = 0

  for(i in seq_len(nrow(m))) {

    if      (m$dir[i] == "N") {y = y + m$val[i]}
    else if (m$dir[i] == "S") {y = y - m$val[i]}
    else if (m$dir[i] == "E") {x = x + m$val[i]}
    else if (m$dir[i] == "W") {x = x - m$val[i]}
    else if (m$dir[i] == "L") {d = (d + m$val[i]) %% 360}
    else if (m$dir[i] == "R") {d = (d - m$val[i]) %% 360}
    else if (m$dir[i] == "F") {
      if      (d ==   0) {x = x + m$val[i]}
      else if (d ==  90) {y = y + m$val[i]}
      else if (d == 180) {x = x - m$val[i]}
      else if (d == 270) {y = y - m$val[i]}
    }
    else {
      stop("Oops")
    }

    if (debug)
      cat(d, x, y, "\n")
  }

  abs(x) + abs(y)
}

f("day12/test.txt")
f("day12/input.txt")

# Part 2

R = function(theta) { # Rotation matrix
  theta = theta * pi / 180
  matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
}

g = function(file, debug = FALSE) {
  m = parse(file)

  d = 0
  p = c(0, 0)
  w = c(10, 1)

  for(i in seq_len(nrow(m))) {
    if      (m$dir[i] == "N") {w[2] = w[2] + m$val[i]}
    else if (m$dir[i] == "S") {w[2] = w[2] - m$val[i]}
    else if (m$dir[i] == "E") {w[1] = w[1] + m$val[i]}
    else if (m$dir[i] == "W") {w[1] = w[1] - m$val[i]}
    else if (m$dir[i] == "L") {w = c( R(m$val[i]) %*% w )}
    else if (m$dir[i] == "R") {w = c( R(-m$val[i]) %*% w )}
    else if (m$dir[i] == "F") {p = p + w * m$val[i]}
    else {
      stop("Oops")
    }

    if (debug)
      cat(d, p, w, "\n")
  }

  sum(abs(p))
}

g("day12/test.txt", debug=TRUE)
g("day12/input.txt")

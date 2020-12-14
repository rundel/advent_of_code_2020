library(tidyverse)

as_binary = function(x, size = 36) {
  x = as.integer(x)
  bin = rep(0L, size)
  twos = rev( 2L^(seq_len(size)-1) )

  for(i in seq_len(size)) {
    if (twos[i] <= x) {
      bin[i] = 1
      x = x - twos[i]
    }
  }

  bin
}

from_binary = function(x) {
  sum(x * rev( 2L^(seq_along(x)-1)))
}


mask_value = function(value, mask) {
  value = as_binary(value)

  i = which(mask != "X")
  if (length(i) != 0)
    value[i] = as.numeric(mask[i])

  from_binary(value)
}

parse = function(file) {
  file %>%
    readLines() %>%
    strsplit(" = ")
}

## Part 1

f = function(file) {

  mem = list()
  mask = NA_character_

  code = parse(file)

  for (x in code) {
    if (x[1] == "mask")
      mask = strsplit(x[2], "")[[1]]
    else {
      i = str_match(x[1], pattern = "mem\\[(\\d+)\\]")[,2] %>% as.integer()
      mem[[i]] = mask_value(as.integer(x[2]), mask)
    }
  }

  mem
}

f("day14/test.txt") %>% unlist() %>% sum()
f("day14/input.txt") %>% unlist() %>% sum() %>% formatC(digits=16)



## Part 2

mask_addr = function(value, mask) {
  value = as_binary(value)

  value[mask == "1"] = 1
  value[mask == "X"] = NA

  value
}

expand_float_bits = function(x) {
  n_float = sum(is.na(x))

  # Use expand grid to get permutations
  xl = as.list(x)
  xl[is.na(x)] = list(0:1) %>%
    rep(n_float) %>%
    expand.grid()

  # Rows are the different binary values
  as.data.frame(xl) %>%
    apply(1, from_binary)
}


g = function(file) {

  mem = list()
  mask = NA_character_

  code = parse(file)

  for (x in code) {
    if (x[1] == "mask")
      mask = strsplit(x[2], "")[[1]]
    else {
      mem_addr = str_match(x[1], pattern = "mem\\[(\\d+)\\]")[,2] %>%
        as.integer() %>%
        mask_addr(mask) %>%
        expand_float_bits()

      mem[as.character(mem_addr)] = as.integer(x[2])
    }
  }

  mem
}

g("day14/test2.txt") %>% unlist() %>% sum()
g("day14/input.txt") %>% unlist() %>% sum() %>% formatC(digits=16)

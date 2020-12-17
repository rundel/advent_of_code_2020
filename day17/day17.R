library(tidyverse)

file = "day17/test.txt"

# Part 1

f = function(file, n=6) {

  nbs = expand.grid(x=-1:1, y=-1:1, z=-1:1) %>%
    filter( !(x == 0 & y == 0 & z == 0) )

  parse = function(file) {
    d = readLines(file)
    n = length(d)

    d %>%
      strsplit("") %>%
      unlist() %>%
      tibble(
        state = .,
        x = rep(1:n, n),
        y = rep(1:n, rep(n, n)),
        z = rep(1, n*n)
      ) %>%
      filter(state == "#")
  }

  pad = function(d) {
    build_range = function(i) (min(i)-1):(max(i)+1)

    expand.grid(
      state = ".",
      x = build_range(d$x),
      y = build_range(d$y),
      z = build_range(d$z)
    ) %>%
      anti_join(d, by = c("x", "y", "z")) %>%
      bind_rows(d, .)
  }


  active_nbs = function(d, nx, ny, nz) {
    semi_join(
      d,
      mutate(nbs, x = x+nx, y = y+ny, z = z+nz),
      by = c("x","y","z")
    ) %>%
      pull(state) %>%
      {sum(. == "#")}
  }


  d = parse(file)

  for(i in seq_len(n)) {

    d = d %>%
      pad() %>%
      rowwise() %>%
      mutate(
        n = active_nbs(d, x, y, z)
      ) %>%
      filter(
        (n %in% 2:3 & state == "#") |
        (n == 3 & state == ".")
      ) %>%
      mutate(
        state = "#"
      )

    cat("i = ", i, ": n_active = ", sum(d$state=="#"), "\n", sep="")
  }

  d
}

#f("day17/test.txt", 6) %>%
#  arrange(z,y,x)
#
#f("day17/input.txt", 6) %>%
#  arrange(z,y,x)




# Part 2

# Should work for any dim >= 2 and much bette performance than f.
g = function(file, n = 6, dim = 4) {

  stopifnot(dim >= 2)

  nbs = purrr::map(seq_len(dim), ~ -1:1) %>%
    do.call(expand.grid, .) %>%
    rowwise() %>%
    filter(!all(c_across() == 0)) %>%
    summarize(
      off = list(c_across()),
      .groups = "drop"
    )


  parse = function(file) {
    d = readLines(file)
    n = length(d)

    d %>%
      strsplit("") %>%
      unlist() %>%
      tibble(
        state = .
      ) %>%
      mutate(
        d1 = rep(1:n, n),
        d2 = rep(1:n, rep(n, n))
      ) %>%
      bind_cols(
        purrr::map(seq_len(dim-2), ~rep(1L, n*n)) %>%
          setNames(seq_len(dim-2))
      ) %>%
      filter(state == "#") %>%
      rowwise() %>%
      summarize(
        state = state,
        pt = list(c_across(-state)),
        .groups = "drop"
      )
  }

  pad = function(d) {
    expand_grid(
      d %>% select(-state),
      nbs
    ) %>%
      rowwise() %>%
      summarise(
        pt = list(pt + off),
        .groups = "drop"
      ) %>%
      distinct() %>%
      anti_join(d, by = "pt") %>%
      mutate(state = ".") %>%
      bind_rows(d, .)
  }

  active_nbs = function(d) {
    expand_grid(
      d,
      nbs
    ) %>%
      rowwise() %>%
    mutate(
      off = list(off + pt)
    ) %>%
      left_join(
        d %>% rename(nb_state = state, off = pt),
        "off"
      ) %>%
      group_by(state, pt) %>%
      summarise(
        n = sum(nb_state == "#", na.rm = TRUE),
        .groups = "drop"
      )
  }

  d = parse(file)

  for(i in seq_len(n)) {

    d = d %>%
      pad() %>%
      active_nbs() %>%
      filter(
        (n %in% 2:3 & state == "#") |
        (n == 3 & state == ".")
      ) %>%
      mutate(
        state = "#"
      ) %>%
      select(-n)

    cat("i = ", i, ": n_active = ", sum(d$state=="#"), "\n", sep="")
  }
}



g("day17/test.txt", 6, 3)
g("day17/input.txt", 6, 3)

g("day17/test.txt", 6, 4)
g("day17/input.txt", 6, 4)



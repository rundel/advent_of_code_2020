
library(tidyverse)

file = "day16/test.txt"

parse = function(file) {
  d = file %>%
    read_file() %>%
    str_split("\n\n", simplify = TRUE) %>%
    str_trim() %>%
    str_split("\n")


  rules = d[[1]] %>%
    tibble(d = .) %>%
    separate(d, c("name", "s1", "e1", "s2", "e2"), sep = ": |-| or ") %>%
    mutate(across(-name, as.integer)) %>%
    rowwise() %>%
    mutate(
      allowed = list(c(s1:e1, s2:e2)), .keep = "unused"
    )

  tickets = d[2:3] %>%
    map(~strsplit(.x[-1], ",") %>% map(as.integer)) %>%
    tibble(
      kind = c("yours", "nearby"),
      vals = .
    ) %>%
    unnest_longer(vals, indices_include = TRUE) %>%
    relocate(kind, vals_id) %>%
    rename(id = vals_id)

  bind_cols(
    tickets,
    rules %>%
      pivot_wider(names_from = name, values_from = allowed)
  ) %>%
    rowwise() %>%
    mutate( across(
      -(kind:vals),
      ~ list(vals %in% .x)
    ) ) %>%
    pivot_longer(
      -(kind:vals),
      names_to = "rule",
      values_to = "present"
    )
}

parse("day16/test.txt")

## Part 1

f = function(file) {

  res = parse(file) %>%
    group_by(kind, id, vals) %>%
    summarize(
      valid = list(reduce(present, `|`)),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      bad_vals = list(vals[!valid]),
    ) %>%
    ungroup()

  list(
    res,
    pull(res, bad_vals) %>%
      unlist() %>%
      sum()
  )
}

f("day16/test.txt")[[2]]
f("day16/input.txt")[[2]]



## Part 2

g = function(file) {
  invalid = f(file)[[1]] %>%
    mutate(bad = map_int(bad_vals, length) != 0) %>%
    filter(bad) %>%
    select(kind, id)

  d = anti_join(
    parse(file),
    invalid,
    by = c("kind", "id")
  )

  rule_locs = rep(NA, length(d$vals[[1]]))

  repeat {
    sub = is.na(rule_locs)

    res = d %>%
      mutate(
        present = purrr::map(present, ~ .x & sub)
      ) %>%
      group_by(rule) %>%
      summarize(
        possible = list(reduce(present, `&`)),
        .groups = "drop"
      ) %>%
      mutate(
        n = purrr::map_int(possible, sum)
      ) %>%
      filter(n == 1) %>%
      mutate(
        i = purrr::map_int(possible, which)
      ) %>%
      filter(!rule %in% rule_locs) %>%
      select(rule, i) %>%
      distinct()

    if (nrow(res) == 0)
      break

    rule_locs[res$i] = res$rule
  }
  rule_locs

  your_vals = d %>%
    filter(kind == "yours") %>%
    pull(vals) %>%
    .[[1]]

  setNames(your_vals, rule_locs)
}



g("day16/test.txt")
g("day16/test2.txt")

g("day16/input.txt") %>%
  {.[ grepl("^departure", names(.)) ]} %>%
  prod()


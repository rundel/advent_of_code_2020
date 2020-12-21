library(tidyverse)

parse = function(file) {
  file %>%
    read_lines() %>%
    tibble(lines = .) %>%
    separate(lines, c("ingredients", "allergens"), sep = " \\(contains ") %>%
    mutate(
      ingredients = str_split(ingredients, " "),
      allergens = str_remove(allergens, "\\)") %>%
        str_split(", ")
    ) %>%
    unnest_longer(allergens)
}

find_allergens = function(d) {
  d = d %>%
    group_by(allergens) %>%
    summarise(
      ingredients = list(reduce(ingredients, intersect)),
      .groups = "drop"
    ) %>%
    mutate(
      n = map_int(ingredients, length)
    )

  while(!all(d$n == 1)) {
    solved = d %>%
      filter(n == 1) %>%
      pull(ingredients) %>%
      unlist()

    d = d %>%
      mutate(
        ingredients = map(
          ingredients,
          function(i) {
            if (length(i) == 1) {
              i
            } else {
              setdiff(i, solved)
            }
          }
        )
      ) %>% mutate(
        n = map_int(ingredients, length)
      )
  }

  d %>%
    pull(ingredients) %>%
    unlist() %>%
    setNames(d$allergens)
}

f = function(file) {

  d = parse(file)

  has_allergens = find_allergens(d)

  d %>%
    distinct(ingredients) %>%
    pull(ingredients) %>%
    unlist() %>%
    {.[!. %in% has_allergens]}
}

f("day21/test.txt") %>% length()
f("day21/input.txt") %>% length()


## Part 2

g = function(file) {
  parse(file) %>%
      find_allergens() %>%
      { .[sort(names(.))] } %>%
      paste(collapse = ",")
}

g("day21/test.txt")
g("day21/input.txt")

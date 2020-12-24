library(tidyverse)

moves = list(
  e  = c( 4,  0),
  se = c( 2, -3),
  sw = c(-2, -3),
  w  = c(-4,  0),
  nw = c(-2,  3),
  ne = c( 2,  3)
)

file = "day24/test.txt"

parse = function(file) {
  read_lines(file) %>%
    str_match_all("sw|se|nw|ne|e|w") %>%
    map(c)
}

f = function(file) {

  d = parse(file) %>%
    tibble(lines = .) %>%
    mutate(
      tile = map(
        lines,
        function(l) {
          l %>% map(~moves[[.x]]) %>% reduce(`+`)
        }
      )
    )

  d %>%
    group_by(tile) %>%
    summarize(
      color = ifelse(n() %% 2 == 1, "black", "white"),
      .groups = "drop"
    )
}

f("day24/test.txt") %>% count(color)
f("day24/input.txt") %>% count(color)


## Part 2

find_neighbors = function(tile) {
  map(moves, ~tile + .x) %>%
    setNames(NULL)
}

pad = function(state) {
  state = filter(state, color == "black")

  state %>%
    filter(color == "black") %>%
    transmute(
      tile = map(tile, find_neighbors)
    ) %>%
    unnest_longer(tile) %>%
    distinct() %>%
    full_join(
      state, ., by = "tile"
    ) %>%
    mutate(
      color = ifelse(is.na(color), "white", color)
    )
}

update = function(state) {

  state = pad(state)

  state %>%
    mutate(
      nbs = map(tile, find_neighbors)
    ) %>%
    unnest_longer(nbs) %>%
    left_join(
      .,
      state %>% rename(nbs = tile, nbs_color = color),
      by = "nbs"
    ) %>%
    group_by(tile) %>%
    summarise(
      color = unique(color),
      n_black = sum(nbs_color == "black", na.rm=TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      color = case_when(
        color == "black" & (! n_black %in% 1:2) ~ "white",
        color == "white" & (n_black == 2) ~ "black",
        TRUE ~ color
      )
    ) %>%
    filter(color == "black") %>%
    select(-n_black)
}

g = function(file, n = 100) {
  state = f(file)

  pb = progress::progress_bar$new(
    format = "[:bar] :percent eta: :eta",
    total = n
  )

  map_dfr(
    seq_len(n),
    function(i) {
      pb$tick()
      state <<- update(state)
      mutate(state, day = i)
    }
  )
}

d = g("day24/test.txt", 10)
d %>% count(day) %>% tail(n=1)


d2 = g("day24/test.txt", 100)
d2 %>% count(day) %>% tail(n=1)

d3 = g("day24/input.txt", 100)
d3 %>% count(day) %>% tail(n=1)


## Bonus - plot the patterns

library(hexbin)

plot_tiles = function(d) {
  d %>%
    mutate(
      tile = map(tile, ~setNames(.x, c("x", "y")))
    ) %>%
    unnest_wider(tile) %>%
    ggplot(aes(x = x, y = y)) +
      geom_hex(binwidth = c(2,3), show.legend = FALSE) +
      guides(c = NA) +
      #facet_wrap(~day)
      gganimate::transition_states(day, 0, 1) +
      #gganimate::ease_aes('sine-in-out') +
      labs(title = 'Day: {closest_state}')
}

plot_tiles(d)

plot_tiles(d2)

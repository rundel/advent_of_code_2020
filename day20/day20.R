library(tidyverse)

parse = function(file, n=10) {

  read_file(file) %>%
    str_trim() %>%
    str_split("\n\n") %>%
    .[[1]] %>%
    tibble::tibble(d = .) %>%
    separate(d, c("id", "tile"), sep = ":\n") %>%
    mutate(
      id = str_remove(id, "Tile ") %>% as.numeric(),
      tile = map(tile, ~str_split(.x, "\n", simplify=TRUE)) %>%
        map(~str_split(.x, "", simplify=TRUE)),
      edges = map(
        tile,
        ~ list(L = .x[,1], R = .x[,n], T = .x[1,], B = .x[n,],
               Lr = rev(.x[,1]), Rr = rev(.x[,n]),
               Tr = rev(.x[1,]), Br = rev(.x[n,]))
      ),
      edges = map(edges, ~ map_chr(.x, paste, collapse=""))
    )
}

get_nbs = function(d) {
  d %>%
    select(-tile) %>%
    expand_grid(x=., y=.) %>%
    do.call(cbind, .) %>%
    setNames(c("x_id", "x_edges", "y_id", "y_edges")) %>%
    filter(x_id != y_id) %>%
    mutate(
      matches = map2(x_edges, y_edges, ~ .x %in% .y),
      n_matches = map_int(matches, sum),
    ) %>%
    filter(n_matches != 0) %>%
    group_by(x_id) %>%
    summarize(
      n = n(),
      matches = list(c(matches)),
      nbs = list(c(y_id)),
      .groups = "drop"
    ) %>%
    rename(id = x_id)
}

## Part 1

f = function(file) {

  d = parse(file)

  d %>%
    get_nbs() %>%
    filter(n == 2) %>%
    pull(id)
}

f("day20/test.txt") %>% prod() %>% formatC(digits=16)
f("day20/input.txt") %>% prod() %>% formatC(digits=16)

## Part 2


position = function(nb) {
  n = sqrt(nrow(nb))

  sol = matrix(NA, n, n)

  for(i in seq_len(n)) {
    for(j in seq_len(n)) {
      n_nb = 2
      if (i != 1 & i != n) n_nb = n_nb +1
      if (j != 1 & j != n) n_nb = n_nb +1

      nb_id = map2_dbl(
        i + c(-1,0,0,1), j + c(0,-1,1,0),
        ~ {if (.x == 0 | .y == 0 | .x == n+1 | .y == n+1) NA else sol[.x, .y]}
      )
      nb_id = nb_id[!is.na(nb_id)]

      if (i==1 & j == 1) {
        new_id = nb %>%
          filter(n == n_nb) %>% pull(id) %>%
          .[1]
      } else {
        new_id = nb %>%
          mutate(
            has_nb = map_lgl(nbs, ~ all(nb_id %in% .x))
          ) %>%
          filter(n == n_nb, has_nb) %>%
          pull(id) %>%
          .[1]
      }


      nb = filter(nb, id != new_id)

      sol[i,j] = new_id

      #cat(i,j, "\n")
      #print(sol)
    }
  }

  sol
}

flip_ver = function(x) {
  x[seq(nrow(x), 1), ]
}

flip_hor = function(x) {
  x[,seq(nrow(x), 1)]
}

turn_right = function(x) {
  t(x) %>% flip_hor()
}

turn_left = function(x) {
  t(x) %>% flip_ver()
}

get_edges = function(x) {
  n = nrow(x)
  list(L = x[,1], R = x[,n], T = x[1,], B = x[n,]) %>%
    map(paste, collapse = "")
}

fix_init_rotation = function(x, mat_pos) {
  dm = dim(x)
  x = case_when( # Move to get R, D
    all( mat_pos == c("T","R") ) ~ turn_right(x),
    all( mat_pos == c("R","T") ) ~ flip_ver(x),
    all( mat_pos == c("T","L") ) ~ turn_right(x) %>% flip_ver(),
    all( mat_pos == c("L","T") ) ~ turn_right(x) %>% turn_right(),
    all( mat_pos == c("B","L") ) ~ turn_left(x),
    all( mat_pos == c("L","B") ) ~ flip_hor(x),
    all( mat_pos == c("B","R") ) ~ turn_right(x) %>% flip_hor(),
  )
  dim(x) = dm
  x
}


try_rotations = function(x, constraints = list()) {
  if (length(constraints) == 0)
    stop("need constraints")

  for(flip in 1:2) {
    for(rot in 1:4) {
      edges = get_edges(x)

      check = map_lgl(
        names(constraints),
        ~ constraints[[.x]] == edges[[.x]]
      )
      if (all(check))
        return(x)

      #print(x)
      #print(check)

      x = turn_right(x)
    }
    x = flip_hor(x)
  }

  stop("Failed to find a match")
}


trim_tile = function(x) {
  n = nrow(x)
  x[-c(1,n),][,-c(1,n)]
}

trim = function(res) {
  map(
    res,
    ~map(.x, trim_tile)
  )
}


collapse = function(res, pad = NULL) {
  map(
    res,
    function(row) {
      map(
        row,
        ~ cbind(.x, pad)
      ) %>%
        do.call(cbind, .) %>%
        rbind(pad)
    }
  ) %>%
    do.call(rbind, .)
}



find_monster = function(image) {

  # Monster Pattern
  # "                  # "
  # "#    ##    ##    ###"
  # " #  #  #  #  #  #   "

  mons_x = c(18, 0, 5, 6, 11, 12, 17, 18, 19, 1, 4, 7, 10, 13, 16)
  mons_y = c( 0, 1, 1, 1,  1,  1,  1,  1,  1, 2, 2, 2,  2,  2,  2)

  for(flip in 1:2) {
    for(rot in 1:4) {

      mons_locs = list()

      for(i in seq_len(nrow(image) - max(mons_y))) {
        for(j in seq_len(ncol(image) - max(mons_x))) {
          vals = map2_chr(mons_x, mons_y, ~ {image[i+.y, j + .x]})
          if (all(vals == "#"))
            mons_locs = c(mons_locs, list(c(i,j)))
        }
      }

      if (length(mons_locs) != 0)
        return(mons_locs)

      image = turn_right(image)
    }
    image = flip_ver(image)
  }

  stop("Didn't find any monsters")
}



g = function(file) {
  d = parse(file)

  nb = d %>%
    left_join(., get_nbs(.), by = "id") %>%
    arrange(n) %>%
    mutate(
      which = map2(
        matches, edges,
        function(m, e) {
          map(m, ~ names(e[1:4][.x[1:4]])) # 1:4 removes the reversed strings
        }
      ),
      which = map2(which, nbs, ~ setNames(.x, .y))
    )

  pos = position(nb)

  i = 1; j = 1
  res = list()

  for(i in seq_len(nrow(pos))) {
    for(j in seq_len(ncol(pos))) {

      cur_id = pos[i, j]
      cur = nb %>% filter(id == cur_id)

      if (is.null(res[i][[1]]))
        res[[i]] = list()


      if (i == 1 & j == 1) {
        ids = as.character( c(pos[1,2], pos[2,1]) )
        cur_which = cur$which %>% unlist() %>% .[ids]

        res[[i]][[j]] = fix_init_rotation(cur$tile[[1]], cur_which)

        next
      }

      if (j == 1) {
        cons = res[[i-1]][[j]] %>% get_edges() %>% {list(T = .$B)}
      } else {
        cons = res[[i]][[j-1]] %>% get_edges() %>% {list(L = .$R)}
      }

      res[[i]][[j]] = try_rotations(cur$tile[[1]], cons)
    }
  }

  image = res %>%
    trim() %>%
    collapse()

  mons = find_monster(image)

  sum(image == "#") - 15 * length(mons)
}

g("day20/test.txt")
g("day20/input.txt")

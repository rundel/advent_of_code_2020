library(tidyverse)

transform = function(x, subject_number, modulus = 20201227) {
  (x * subject_number) %% modulus
}

find_loop_size = function(
  public_key,
  subject_number = 7,
  modulus = 20201227
) {
  value = 1
  i = 0
  while (value != public_key) {
    value = transform(value, subject_number, modulus)
    i = i+1
  }

  i
}

find_enc_key = function(pub_key, loop_size) {
  reduce(
    seq_len(loop_size+1),
    function(x, y, subject_number) {
      transform(x, subject_number)
    },
    subject_number = pub_key
  )
}


f = function(card_pk, door_pk) {
  card_ls = find_loop_size(card_pk)
  door_ls = find_loop_size(door_pk)

  card_ek = find_enc_key(card_pk, door_ls)
  door_ek = find_enc_key(door_pk, card_ls)

  c(card_ek = card_ek, door_ek = door_ek)
}


# Test
f(card_pk = 5764801, door_pk = 17807724)


# Input
f(card_pk = 8458505, door_pk = 16050997)

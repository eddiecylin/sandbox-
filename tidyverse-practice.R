## tidyverse practice

library("tidyverse")
library("repurrrsive")

sw_people <- sw_people %>%
  set_names(map_chr(sw_people, 'name'))
sw_people

map_int(sw_people, ~length(.x[["starships"]]))

map_chr(sw_people, ~.x[["eye_color"]])
map_lgl(sw_people, ~.x[["gender"]] == "male")
map_chr(sw_people,  2)
# starwars movie with most characters
# solution 1
num_char <- map_int(sw_films, ~length(.x[["characters"]]))
max_pos <- which.max(num_char)
paste0('the move with most number of character is ', film_name[max_pos])
# solution 2
map(sw_films, "characters") %>% 
  map_int(length) %>%
  set_names(map_chr(sw_films, "title")) %>% 
  sort()
# which species has the most eye colors
map_chr(sw_species, "eye_colors") %>% 
  strsplit(",") %>% 
  map_int(length) %>%
  set_names(map_chr(sw_species, "name")) %>% 
  sort()
#  which planet has the most unknown
map_int(sw_planets, ~ map_lgl(.x, ~"unknown" %in% .x) %>% sum()) %>%
  set_names(map_chr(sw_planets, "name")) %>% 
  sort(decreasing = TRUE)
## tidyverse practice
## references (from Charlotte Wickham):
## https://www.youtube.com/watch?v=b0ozKTUho0A
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
# practice with gap_split by drawing and saving ggplots
gap_split_small <- gap_split[1:10]
countries <- names(gap_split_small)
plots <- map2(.x = gap_split_small, .y = countries, 
     ~ ggplot(.x, aes(year, lifeExp)) + geom_line() + geom_point() + ggtitle(.y)
     )
## to auto save each plot in the process
map2(.x = countries, .y = plots,
        .f = ~ ggsave(filename = paste0(.x, ".pdf"), plot = .y))
# practice with list column
## A useful lookup table -----------------------------------------------
film_number_lookup <- map_chr(sw_films, "url") %>%
        map(~ stringr::str_split_fixed(.x, "/", 7)[, 6]) %>%
        as.numeric() %>%
        set_names(map_chr(sw_films, "url"))
people_tbl <- tibble(
        name = sw_people %>% map_chr("name"),
        films = sw_people %>% map("films"),
        height = sw_people %>% map_chr("height") %>%
                readr::parse_number(na = "unknown"),
        species = sw_people %>% map_chr("species", .null = NA_character_)
)
# Turning parts of our list to a tibble ---------------------------------
people_tbl$films
# Use map with mutate to manipulate list columns
people_tbl <- people_tbl %>%
        mutate(
                film_numbers = map(films,
                                   ~ film_number_lookup[.x]),
                n_films = map_int(films, length)
        )
people_tbl %>% select(name, film_numbers, n_films)
# get the number of films within the column of film_numbers -------------
people_tbl %>% 
        mutate(film_list = map_chr(.x = film_numbers, 
                                   .f = ~ paste(sort(.x), collapse =  ", "))) %>% 
        select(name, film_list)


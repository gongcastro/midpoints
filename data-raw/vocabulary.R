## code to prepare `vocabulary` dataset goes here
vocabulary <- sim_data(n_words = 10, n_id = 100, x0 = 20, ) |>
    select(id, lp, age, word, y)

usethis::use_data(vocabulary, overwrite = TRUE)

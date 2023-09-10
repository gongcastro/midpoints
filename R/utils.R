# function to simulate binary data following logistic growth
sim_data <- function(x = seq(0, 35, 1),
                     n_id = 10,
                     k = c(3, 1),
                     x0 = c(23, 15),
                     n_words = 1,
                     word_sd_k = 3,
                     word_sd_x0 = 5) {

    # participant data
    id <- as.factor(1:n_id)
    k <- rnorm(n_id, k, 0.05) # slope
    x0 <- rnorm(n_id, x0, 0.01) # mid-point

    # word data
    word <- 1:n_words
    word_k_adj <- runif(n_words, 0, word_sd_k)
    word_x0_adj <- rnorm(n_words, 0, word_sd_x0)
    words_df <- data.frame(word, word_k_adj, word_x0_adj)

    # data participant and word data
    expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
    dataset <- expand.grid.df(data.frame(id, k, x0),
                              as.data.frame(x),
                              words_df)

    # generate data
    k <- k + word_k_adj
    x0 <- x0 + word_x0_adj
    p <- plogis(dataset$x, dataset$x0, dataset$k)
    y <- rbinom(n = nrow(dataset), size = 1, prob = p)

    dataset[, c("k", "x0", "p", "y")] <- cbind(k, x0, p, y)
    dataset <- tibble::as_tibble(dataset)
    dataset <- dataset[order(dataset$id, dataset$word, dataset$x), , drop = FALSE]
    dataset <- dataset[, c("word", "id", "x", "y", "p", "x0", "k")]

    return(dataset)
}

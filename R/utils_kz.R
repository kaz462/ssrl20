## A collection of utility functions 

suppressPackageStartupMessages({
  library(dplyr);
  library(tibble);
  library(tidytext);
  library(ggplot2);
  library(wordcloud);
})

related_hashtags_freq <- function(tw, upper = FALSE, thres = 0) {
  if (upper) {
    tags.all <- toupper(unlist(tw$hashtags))    
  } else {
    tags.all <- tolower(unlist(tw$hashtags))  
  }
  tags.freq <- table(tags.all)
  return (tags.freq[tags.freq > thres])
}

clean_tweets <- function(tw) {
  tibble(txt = tw$text) %>%
    unnest_tokens(
      output = word,
      input = txt,
      token = "tweets",
      strip_url = T
    ) %>%
    anti_join(get_stopwords(), by = "word") %>%
    filter(!substr(word, 1, 1) %in% c("#", "@"))  # eliminate hashtags and mentions
}


plot_word_freq <- function(w, ntop = 15) {
  w %>%
    count(word, sort = T) %>%
    top_n(ntop, wt = n) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    labs(x = "", y = "Number of appearances") +
    coord_flip()
}


plot_density <- function(data, xname, bins = 30, dens.adjust = 4, fit = "gaussian") {
  if (fit == "gaussian") {
    fitfun = dnorm
  } 
  ggplot(data = data, aes(x = data[[xname]])) +
    geom_histogram(aes(y = ..density..),
                   bins = bins,
                   colour = "grey",
                   fill = "white") +
    geom_density(
      alpha = 0.2,
      adjust = dens.adjust,
      fill = "#FF6666",
      aes(colour = "est. density")
    ) +
    stat_function(
      fun = fitfun,
      linetype = "dashed",
      aes(colour = fit),
      args = list(mean = mean(data[[xname]]),
                  sd = sd(data[[xname]]))
    ) +
    scale_colour_manual("", values = c("red", "blue"))
}

## A collection of utility functions 

suppressPackageStartupMessages({
  library(dplyr);
  library(tibble);
  #library(tidytext);
  library(ggplot2);
  library(wordcloud);
  library(stringr)
  library(tokenizers)
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

get_stopwords <- function() {
  stopwords <- stopwords::stopwords()
  stopwords.with.punct <- stopwords[str_detect(stopwords, "'")]
  stopwords.special <- c("&amp;")
  c(stopwords, str_replace(stopwords.with.punct, "'", "’"), stopwords.special)
}

clean_tweets <- function(txt) {
  tokenize_tweets(txt, stopwords = get_stopwords(), strip_url = T, strip_punct = T, strip_special = T)
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

sourceCpp("src/hashtag_co_occurrence.cpp")
get_co_occurrence_matrix <- function(hashtag.list,
                                     include = hashtag.list %>% unlist() %>% unique()) {
  adj <- getCoOccurrenceMatrix(hashtag.list, include)
  colnames(adj) <- include
  rownames(adj) <- include
  return(adj)
}

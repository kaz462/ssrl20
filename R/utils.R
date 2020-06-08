## A collection of utility functions 

suppressPackageStartupMessages({
  library(dplyr);
  library(tibble);
  library(tidytext);
  library(ggplot2);
  library(wordcloud);
})


sdaway <- function(x) {
  u <- mean(x); s <- sd(x)
  c(mean = u, one.sd = u + s, two.sd = u + 2*s)
}


botcheck_roc <- function(b, u, n.step = 100) {
  cutoff <- seq(0, 1, by = 1/n.step)
  tpr <- sapply(cutoff, function(x){sum(b > x) / length(b)})  
  fpr <- sapply(cutoff, function(x){sum(u > x) / length(u)})
  data.frame(tpr = tpr, fpr = fpr)
}


related_hashtags_freq <- function(tw, upper = FALSE, thres = 0) {
  if (upper) {
    tags.all <- toupper(unlist(tw$hashtags))    
  } else {
    tags.all <- tolower(unlist(tw$hashtags))  
  }
  tags.freq <- table(tags.all)
  return (tags.freq[tags.freq > thres])
}


get_vars_description <- function() {
  source("R/feat_def.R")
  get_vars_description_all()
}


timeline_feature_simp <- function(tl) {
  if (nrow(tl) == 0) {
    return(tl)
  } else {
    vars <- c("user_id", "text", "is_retweet", "retweet_count", "description", 
              "location", "name", "created_at", "is_quote", "favorite_count",
              "favourites_count", "source", "verified", "account_created_at", "statuses_count",
              "followers_count", "friends_count", "listed_count", "screen_name")
    subset(tl, select = vars)
  }
}


tag_bots <- function(tw, cri.fast = 0.95, cri.slow = 0.9, cri.ntw = 13) {
  tw %>% add_column(is.bot = (tw$ntweets > cri.ntw) 
                    & (tw$pbots2 > cri.slow | tw$pbots1 > cri.fast))
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

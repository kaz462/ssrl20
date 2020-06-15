library(ggplot2)
library(igraph)
library(Rcpp)
library(readr)
library(httr)
library(tidyverse)  
library(rtweet)
library(DT)
library(shiny)

source("R/utils.R")  

# tw <- search_tweets("(crisis AND management AND learning) OR
# (COVID AND management AND learning) OR
# (pandemic AND management AND learning) OR
# (course AND migration) OR
# (COVID AND migration) OR
# (pandemic AND migration) OR
# (COVID AND education) OR
# (pandemic AND education) OR
# (remote AND instruction) OR
# (emergency AND instruction) OR
# (COVID AND migrate AND online) OR
# (COVID AND pivot AND online) OR
# (COVID AND higher AND education AND online) OR
# (COVID AND higher AND education)", lang = "en", include_rts = T, n = 10000, retryonratelimit = TRUE)
# saveRDS(object = tw, file = "data/tw_31MAY2020.rds")

if (file.exists("data/tw_31MAY2020.rds")) {
  tw <- readRDS("data/tw_31MAY2020.rds")
} else {
  tw <- readRDS(
    url("https://ssrl20-online-teaching.s3.ca-central-1.amazonaws.com/tw_31MAY2020.rds")
  )
}

users <- users_data(tw)
tw.variables <- names(tw)
users.variables <- names(users)
unique.tw <- tw$text %>% 
  plain_tweets() %>% 
  unique() %>% 
  length()
unique.accounts <- users$user_id %>% 
  unique() %>% 
  length()

# plot1
source <- table(tw$source)
source <- tibble(var = names(source), freq = source) %>% arrange(desc(freq)) 
others <- tibble(var = "Others", freq = sum(source[-(1:8),]$freq))
source <- rbind(source[1:8,], others)

# hashtag
tw$hashtags <- lapply(tw$hashtags, toupper)
ht.df <- tibble(word = tw$hashtags %>% unlist() %>% na.omit())
ht.top <- ht.df %>% count(word, sort = T)
ht.top

# keywords
words.list <- clean_tweets(tw$text) 
words.df <- tibble(word = unlist(words.list)) 
words.top <- words.df %>% count(word, sort = T)
words.top

shinyServer(function(input, output) {
  
  #creating the valueBoxOutput content  
  output$value1 <- renderValueBox({    
    valueBox(formatC(nrow(tw), format="d", big.mark=','),
             "Total number of tweets",
             icon = icon("stats",lib='glyphicon'),
             color = "purple")})  
  output$value2 <- renderValueBox({     
    valueBox(      
      formatC(unique.tw, format="d", big.mark=','),
      'Number of unique Tweets',
      icon = icon("menu-hamburger",lib='glyphicon'),
      color = "green")})
  output$value3 <- renderValueBox({    
    valueBox(      
      formatC(unique.accounts, format="d", big.mark=','),
      'Number of unique accounts',
      icon = icon("menu-hamburger",lib='glyphicon'),
      color = "yellow")})
  #creating the plotOutput content  
  output$DivicePie <- renderPlot({
    source %>%
      ggplot(aes(x = "", y = freq, fill = var)) +
      geom_col(width = 1) +
      coord_polar(theta = "y", direction = 1) + 
      labs(x = NULL, y = NULL, fill = "Utility") +
      labs(title="Devices/App used distribution") + 
      theme_minimal()
  })
  output$TimeSeries <- renderPlot({    
    ## plot time series of tweets
    tw %>%
      ts_plot("3 hours") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = "Frequency of Twitter statuses from past 1-9 days",
        subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
      )   
  })
  output$geo <- renderPlot({    
    countries <- maps::map("world", namesonly = T, plot = F)
    maps::map("world", region = countries[-grep("Antarctica", countries)], lwd = .25)
    with(lat_lng(tw), points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))    
  })
  output$hash_cloud <- renderPlot({    
    names(ht.top) <- c("word", "n")
    words.count <- ht.top %>% arrange(desc(n))
    wordcloud(words = words.count$word, freq = words.count$n, 
              random.order = F, max.words = 200, rot.per = 0.15,
              colors = brewer.pal(8, "Dark2"))  
  })
  output$hash_freq <- renderPlot({    
    ht.df %>% plot_word_freq(20)
  })
  
  output$key_cloud <- renderPlot({    
    words.count <- words.df %>% count(word, sort = T)
    wordcloud(words = words.count$word, freq = words.count$n, 
              random.order = F, max.words = 200, rot.per = 0.15,
              colors = brewer.pal(8, "Dark2"))
  })
  output$key_freq <- renderPlot({    
    words.df %>% plot_word_freq(20)
  })  
  
  output$net1 <- renderPlot({    
    ht.adj <- get_co_occurrence_matrix(tw$hashtags, ht.top$word[1:20])
    ht.net <- graph_from_adjacency_matrix(ht.adj, mode = "undirected", weighted = T, diag = F) 
    
    graph_attr(ht.net, "layout") <- layout_in_circle
    plot(
      ht.net,
      vertex.shape = "none",
      edge.color = "orange",
      edge.width = E(ht.net)$weight / 10,
      vertex.label.dist = 0,
      vertex.label.color = "steel blue",
      vertex.label.font = 1.2,
      vertex.label.cex = .4, 
      vertex.color = "gray50"
    )
  })  
  
  
  output$net2 <- renderPlot({
    kw.adj <- get_co_occurrence_matrix(words.list, words.top$word[1:20])
    kw.net <- graph_from_adjacency_matrix(kw.adj, mode = "undirected", weighted = T, diag = F) 

    graph_attr(kw.net, "layout") <- layout_in_circle
    plot(
      kw.net,
      vertex.shape = "none",
      edge.color = "orange",
      edge.width = E(kw.net)$weight / 6000,
      vertex.label.dist = 0,
      vertex.label.color = "steel blue",
      vertex.label.font = 1.2,
      vertex.label.cex = .4, 
      vertex.color = "gray50"
    )
  })    
  

})
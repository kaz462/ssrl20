source("R/utils_kz.R")
library(readr)
library(httr)
library(tidyverse)  
library(rtweet)
library(DT)
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
tw <- readRDS("data/tw_31MAY2020.rds")
users <- users_data(tw)
tw_variables <- names(tw)
users_variables <- names(users)
unique_tw <- tw$text %>% 
  plain_tweets() %>% 
  unique() %>% 
  length()
unique_accounts <- users$user_id %>% 
  unique() %>% 
  length()

#plot1
source <- tw$source %>% table()
source <- data_frame(var = names(source), freq = source) %>% arrange(desc(freq)) 
others <- data_frame(var = "Others", freq = sum(source[-(1:8),]$freq))
source <- rbind(source[1:8,], others)
#hasgtag
ht.top <- tw %>%
  related_hashtags_freq(upper = T) %>%
  sort() %>%
  as.data.frame() 
##keywords
tw.words <- tw %>% clean_tweets() 
ht.top <- tw %>%
  related_hashtags_freq(upper = T) %>%
  sort() %>%
  as.data.frame() 
## list all hashtags ordered by frequency
top <- ht.top %>% arrange(desc(Freq))
top 


## list all hashtags ordered by frequency
tw.words <- tw %>% clean_tweets() 
topwords <- tw.words %>% count(word, sort = T) 
topwords

library(shiny)
library(shinydashboard)
ui <- dashboardPage(  dashboardHeader(),  dashboardSidebar(),  dashboardBody())
server <- function(input, output) { }
shinyApp(ui, server)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Twitter Data Dashboard")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(  sidebarMenu(    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),  
                                              menuItem("Hashtags", tabName = "Hashtags", icon = icon("hashtag")), 
                                              menuItem("Keywords", tabName = "Keywords", icon = icon("search")), 
                                              menuItem(" Cooccurrences", tabName = "net", icon = icon("connectdevelop")), 
                                              #menuItem("Popular Tweets", tabName = "Popular", icon = icon("fire")), 
                                              menuItem("Geovisualization", tabName = "geo", icon = icon("map-marker-alt")), 
                                              menuItem("File Download", tabName = "download", icon = icon("download")), 
                                              menuItem("Visit-us", icon = icon("send",lib='glyphicon'),              
                                                       href = "https://ssrl.usask.ca/")  ))

frow1 <- fluidRow(valueBoxOutput("value1")  ,valueBoxOutput("value2"),valueBoxOutput("value3"))
frow2 <- fluidRow(box(title = "Utility used to post the Tweet",status = "primary",
                      solidHeader = TRUE,collapsible = TRUE,plotOutput("DivicePie", height = "400px")),
                  box(title = "Twitter status (tweet) counts",status = "primary",
                      solidHeader = TRUE,collapsible = TRUE,plotOutput("TimeSeries", height = "400px")  ) )

hash1 <- fluidRow(box(title = "Would cloud for Hashtags",status = "primary",
                      solidHeader = TRUE,collapsible = TRUE,plotOutput("hash_cloud", height = "400px")),
                  box(title = "Frequency table of top 30 hashtags",status = "primary",
                      solidHeader = TRUE,collapsible = TRUE,plotOutput("hash_freq", height = "400px")  ) )
hash2 <- fluidRow(width=12, h2("The mtcars data"),
                      DT::dataTableOutput("hash_list"))## TO be modified

key1 <- fluidRow(box(title = "Would cloud for Keywords",status = "primary",
                      solidHeader = TRUE,collapsible = TRUE,plotOutput("key_cloud", height = "400px")),
                  box(title = "Frequency table of top 20 Keywords",status = "primary",
                      solidHeader = TRUE,collapsible = TRUE,plotOutput("key_freq", height = "400px")  ) )
key2 <- fluidRow(box(title = "List of all Keywords",status = "primary",
                      solidHeader = TRUE,collapsible = TRUE,plotOutput("key_list", height = "400px")) )


pop1 <- fluidRow(box(title = "Histogram of number of likes (>100)",status = "primary",
                     solidHeader = TRUE,collapsible = TRUE,plotOutput("pop_like1", height = "400px")),
                 box(title = "Histogram of number of likes (<100)",status = "primary",
                     solidHeader = TRUE,collapsible = TRUE,plotOutput("pop_like2", height = "400px")  ) )
pop2 <- fluidRow(box(title = "List of most-liked tweets",status = "primary",
                     solidHeader = TRUE,collapsible = TRUE,plotOutput("pop_like", height = "400px")) )
pop3 <- fluidRow(box(title = "Histogram of number of retweets (>5000)",status = "primary",
                     solidHeader = TRUE,collapsible = TRUE,plotOutput("pop_ret1", height = "400px")),
                 box(title = "Histogram of number of retweets (<5000)",status = "primary",
                     solidHeader = TRUE,collapsible = TRUE,plotOutput("pop_ret2", height = "400px")  ) )
pop4 <- fluidRow(box(title = "List of top retweets",status = "primary",
                     solidHeader = TRUE,collapsible = TRUE,plotOutput("key_ret", height = "400px")) )


geo <- fluidRow(column(width=12,
                       box(title = "Search by geo-location, tweets sent from different locations.",status = "primary",
                           solidHeader = TRUE,collapsible = TRUE,plotOutput("geo",  height ="530px" ))))

net1 <- fluidRow(box(title = "Hashtag cooccurrences network",status = "primary",
                     solidHeader = TRUE,collapsible = TRUE,plotOutput("net1", height = "400px")),
                 box(title = "Keywords cooccurrences network",status = "primary",
                     solidHeader = TRUE,collapsible = TRUE,plotOutput("net2", height = "400px")  ) )
download <- fluidPage(
  titlePanel('Twitter data download'),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("Tweets", "Users' info")),
      radioButtons("filetype", "File type:",
                   choices = c("csv", "rds")),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tableOutput('table')
    )
  )
)

# combine the two fluid rows to make the body
#body <- dashboardBody(frow1, frow2)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard", frow1, frow2),
    tabItem(tabName = "Hashtags", hash1),
    tabItem(tabName = "Keywords", key1),
    tabItem(tabName = "Popular", pop1, pop2),
    tabItem(tabName = "net", net1),
    tabItem(tabName = "geo", geo),
    tabItem(tabName = "download", download)
  )
)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')
# create the server functions for the dashboard  
server <- function(input, output) {   #some data summaries
  #creating the valueBoxOutput content  
  output$value1 <- renderValueBox({    
    valueBox(formatC(nrow(tw), format="d", big.mark=','),
             "Total number of tweets",
             icon = icon("stats",lib='glyphicon'),
             color = "purple")})  
  output$value2 <- renderValueBox({     
    valueBox(      
    formatC(unique_tw, format="d", big.mark=','),
    'Number of unique Tweets',
    icon = icon("menu-hamburger",lib='glyphicon'),
    color = "green")})
  output$value3 <- renderValueBox({    
    valueBox(      
    formatC(unique_accounts, format="d", big.mark=','),
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
    ht.top %>% top_n(30, wt = Freq) %>%
      ggplot(aes(tags.all, Freq)) +
      geom_col() +
      labs(x = "", y = "Number of appearances") +
      coord_flip()
  })
  
  output$key_cloud <- renderPlot({    
    words.count <- tw.words %>% count(word, sort = T)
    wordcloud(words = words.count$word, freq = words.count$n, 
              random.order = F, max.words = 200, rot.per = 0.15,
              colors = brewer.pal(8, "Dark2"))
  })
  output$key_freq <- renderPlot({    
    tw.words %>% 
      plot_word_freq(20)
  })  

  output$net1 <- renderPlot({    
    tw$hashtags <- lapply(tw$hashtags, toupper)
    adj <- getCoOccurrenceMatrix(tw$hashtags, tw$hashtags %>% unlist() %>% unique())
    names <- tw$hashtags %>% unlist() %>% unique()
    colnames(adj) <- names
    rownames(adj) <- names
    ht.net <- 
      adj %>%
      graph_from_adjacency_matrix(mode = "undirected", weighted = T, diag = F) 
    
    ht.net <- induced_subgraph(ht.net, as.vector(top$tags.all)[1:20])
    
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
    list <- rep(list(NA),nrow(tw))
    for (i in 1:nrow(tw)) {
      list[i] <- clean_tweets(tw[i,])
    }
    
    list <- lapply(list, toupper)
    adj <- getCoOccurrenceMatrix(list, list %>% unlist() %>% unique())
    names <- list %>% unlist() %>% unique()
    colnames(adj) <- names
    rownames(adj) <- names
    ht.net <- 
      adj %>%
      graph_from_adjacency_matrix(mode = "undirected", weighted = T, diag = F) 
    
    ht.net <- induced_subgraph(ht.net, as.vector(toupper(topwords$word)[1:20]))
    
    graph_attr(ht.net, "layout") <- layout_in_circle
    plot(
      ht.net,
      vertex.shape = "none",
      edge.color = "orange",
      edge.width = E(ht.net)$weight / 6000,
      vertex.label.dist = 0,
      vertex.label.color = "steel blue",
      vertex.label.font = 1.2,
      vertex.label.cex = .4, 
      vertex.color = "gray50"
    )
  })    
  

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )  
}

shinyUI(fluidPage(
  titlePanel('File download'),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("Tweets", "Users'info")),
      radioButtons("filetype", "File type:",
                   choices = c("csv", "rds")),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tableOutput('table')
    )
  )
))


#run/call the shiny app
shinyApp(ui, server)

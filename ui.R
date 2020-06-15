library(shiny)
library(shinydashboard)

# Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Twitter Data Dashboard") 

# Sidebar content of the dashboard
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),  
  menuItem("Hashtags", tabName = "Hashtags", icon = icon("hashtag")), 
  menuItem("Keywords", tabName = "Keywords", icon = icon("search")), 
  menuItem(" Cooccurrences Network", tabName = "net", icon = icon("connectdevelop")), 
  #menuItem("Popular Tweets", tabName = "Popular", icon = icon("fire")), 
  menuItem("Geovisualization", tabName = "geo", icon = icon("map-marker-alt")), 
  #menuItem("File Download", tabName = "download", icon = icon("download")), 
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
      #radioButtons("filetype", "File type:",
      #             choices = c("csv")),
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
    tabItem(tabName = "geo", geo)#,
    #tabItem(tabName = "download", download)
  )
)

# completing the ui part with dashboardPage
dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

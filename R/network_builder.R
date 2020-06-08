## Extract the sociogram from Twitter data.

library(dplyr)
library(foreach)

network_builder <- 
  function(tw,
           util,
           connect.by = c("mentions_user_id", "quoted_user_id",
                          "retweet_user_id", "reply_to_user_id"),
           load.adjList.path = NULL,
           save.adjList = FALSE) {
    
  if (!is.null(load.adjList.path)) {
    adj.list <- readRDS(load.adjList.path)
    if (!missing(tw)) {
      message("\nAdjacency list loaded, ignoring given data.\n")
    }
  } else {
    adj.list <- adjList_builder(tw, connect.by = connect.by)
    if (save.adjList) {
      saveRDS(adj.list, "data/adj_list.rds")
    }    
  }
  
  switch(
    util,
    statnet = network::network(
      x = adjList_to_edgeList(adj.list),
      matrix.type = "edgelist",
      directed = T,
      loops = T,
      ignore.eval = F,
      names.eval = 'weight'
    ),
    igraph = igraph::graph.data.frame(adjList_to_edgeList(adj.list)),
    stop("Not supported utility.")
  )
}

adjList_builder <- function(tw, connect.by) {
  
  usr.id <- tw$user_id %>% unique()
  
  if (length(usr.id) > 10000) {
    message("\nGrab a cup of coffee this could take a while...\n")
    flush.console()
  }

  adj.list <- foreach(u = usr.id) %do% {
    tw %>% 
      filter(user_id == u) %>% 
      select(connect.by) %>% 
      unlist() %>% 
      na.omit() %>% 
      table()
  } %>% rlist::list.flatten()
  
  single.idx <- foreach (i = 1L:length(adj.list)) %do% {
    if (length(adj.list[[i]]) == 0) {i}
  } %>% unlist()
  
  list("singleton" = usr.id[single.idx],
       "connected" = usr.id[-single.idx],
       "adj.list" = adj.list[-single.idx])
}

adjList_to_edgeList <- function(l) {
  
  node.out <- lapply(l$adj.list, as.data.frame) %>% data.table::rbindlist() 
  
  len <- lapply(l$adj.list, length) %>% unlist()
  node.in <- data.frame(id = l$connected, len = len) %>%
    apply(MARGIN = 1, FUN = function(r) {rep(r[1], times = r[2])}) %>% 
    unlist() %>% 
    as.data.frame() 
  
  edge.list <- node.in %>% bind_cols(node.out) 
  colnames(edge.list) <- c("V1", "V2", "weight")
  return(edge.list)
}

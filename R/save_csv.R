library(readr)
library(dplyr)
library(tidyverse)  
library(lubridate)

t2 <- read_rds("data/tw_100.rds")
t2[,3] <- as.POSIXct((as.matrix(t2[,3]))) %>% as.character()
t2 <- apply(t2, 2, as.character)
write.csv(t2, "data/tw_100.csv")


t3 <- read_rds("data/tw_31MAY2020.rds")
t3[,3] <- as.POSIXct((as.matrix(t3[,3]))) %>% as.character()
t3 <- apply(t3, 2, as.character)
write.csv(t3, "data/tw_31MAY2020.csv")






           
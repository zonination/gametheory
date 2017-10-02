# Load libraries, dataset, and theme
library(tidyverse)
library(gsheet)
library(lubridate)
game<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1lXig_oCubwzRTd4f5zxLQKIhmm8mOA3-tGWWjVio6iQ/")
source("z_theme.R")

# Name the game headers, set dates, convert all usernames to lower case
names(game)<-c("time", "ok", "user", "game",
               "choice.a", "choice.b", "choice.c", "choice.d", "choice.e")
game$time<-strptime(game$time, format="%m/%d/%Y %H:%M:%S")
game$user<-tolower(game$user)

# Run our subscripts
source("game_a.R")
source("game_b.R")
source("game_c.R")
source("game_d.R")
source("game_e.R")
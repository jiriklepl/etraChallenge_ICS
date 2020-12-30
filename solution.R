library(tidyverse)
library(here)
library(ez)
library(saccades)
require(ggplot2) 

library(papaja)

source("utils.R")

data_pth <- "data"

local_data_pth <- here(data_pth, "data", "009")
files <- dir(local_data_pth, pattern = ".*FreeViewing_Puzzle.*.csv") # get file names

if (exists("df_fix_puzzle", inherits = FALSE) == FALSE) {
  df_fix_puzzle <- files %>%
    map(read_data) %>% 
    reduce(rbind) %>% as_tibble()
}

if (exists("df_fix_waldo", inherits = FALSE) == FALSE) {
  df_fix_waldo <- files %>%
    map(read_data) %>% 
    reduce(rbind) %>% as_tibble()
}

mymax <- function(a,b)
  ifelse((a > b), a, b)

my_transform <- function(a, b, c, d)
  mymax(mymax(mymax(a,b),c), d)

get_graph <- function(stimulusId) {
  img.file <- file.path("data/images/PUZZLE", sprintf("puz%03d.bmp", stimulusId))
  img <- as.raster(bmp::read.bmp(img.file), max=255)
  left <- img[,1:(920/2)]
  left_check <- img[,1:(920/2)+5]
  left_check2 <- img[(function(x) ifelse(x > 0, x, 1))((1:630)-5),1:(920/2)]
  right_shift = 920/2 + 1
  right <- img[,right_shift:920]
  distance <- t((col2rgb(left) - col2rgb(right))**2 - (col2rgb(left) - col2rgb(left_check))**2 - (col2rgb(left_check) - col2rgb(left_check2))**2) %>% as_tibble() %>%
    mutate(red2 = ifelse(red > 0, red, 0)) %>%
    mutate(green2 = ifelse(green > 0, green, 0)) %>%
    mutate(blue2 = ifelse(blue > 0, blue, 0)) %>%
    mutate(D = my_transform(sqrt(red2), sqrt(green2), sqrt(blue2), sqrt(red2+green2+blue2))) %>% pull(D)
  graph <- t(sapply(which(distance >= quantile(distance, .98)), function(x) c((x %% 460)+1, (x %/% 460)+1))) %>%
    as_tibble() %>% 
    ggplot(aes(x = V1, y = 630-V2, xmin = 0, ymin = 0, xmax = 921, ymax = 630)) +
    annotation_raster(img, xmin = 0, ymin = 0, xmax = 921, ymax = 630) +
    geom_tile()

  # graph <- df_fix_puzzle %>%
  #   filter(stimulus_id == sprintf("puz%03d.csv", stimulusId)) %>%
  #   drop_na() %>%
  #   mutate(X = (LXpix + RXpix) / 2) %>%
  #   mutate(Y = (LYpix + RYpix) / 2) %>%
  #   mutate(Xhref = (LXhref + RXhref) / 2) %>%
  #   mutate(Yhref = (LYhref + RYhref) / 2) %>%
  #   arrange(Time) %>%
  #   ggplot(aes(x = X, y = 630-Y, xmin = 0, ymin = 0, xmax = 921, ymax = 630)) +
  #   annotation_raster(img, xmin = 0, ymin = 0, xmax = 921, ymax = 630) +
  #   geom_path()
  return (graph)
}



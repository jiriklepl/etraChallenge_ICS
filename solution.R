library(raster)
library(imager)
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

set_distance <- function(x, y, set) {
  if (x > 460) x = x - 460
  return (quantile((set %>% mutate(D = sqrt((X - x)**2 + (Y - y)**2)) %>% pull(D)),1/20))
}

first_test <- function(stimulusId) {
  fix_shift <- 0
  fix_vert_shift <- 0
  if (stimulusId==1) fix_shift <- 3
  if (stimulusId==4) {
    fix_shift <- 2
    fix_vert_shift <- -0
  }
  fix_shift1 <- ifelse(fix_shift < 0, 0, fix_shift)
  fix_shift2 <- ifelse(fix_shift > 0, 0, fix_shift)
  if (fix_vert_shift > 0) fix_vert_shift <- mapply(function(x) max(x-fix_vert_shift,1),1:630)
  else fix_vert_shift <- 631-mapply(function(x) max(x+fix_vert_shift,1),630:1)
  img.file <- file.path("data/images/PUZZLE", sprintf("puz%03d.bmp", stimulusId))
  img <- as.raster(bmp::read.bmp(img.file), max=255)
  left <- col2rgb(img[,(function(x) ifelse(x > 0, x, 1))(1:(920/2)-fix_shift2)])
  right_shift = 920/2 + 1
  right <- col2rgb(img[(function(x) ifelse(x > 0, x, 1))(1:630),right_shift:920 - fix_shift1])
  left_check <- ifelse((img[fix_vert_shift,right_shift:920 - fix_shift1] %>% as.raster() %>% as.cimg() %>% cannyEdges(alpha=.5, sigma=1) %>% as.raster()) == "#FFFFFF", 0,1) *
                ifelse((img[,1:(920/2)] %>% as.raster() %>% as.cimg() %>% cannyEdges(alpha=0.5, sigma=1) %>% as.raster()) == "#FFFFFF", 0,1)
  left <- left / mean(left)
  left <- (((left - min(left)) / (max(left) - min(left))) - .5)
  right <- right / mean(right)
  right <- (((right - min(right)) / (max(right) - min(right))) - .5)
  distance <- t((left - right)**2) %>% as_tibble() %>%
    mutate(red2 = ifelse(red > 0, red * left_check, 0)) %>%
    mutate(green2 = ifelse(green > 0, green * left_check, 0)) %>%
    mutate(blue2 = ifelse(blue > 0, blue * left_check, 0)) %>%
    mutate(D = sqrt(red2+green2+blue2)) %>% pull(D)
  graph1 <- t(sapply(which(distance >= quantile(distance, .97)), function(x) c(x, distance[x]))) %>%
    as_tibble() %>% 
    mutate(X = (V1 %% 460)+1) %>%
    mutate(Y = ((V1 %/% 460)+1))
    # %>% ggplot(aes(x = X, y = 630-Y, fill = V2, xmin = 0, ymin = 0, xmax = 921, ymax = 630)) +
    # annotation_raster(img, xmin = 0, ymin = 0, xmax = 921, ymax = 630) +
    # geom_tile()

  graph2 <- df_fix_puzzle %>%
    filter(stimulus_id == sprintf("puz%03d.csv", stimulusId)) %>%
    drop_na() %>%
    mutate(X = (LXpix + RXpix) / 2) %>%
    mutate(Y = (LYpix + RYpix) / 2) %>%
    mutate(Xhref = (LXhref + RXhref) / 2) %>%
    mutate(Yhref = (LYhref + RYhref) / 2) %>%
    arrange(Time) %>% 
    rename(time = Time, trial = trial_id, x = LXpix, y = LYpix) %>% 
    do(detect.fixations(.)) %>%
    mutate(Z = mapply(function(x, y) set_distance(x, y, graph1), x, y)) %>%
    # ggplot(aes(x = x, y = 630-y, size=end-start, xmin = 0, ymin = 0, xmax = 921, ymax = 630)) +
    # annotation_raster(img, xmin = 0, ymin = 0, xmax = 921, ymax = 630) +
    # geom_point()
    mutate(F = end-start)

    
  return (cor(graph2 %>% pull(Z),graph2 %>% pull(F)))
}

second_test <- function(stimulusId) {
  graph2 <- df_fix_puzzle %>%
    filter(stimulus_id == sprintf("puz%03d.csv", stimulusId)) %>%
    drop_na() %>%
    mutate(X = (LXpix + RXpix) / 2) %>%
    mutate(Y = (LYpix + RYpix) / 2) %>%
    mutate(Xhref = (LXhref + RXhref) / 2) %>%
    mutate(Yhref = (LYhref + RYhref) / 2) %>%
    arrange(Time) %>% 
    rename(time = Time, trial = trial_id, x = LXpix, y = LYpix) %>% 
    do(detect.fixations(.)) %>%
    mutate(Z = sqrt(ifelse(x < 920/2, x, 920-x)**2 + ifelse(y < 630/2, y, 630-y)**2)) %>%
    # ggplot(aes(x = x, y = 630-y, size=end-start, xmin = 0, ymin = 0, xmax = 921, ymax = 630)) +
    # annotation_raster(img, xmin = 0, ymin = 0, xmax = 921, ymax = 630) +
    # geom_point()
    mutate(F = end-start)

  img.file <- file.path("data/images/PUZZLE", sprintf("puz%03d.bmp", stimulusId))

  return (cor(graph2 %>% pull(Z),graph2 %>% pull(F)))
}


do_first <- function() {
  results = c()
  for (i in 1:15) {
    results[i] <- first_test(i)
    message(paste(i/15,'%',''))
  }
  return (results)
}

do_second <- function() {
  results = c()
  for (i in 1:15) {
    results[i] <- second_test(i)
    message(paste(i/15,'%',''))
  }
  return (results)
}
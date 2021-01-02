library(raster)
library(imager)
library(tidyverse)
library(here)
library(ez)
library(saccades)
require(ggplot2) 

library(papaja)


data_pth <- "data"

load_files <- function(subject) {
  local_data_pth <- here(data_pth, "data", sprintf("%03d", subject))
  files <- dir(local_data_pth, pattern = ".*FreeViewing_Puzzle.*.csv") # get file names

  read_data <- function(pth) {
    
    df_tmp <- read_csv(file.path(local_data_pth, pth))
    
    # set participant_id, trial id and stimulus id
    parsed_name <- pth %>% str_split("_") %>% unlist()
    df_tmp <- df_tmp %>% 
      mutate(participant_id = parsed_name[1],
            trial_id = parsed_name[2],
            fv_fixation = parsed_name[3],
            task_type = parsed_name[4],
            stimulus_id = parsed_name[5])
    df_tmp %>% select(participant_id, trial_id, fv_fixation, task_type, stimulus_id, everything())
  }

  return (files %>%
    map(read_data) %>% 
    reduce(rbind) %>% as_tibble())
}

mymax <- function(a,b)
  ifelse((a > b), a, b)

my_transform <- function(a, b, c, d)
  mymax(mymax(mymax(a,b),c), d)

set_distance <- function(x, y, set) {
  if (x > 460) x = x - 460
  return (quantile((set %>% mutate(D = sqrt((X - x)**2 + (Y - y)**2)) %>% pull(D)),1/20))
}

first_test <- function(stimulusId, df_fix_puzzle) {
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

    
  return (graph2)
}

second_test <- function(stimulusId, df_fix_puzzle) {
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

  return (graph2)
}

third_test <- function(stimulusId, df_fix_puzzle) {
  graph2 <- df_fix_puzzle %>%
    filter(stimulus_id == sprintf("puz%03d.csv", stimulusId)) %>%
    drop_na() %>%
    mutate(X = (LXpix + RXpix) / 2) %>%
    mutate(Y = (LYpix + RYpix) / 2) %>%
    mutate(Xhref = (LXhref + RXhref) / 2) %>%
    mutate(Yhref = (LYhref + RYhref) / 2) %>%
    mutate(P = (LP + RP) / 2) %>%
    mutate(time = Time - min(Time))

  return (graph2)
}

do_first <- function(subject) {
  df_fix_puzzle <- load_files(subject)
  for (i in 1:16) {
    if (!(sprintf("puz%03d.csv", i) %in% df_fix_puzzle$stimulus_id))
      next
    result <- as.data.frame(first_test(i, df_fix_puzzle))
    if (exists('results'))
      results <- rbind(results, result)
    else
      results <- result
    cat("correlation in ", i, ": ", cor(result %>% pull(Z), result %>% pull(F)), '\n')
  }

  cat("total correlation: ", cor(results %>% pull(Z), results %>% pull(F)), '\n')

  return (results)
}

do_second <- function(subject) {
  df_fix_puzzle <- load_files(subject)
  for (i in 1:16) {
    if (!(sprintf("puz%03d.csv", i) %in% df_fix_puzzle$stimulus_id))
      next
    result <- as.data.frame(second_test(i, df_fix_puzzle))
    if (exists('results'))
      results <- rbind(results, result)
    else
      results <- result
    cat("correlation in ", i, ": ", cor(result %>% pull(Z), result %>% pull(F)), '\n')
  }

  cat("total correlation: ", cor(results %>% pull(Z), results %>% pull(F)), '\n')

  return (results)
}

do_third_two <- function(subject) {
  df_fix_puzzle <- load_files(subject)
  for (i in 1:16) {
    if (!(sprintf("puz%03d.csv", i) %in% df_fix_puzzle$stimulus_id))
      next
    result <- as.data.frame(third_test(i, df_fix_puzzle) %>% select(time, P))
    if (exists('results'))
      results <- rbind(results, result)
    else
      results <- result

    cat(cor(result$P, result$time**(1/2)), '\n')
    cat(paste(100 * i / 15, '%', ''), '\n')
  }

  return (cor(results$P, results$time**(1/2)))

  return (results)
}

do_third <- function(subject) {
  df_fix_puzzle <- load_files(subject)
  for (i in 1:16) {
    if (!(sprintf("puz%03d.csv", i) %in% df_fix_puzzle$stimulus_id))
      next
    result <- as.data.frame(third_test(i, df_fix_puzzle) %>% select(time, P))
    if (exists('results'))
      results <- rbind(results, result)
    else
      results <- result
      
    result$P2 <- 1.381e+03 + 5.332e-03*result$time
    ns <- order(result$P2)

    first <- result[ns,][1:(nrow(result) %/% 2), ]
    second <- result[ns,][(nrow(result) %/% 2 + 1):nrow(result), ]

    cat("t.test (", i, "): p.value = ", t.test(first$P, second$P)$p.value, '\n')
  }

  results$P2 <- 1.381e+03 + 5.332e-03*results$time
  ns <- order(results$P2)

  first <- results[ns,][1:(nrow(results) %/% 2), ]
  second <- results[ns,][(nrow(results) %/% 2 + 1):nrow(results), ]

  cat("t.test (final): p.value = ", t.test(first$P, second$P)$p.value, '\n')

  return (results)
}

participants <- c(9, 19, 22, 58, 59, 60, 62)

first_hyp <- function() {
  cat("here go correlations of lengths of fixations and their distance from the assumed closest target:", '\n')
  for (i in participants) {
    cat("participant ", i, ":", '\n')
    result <- as.data.frame(do_first(i))
    if (exists('results'))
      results <- rbind(results, result)
    else
      results <- result
  }

  cat("total correlation: ", cor(results %>% pull(Z), results %>% pull(F)), '\n')
}

second_hyp <- function() {
  cat("here go correlations of lengths of fixations and their distance from the edge of the image:", '\n')
  for (i in participants) {
    cat("participant ", i, ":", '\n')
    result <- as.data.frame(do_second(i))
    if (exists('results'))
      results <- rbind(results, result)
    else
      results <- result
  }

  cat("total correlation: ", cor(results %>% pull(Z), results %>% pull(F)), '\n')
}

third_hyp <- function() {
  cat("here go p.values for null hypothesis that P is independend on Time:", '\n')
  for (i in participants) {
    cat("participant ", i, ":", '\n')
    result <- as.data.frame(do_third(i))
    if (exists('results'))
      results <- rbind(results, result)
    else
      results <- result
  }

  results$P2 <- 1.381e+03 + 5.332e-03*results$time
  ns <- order(results$P2)

  first <- results[ns,][1:(nrow(results) %/% 2), ]
  second <- results[ns,][(nrow(results) %/% 2 + 1):nrow(results), ]

  cat("t.test (final): p.value = ", t.test(first$P, second$P)$p.value, '\n')
}

do_assignment <- function() {
  first_hyp()
  second_hyp()
  third_hyp()
}

do_assignment()
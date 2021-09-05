library(tidyverse)
source("rogue-ss-tactics.R")

generate_master_frame <- function(build_file, enemyACs)
{
  build_grid <- read_csv(build_file)
  frames <- NULL
  for(level in 1:20){
    frames <- rbind(
      frames, 
      cbind(attack_dpr(
          enemyAC = enemyACs,
          level = build_grid$Level[level],
          DEX   = build_grid$DEX[level],
          CBE   = build_grid$CBE[level],
          SS    = build_grid$SS[level],
          ARCHERY = build_grid$ARCHERY[level],
          ADV1 = build_grid$ADV1[level],
          ADV2 = build_grid$ADV2[level],
          EA   = build_grid$EA[level],
          PIERCER = build_grid$PIERCER[level])))
  }
  return(frames)
}


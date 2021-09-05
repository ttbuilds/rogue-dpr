#!/usr/bin/env Rscript

library(tidyverse)
library(RColorBrewer)
library(colorblindr)
source("rogue-ss-tactics.R")
source("rogue-ss-best-tactics-master.R")

args = commandArgs(trailingOnly=TRUE)

image_width    = 7
image_height   = 4
ACRange        = 6:25
default_build_file = "vuman-cbe-ss.csv"

if(length(args) > 0) {
  print(paste("Using supplied build file", args[1]))
  } else {
  print(paste("Using default build file", default_build_file))
  args[1] = default_build_file
  }

build_file = args[1]
build_name = strsplit(build_file, split = ".", fixed = TRUE)[[1]][1]

tactics_labels <- c(
  "Aim/Hide, No -5/+10",
  "Aim/Hide, -5/+10",
  "Two Attacks, No -5/+10",
  "Two Attacks, -5/+10",
  "-5/+10 on First, 2nd if Hits",
  "No -5/+10 on First, 2nd if Hits"
)

master_data <- generate_master_frame(build_file, ACRange)

format_data_for_plot <- function(dataset)
{
  dataset %>%
    mutate(
      best_tactic = factor(
        tactics[apply(dataset[,-c(1:2)], MARGIN = 1, FUN = which.max)], 
        levels = tactics),
      best_dpr    = apply(dataset[,-c(1:2)], MARGIN = 1, FUN = max)) %>%
    pivot_longer(!c(level, enemyAC, best_tactic, best_dpr), names_to = "Tactic", values_to = "DPR") %>%
    mutate(Tactic = factor(Tactic, levels = tactics))  
}

master_data_for_plot <- format_data_for_plot(master_data)

# colorPalette <- brewer.pal(6, "Set2")
colorPalette <- palette_OkabeIto[1:6]

for(l in unique(master_data_for_plot$level)) {
  data_for_plot <- master_data_for_plot %>% filter(level == l)
  all_tactics_graph <- 
    data_for_plot %>%
      ggplot(aes(x = enemyAC, y = DPR, color = Tactic)) +
      geom_point() + geom_line() +
      scale_x_continuous(
        name   = "Enemy AC", 
        breaks = 6:25, 
        minor_breaks = NULL) +
      scale_y_continuous(
        name   = "Expected DPR", 
        breaks = seq(0,ceiling(max(data_for_plot$best_dpr)),2), 
        limits = c(0,ceiling(max(data_for_plot$best_dpr)))) +
      scale_color_manual(
        name   = "Tactic", 
        breaks = tactics, 
        labels = tactics_labels, 
        values = colorPalette)

best_tactic_graph <- 
  data_for_plot %>%
    ggplot(aes(x = enemyAC, y = best_dpr, color = best_tactic)) +
    geom_point() +
    scale_x_continuous(name = "Enemy AC", breaks = 6:25, minor_breaks = NULL) +
    scale_y_continuous(
      name   = "DPR of Best Tactic", 
      breaks = seq(0,ceiling(max(data_for_plot$best_dpr)),2), 
      limits = c(0,ceiling(max(data_for_plot$best_dpr)))) +
    scale_color_manual(
      name   = "Tactic", 
      breaks = tactics, 
      labels = tactics_labels, 
      values = colorPalette)
ggsave(
  file  = paste0(build_name,"-all-level",l,".png"),
  plot  = all_tactics_graph,
  width = image_width,
  height = image_height)
ggsave(
  file = paste0(build_name,"-best-level",l,".png"),
  plot = best_tactic_graph,
  width = image_width,
  height = image_height)
}

master_graph <- master_data_for_plot %>%
  ggplot(aes(x = enemyAC, y = level, color = best_tactic)) +
  geom_point() +
  scale_x_continuous(name = "Enemy AC", breaks = 6:25, minor_breaks = NULL) +
  scale_y_continuous(name = "Level", breaks = 1:20, minor_breaks = NULL) +
  scale_color_manual(name = "Tactic", breaks = tactics, labels = tactics_labels, values = colorPalette)  

ggsave(
  file = paste0(build_name,"-master.png"), 
  plot = master_graph,
  width = image_width,
  height = image_height)

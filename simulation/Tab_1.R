rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(kableExtra)

dat <- readRDS("Tab_1.RDS")

dat %>%
group_by(
  setup, s, dist.x,n
) %>%
  summarise(
    rejrate =  mean(value)
  ) %>%
  ungroup() %>%
  filter(s >= .5) %>%
  filter(setup=="noiso") %>%
  select(!c(dist.x,setup)) %>%
  tidyr::pivot_wider(names_from = n, values_from = rejrate) %>%
  kbl("latex", booktabs = T, escape = FALSE, digits = 2) %>%
  add_header_above(c(" " = 1 ,"Sample size n" = 7)) %>%
  save_kable(paste("Tab_1.tex"),float = FALSE)

# requires \usepackage{booktabs}

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)


dat <- readRDS("../simulation/isotest.RDS")

dat %>%
group_by(
  setup, s, dist.x,n
) %>%
  summarise(
    rejrate =  mean(value)
  ) %>%
  ungroup() %>%
  #filter(rejrate > 0) %>%
  filter(setup=="noiso") %>%
  select(!dist.x) %>%
  data.frame() %>%
  tidyr::pivot_wider(names_from = n, values_from = rejrate)



  ggplot()+
  geom_point(aes(x=as.factor(s),y=rejrate, color = as.factor(n)))

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)

dat <- readRDS("../../sim_data/sim_xdist_A_unif.RDS")
check <- sapply(dat, function(x) inherits(x, 'error'))
out <- dat[!check]
dat[check]
dat <-do.call("rbind", out)
dat


# coverage ---------------------------------------------------------------------

dat.cov <-
  dat %>%
  filter(
    n%in%c(2^seq(9,15,2))
    ) %>%
  filter(
    #!(setup == "Step" & s== 0),
    label == "coversCEP",
    k==Inf) %>%
  #filter(stringr::str_detect(method, ".cov")) %>%
  #tidyr::pivot_wider(names_from = method, values_from = value) %>%
  group_by(
    dist.x,
    n,
    k,
    s,
    setup,
    method
    ) %>%
  summarise(
    cov.rate = mean(value)
    ) %>%
  mutate(
    Coverage = cut(
      cov.rate,
      c(0, .25, .5, 0.75, .875, .925, .975,1),
      include.lowest = TRUE
      ),
    n=log2(n)
    ) %>%
  ungroup() %>%
  mutate(
    method = recode_factor(
      method,
      round = "Calibration Bands",
      round.nc = "NC Calibration Bands",
      #YB.cov = "YB",
      YB = "YB",
      GiViTI = "GiViTI"
      ),
    setup = recode_factor(
      setup,
      DGJ = "Monomial",
      S = "S-shaped",
      kink = "Kink",
      disc = "Disc",
      Step = "Step",
      .ordered = T
      )
    )

relevant.values <-
  dat.cov %>%
  group_by(
    dist.x,
    setup,
    k
    ) %>%
  summarise(
    maxN=max(n),
    minN=min(n),
    maxS=max(s),
    minS=min(s)
  )


p <-
  ggplot(dat.cov %>% filter(method %in% c("Calibration Bands","GiViTI")))+
  geom_tile(
    aes(x=n,y=s,fill = Coverage)
    )+
  facet_grid(method~setup)+
  scale_fill_brewer(
    palette = "PuBuGn",
    direction = 1,
    drop = F
    ) +
  theme(
    legend.position = "bottom",
    legend.key = element_rect(
      color = "black", size =1.2
      ),
    legend.text = element_text(
      size = 10,
      colour = "black",
      margin = margin(
        r = 12,
        unit = "pt"
        )
      ),
    legend.title = element_text(size = 10),
    legend.key.size = unit(5, "mm"),
    legend.spacing.x = unit(1, "mm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust=1,
      colour = "black",
      size = 10),
    axis.text.y = element_text(
      colour = "black",
      size = 10
      ),
    strip.text = element_text(
      face = "bold",
      size = 10
      )
    )+
  scale_x_continuous(
    breaks = seq(min(dat.cov$n),max(dat.cov$n),2),
    labels = c(2^seq(min(dat.cov$n),max(dat.cov$n),2))
  )+
  geom_hline(
    yintercept = seq(-0.05,1.05, 0.1),
    color = "lightgray",
    lwd = 0.5
  )+
  geom_vline(
    xintercept = seq(min(dat.cov$n)+1,max(dat.cov$n)+1, 2),
    color = "lightgray", lwd = 0.5
  )+
  scale_y_continuous(
    breaks = seq(0, 1, 0.2)
  )+
  geom_rect(
    data=relevant.values %>% filter(k==Inf & dist.x=="unif"),
    mapping=aes(
      ymin=minS-.05, ymax=maxS+.05,
      xmin=minN-1, xmax=maxN+1
      ),
    fill=NA,
    color = "black",
    linetype="solid",
    alpha=.5
    )+
  guides(fill = guide_legend(nrow = 1, title = "Coverage rate   "))+
  xlab(expression(paste('sample size ', italic(n))))+
  ylab(expression(paste('shape parameter ', italic(s))))
#ggtitle(paste(dist_, ",","K = ",k_, sep = ""))
p

ggsave("Fig_3.pdf", height = 5, width = 10)


# differences between bands ----

dif.in.round <-
  dat %>%
  filter(
    method%in%c("round","round.nc"),
    label == "width"
  ) %>%
  tidyr::pivot_wider(
    names_from = method,
    values_from = value
  ) %>%
  mutate(
    dif.round = as.numeric(round!=round.nc),
    relativ = round.nc/round,
    n=log2(n)
  ) %>%
  filter(
    #n %in% c(8192,32768),
    dif.round ==1
  ) %>%
  group_by(
    dist.x,
    n,
    k,
    s,
    setup
  ) %>%
  mutate("distrinct draws" = as.factor(n_distinct(seed)),
         s= as.factor(s))

dif.in.round %>%   group_by(n) %>%
  # .[complete.cases(.),] %>%
  ggplot(aes(x=n,y=(relativ), shape = `distrinct draws`, color = s))+
  geom_point()+
  facet_grid(k~setup, scales = "free_y")+
  scale_x_continuous(
    breaks = seq(min(dat.cov$n),max(dat.cov$n),2),
    labels = c(2^seq(min(dat.cov$n),max(dat.cov$n),2))
  )
# If there is a difference between the width of CB and CBnc, then obviously CBnc is wider. We here plot the relative difference between the two methods.



# wir haben bei nc methode immer round+fcasts values... klar ist der durchschnittl breite über alle x (fcast values) anders, da wir die riund mit dazu nehmen


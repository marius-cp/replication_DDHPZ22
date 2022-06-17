rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)

dat <- readRDS("Fig_3.RDS")

# coverage ---------------------------------------------------------------------

dat.cov <-
  dat %>%
  filter(
    n%in%c(2^seq(9,15,2))
    ) %>%
  filter(
    !(setup == "Step" & s== 0)
    ) %>%
  group_by(
    #dist.x,
    n,
    #k,
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
      noiso = "Wave",
      .ordered = T
      )
    )
dat.cov

relevant.values <-
  dat.cov %>%
  group_by(
    #dist.x,
    setup,
    #k
    ) %>%
  summarise(
    maxN=max(n),
    minN=min(n),
    maxS=max(s),
    minS=min(s)
  )
relevant.values

p <-
  ggplot(dat.cov)+
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
    data=relevant.values,
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
  xlab(expression(paste('Sample size ', italic(n))))+
  ylab(expression(paste('Shape parameter ', italic(s))))

p

ggsave("Fig_3.pdf", height = 5, width = (10+2.5))

# verbal stuff ----

covrates.verbal <-
dat.cov %>% filter(
  method == c("Calibration Bands"),
  n %in% seq(9,15,2),
  cov.rate != 1
  )
covrates.verbal

# 212 colored panels
# 33 are != 1
# We observe coverage rates between 0.998 and 1, with the majority of 212-33= 179 out of the 212 displayed coverage values being exactly one.


# alternative ----


dat.cov <- readRDS("sim_rbind_xdist_unif.rds") %>%
  filter(k==Inf) %>%
  select(!k) %>%
  filter(
    label=="coversCEP",
    method %in% c("round","GiViTI", "YB")
  ) %>%
  filter(
    n%in%c(2^seq(9,15,2))
  ) %>%
  filter(
    !(setup == "Step" & s== 0)
  ) %>%
  group_by(
    #dist.x,
    n,
    #k,
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
      noiso = "Wave",
      .ordered = T
    )
  )

ggplot(dat.cov)+
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
    data=relevant.values,
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
  xlab(expression(paste('Sample size ', italic(n))))+
  ylab(expression(paste('Shape parameter ', italic(s))))


ggsave("Fig_3_alternative.pdf", height = (5+2.5), width = (10+2.5))

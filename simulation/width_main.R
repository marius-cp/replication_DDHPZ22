rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)

dat <- readRDS("Fig_4.RDS")



# avg width full interval ----
dat.w01 <-
  dat %>%
  filter(
    label == "width01"
    ) %>%
  group_by(
    #dist.x,
    n,
    #k,
    #s,
    setup,
    method
    ) %>%
  summarise(
    avg_width = mean(value)
  ) %>%
  ungroup() %>%
  mutate(
    n = log2(n),
    Band = recode_factor(
      method,
      round = "Calibration Bands",
      round.nc = "Non-Crossing Calibration Bands",
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


wid.01 <-
  ggplot(dat.w01)+
  geom_line(
    aes(
      y=avg_width, x=n,
        color=Band, linetype=Band
      ),
    size=1
    )+
  facet_grid(1~setup)+
  theme_bw()+
  theme(
    aspect.ratio = 1,
    legend.position = "none",
    legend.text = element_text(size = 10,
                               colour = "black",
                               margin = margin(r = 12, unit = "pt")
                               ),
    legend.title = element_text(size = 10),
    legend.key.width = unit(3, "line"),
    legend.spacing.x = unit(0.075, "cm"),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust=1,
      colour = "black",
      size = 10
      ),
    axis.text.y = element_text(
      colour = "black",
      size = 10
      ),
    strip.text = element_text(face = "bold",
                              size = 10
                              ),
    strip.text.y = element_blank()
    )+
  scale_linetype_manual(values=c("solid","22"))+
  scale_color_brewer(palette = "Set1")+
  scale_x_continuous(
    breaks = seq(min(dat.w01$n),max(dat.w01$n),2),
    labels=c(2^seq(min(dat.w01$n),max(dat.w01$n),2))
  )+
  guides(
    color = guide_legend(nrow = 1, title = "Band              "),
    linetype = guide_legend(nrow = 1, title = "Band              ")
  ) +
  xlab(expression(paste('Sample size ', italic(n))))+
  ylab("Average width")


wid.01
ggsave("Fig_4_top.pdf", wid.01, height = 3, width = 10)

# width per x ----

n_set = c(8192,32768)

dat.wx <-
  dat %>%
  filter(
    label == "width",
    n %in% n_set
  ) %>%
  group_by(
    dist.x,
    n,
    #k,
    #s,
    setup,
    method,
    x
  ) %>%
  summarise(
    avg_width = mean(value)
  ) %>%
  ungroup() %>%
  mutate(
    #n = log2(n),
    Band = recode_factor(
      method,
      round = "Calibration Bands",
      round.nc = "Non-Crossing Calibration Bands",
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

sample_sizes = c(
  '512' =  "n==512",
  '2048' = "n==2048",
  '8192' = "n==8192",
  '32768' = "n==32768"
)

w <-
  ggplot(dat.wx)+
  geom_line(
    aes(
      x=x, y=avg_width,
      color=Band, linetype=Band
      ),
    size=1
    )+
  xlim(c(0,1))+
  facet_grid(n~setup,
             labeller = labeller(n  = as_labeller(sample_sizes, label_parsed)))+
  theme_bw()+
  theme(
    aspect.ratio=1,
    legend.position = "none",#"bottom",
    legend.text = element_text(
      size = 10,
      colour = "black",
      margin = margin(r = 12, unit = "pt")
    ),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust=1,
      colour = "black",
      size = 10
    ),
    axis.text.y = element_text(
      colour = "black",
      size = 10
      ),
    strip.text = element_text(
      face = "bold",
      size = 10
      ),
    legend.key.width = unit(3, "line"),
    legend.spacing.x = unit(0.075, "cm"),
    plot.margin=unit(c(-.5,.05,-.6,.05), "null")
  )+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab("Average width")+
  scale_linetype_manual(values=c("solid","22"))+
  scale_color_brewer(palette = "Set1")+
  guides(
    color = guide_legend(nrow = 1, title = "Band              "),
    linetype = guide_legend(nrow = 1, title = "Band              ")
  )
w

ggsave("Fig_4_bottom.pdf", w, height = 4.75, width = 10)

# other stuff  ----
# verbal stuff on difference between round and nc.round
dat <- readRDS("sim_rbind_xdist_unif.RDS")


dif.in.round <-
  dat %>%
  filter(
    method%in%c("round","round.nc"),
    label == "width",
    k==Inf
  ) %>%
  tidyr::pivot_wider(
    names_from = method,
    values_from = value
  ) %>%
  mutate(
    dif.round = as.numeric(round!=round.nc),
    relativ = round.nc/round,
    n=log2(n)
  )

dif.in.round

dif.in.round %>%
  filter(
    dif.round ==1
    )%>%
  mutate(
    "distrinct draws" = as.factor(n_distinct(seed)),
    s= as.factor(s)
    ) %>%
  ggplot(
    aes(x=n,y=(relativ), shape = `distrinct draws`, color = s)
    )+
  geom_point()+
  facet_grid(k~setup, scales = "free_y")+
  scale_x_continuous(
    breaks = seq(min(log2(dat$n)),max(log2(dat$n)),2),
    labels = seq(min(log2(dat$n)),max(log2(dat$n)),2)
  )


dif.in.round %>%
  group_by(dif.round) %>%
  count()
#  a difference between nc.round and round is observed for 35 of 9393265 x-values
#  the differences occur in 5 of 1000 distinct simulation draws, only for disc and step and only for shape parameters s=0.9 and s=1


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

dat <- readRDS("../../sim_data/sim_rbind_xdist_unif.RDS")

# avg width full interval ----
s_=0.5

dat.w01 <-
  dat %>%
  filter(
    s==s_,
    label == "width01"
    ) %>%
  group_by(
    dist.x,
    n,
    k,
    s,
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

k_=Inf

wid.01 <-
  ggplot(dat.w01%>%
           filter(
             k==k_ & Band%in%c("Non-Crossing Calibration Bands", "YB") )
         )+
  geom_line(
    aes(
      y=avg_width, x=n,
        color=Band, linetype=Band
      ),
    size=1.05
    )+
  facet_grid(.~setup)+
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
                              )
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
  xlab(expression(paste('sample size ', italic(n))))+
  ylab("average width")


wid.01
ggsave("Fig_4_top.pdf", wid.01, height = 3, width = 10)

# width per x ----
s_=.5
k_=Inf
n_set = c(8192,32768)

dat.wx <-
  dat %>%
  filter(
    s==s_,
    label == "width",
    k == k_,
    n %in% n_set
  ) %>%
  group_by(
    dist.x,
    n,
    k,
    s,
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
  '512' =  "n==512", #  '512' =  "bold(n)==bold(512)",
  '2048' = "n==2048",
  '8192' = "n==8192",
  '32768' = "n==32768"
)

w <-
  ggplot(dat.wx %>%
           filter(
             k==k_ & Band%in%c("Non-Crossing Calibration Bands", "YB")
             )
         )+
  geom_line(
    aes(
      x=x, y=avg_width,
      color=Band, linetype=Band
      ),
    size=.75
    )+
  xlim(c(0,1))+
  facet_grid(n~setup,
             labeller = labeller(n  = as_labeller(sample_sizes, label_parsed)))+
  theme_bw()+
  theme(
    aspect.ratio=5/5,
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
  xlab(expression(paste('forecast value ', italic(x))))+
  ylab("average width")+
  scale_linetype_manual(values=c("solid","22"))+
  scale_color_brewer(palette = "Set1")+
  guides(
    color = guide_legend(nrow = 1, title = "Band              "),
    linetype = guide_legend(nrow = 1, title = "Band              ")
  )
w
ggsave("Fig_4_bottom.pdf", w, height = 4.75, width = 10)

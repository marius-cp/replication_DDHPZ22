rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(doParallel)
library(givitiR)
library(ggplot2)
library(foreach)

# install calibrationband library
# install.packages("devtools")
devtools::install_github("marius-cp/calibrationband")
library(calibrationband)

p.dat <- function(bands){
  r <- bands
  x_ <- r$x
  upr <- r$upr
  lwr <- r$lwr

  m <- length(x_)
  ind_1 <- c(1, rep(2:m, each = 2))
  ind_2 <- c(rep(seq_len(m - 1), each = 2), m)
  upr <- upr[ind_1]
  lwr <- lwr[ind_2]

  x_lwr <- x_[ind_1]
  x_upr <- x_[ind_2]

  p.dat = tibble::tibble(
    x_lwr = x_lwr,
    x_upr = x_upr,
    upr = upr,
    lwr = lwr
  )
  return(p.dat)

}


pc <- approxfun(y = c(0, 0.25, .9, .95), x = c(0, .25,.257, 1), ties = min)


set.seed(123)
al = .05
n <-10000
x <- (1:n)/n

dat <- tibble(pr=x, cep = pc(pr),
              y=rbinom(n,1,cep)
              )%>% arrange(pr)
dat


# K = 10, 1 digit
round1 <-
  calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = log10(10)/log10(10), nc = F)

# K = 100, 2 digits
 round2 <-
  calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = log10(100)/log10(10), nc = F)

# K = 1000, 3 digits
 round3 <-
   calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = 3, nc = F)

# K = 20
 round4 <-
   calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = log10(20)/log10(10), nc = F)

# K = 30
 round5 <-
   calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = log10(30)/log10(10), nc = F)

# K = Inf
standard <-
  calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "standard", nc = F)

plotdat <-
  bind_rows(
    #p.dat(round1$bands)%>% mutate(K ="10", k = 10),
    p.dat(round4$bands) %>% mutate(K ="20", k = 20),
    #p.dat(round5$bands) %>% mutate(K ="30", k= 30),
    p.dat(round2$bands) %>% mutate(K ="100", k = 100),
    #p.dat(round3$bands) %>% mutate(K ="1000"),
    p.dat(standard$bands) %>% mutate(K ="Inf", k = Inf)
  ) %>%
  mutate(
    K = recode_factor(
      K,
      "Inf" = "K = Inf         ",
      "1000" = "K = 1000         ",
      "100" = "K = 100         ",
      "20" = "K = 20         ",
      #"YB 0" = "Yang & Barber      ",
      .ordered = T
    )
  )


pcurve <-
  bind_rows(
    dat %>% mutate(id = "calibration bands"),
    dat %>% dplyr::filter(pr>=.2&pr<=.3) %>% mutate(id = "zoom calibration bands")
    )

p_1 <-
  ggplot(
    bind_rows(
      plotdat  %>% mutate(id = "calibration bands"),
      plotdat  %>% dplyr::filter(x_upr>=.2&x_lwr<=.3) %>% mutate(id = "zoom calibration bands")
    )
  )+
  facet_wrap(id~., scales = "free_x")+
  geom_line(data = pcurve,mapping = aes(x=pr,y=cep), color = "darkgray")+
  geom_line(mapping = aes(x=x_upr,y=upr,color=as.factor(K), linetype = as.factor(K)))+
  geom_line(mapping=aes(x=x_lwr,y=lwr,color=as.factor(K), linetype = as.factor(K)))+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))+
  theme_bw()+
  theme(
    aspect.ratio=1,
    legend.position = "bottom",
    legend.key.width = unit(2.5, "line"),
    legend.spacing.x = unit(.01, "cm")
  )+
  scale_color_manual(
    values = c("black",RColorBrewer::brewer.pal(n=9,"YlOrRd")[(c(7,5))], "black" )
  )+
  scale_linetype_manual(
    values=c("solid","31","11")
  )+
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))+
  scale_y_continuous(
    breaks = c(0,.2,.4,.6,.8,1),
    labels= c(0,.2,.4,.6,.8,1)
  )



p_1

ggsave("Fig_S1.pdf", p_1, width = 8, height = 5)

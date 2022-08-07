rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# helper
interpolate <- function(x, y, xout, right = 1) {
  stats::approx(x = x, y = y, xout = xout, f = right, method = "constant")$y
}

library(dplyr)
library(ggpubr)
library(ResourceSelection)

# install calibrationband library
# install.packages("devtools")
devtools::install_github("marius-cp/calibrationband")
library(calibrationband)

bw_train <- readRDS("../application/bw_train.RDS")
bw_test <- readRDS("../application/bw_test.RDS")

lt = "solid" # linetype global for YB
cb = FALSE
# Fig 1 ----
mod1 <- glm(
  lbw ~ mager+ I(mager^2) + sex +  bmi + smoke + r1_diabetis + r2_hypertension +
    r3_prepreterm + r4_infertitlity + r5_precesar + preterm  + momHS + more + inf,
  data = bw_train,
  family=binomial(link='probit')
)
summary(mod1)

mod1.pairs.ts <-
  tibble(
    "Y" = bw_test$lbw,
    "P" = predict(mod1,newdata = bw_test, type='response')) %>%
  arrange(P)
mod1.pairs.ts

fig1 <- calibration_bands(
  x=mod1.pairs.ts$P,
  y=mod1.pairs.ts$Y,
  method = "round",
  digits = 3,
  nc = F
)


summary(fig1,iso_test = T)

fig1.addYB <- calibration_bands(
  x=mod1.pairs.ts$P,
  y=mod1.pairs.ts$Y,
  method = "YB"
)

min(mod1.pairs.ts$P)

fig1a.addYB <-
  autoplot(fig1, approx.equi = 1000 ,cut.bands = cb)+
  geom_line(mapping = aes(
    x=seq(0,1,length.out=1000),#fig1.addYB$bands$x,
    y=interpolate(x=fig1.addYB$bands$x, y= fig1.addYB$bands$lwr, xout=seq(0,1,length.out=1000), right = 0)#fig1.addYB$bands$lwr
    ), color = "purple", linetype = lt)+
  geom_line(mapping = aes(
    x=seq(0,1,length.out=1000),
    y=interpolate(x=fig1.addYB$bands$x, y= fig1.addYB$bands$upr, xout=seq(0,1,length.out=1000), right = 1)#fig1.addYB$bands$upr
    ), color = "purple",  linetype = lt)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))


fig1b.addYB <-
  autoplot(fig1, approx.equi = 1000,cut.bands = cb)+
  geom_line(mapping = aes(
    x=seq(0,1,length.out=1000),#fig1.addYB$bands$x,
    y=interpolate(x=fig1.addYB$bands$x, y= fig1.addYB$bands$lwr, xout=seq(0,1,length.out=1000), right = 0)#fig1.addYB$bands$lwr
  ), color = "purple", linetype = lt)+
  geom_line(mapping = aes(
    x=seq(0,1,length.out=1000),
    y=interpolate(x=fig1.addYB$bands$x, y= fig1.addYB$bands$upr, xout=seq(0,1,length.out=1000), right = 1)#fig1.addYB$bands$upr
  ), color = "purple",  linetype = lt)+
  coord_cartesian(xlim=c(0,.1),ylim=c(0,.15))+
  theme(aspect.ratio=1)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))


# mod 2 ----

mod2 <-
  glm(
    lbw ~ mager+ I(mager^2) + sex +  bmi + smoke + r1_diabetis +
      r2_hypertension + r3_prepreterm + combgest +r4_infertitlity +
      r5_precesar + momHS + more + inf,
    data = bw_train,
    family=binomial(link='probit')
  )

mod2.pairs.ts <- tibble(
  "Y" = bw_test$lbw,
  "P" = predict(mod2,newdata = bw_test, type='response')
) %>%
  arrange(P)
mod2.pairs.ts

mod2.fig5 <- calibration_bands(
  x=mod2.pairs.ts$P,
  y=mod2.pairs.ts$Y,
  method = "round",
  digits = 3,
  nc = T
)
mod2.fig5
summary(mod2.fig5, iso_test = T)

fig5a.addYB <- calibration_bands(
  x=mod2.pairs.ts$P,
  y=mod2.pairs.ts$Y,
  method = "YB"
)
fig5a.addYB$bands$x

left <-
  autoplot(mod2.fig5,cut.bands = cb)+
  geom_line(mapping = aes(
    x=seq(0,1,length.out=1000),
    y=interpolate(x=fig5a.addYB$bands$x, y= fig5a.addYB$bands$lwr, xout=seq(0,1,length.out=1000), right = 0)#fig5a.addYB$bands$lwr
    ), color = "purple", linetype = lt)+
  geom_line(mapping = aes(
    x=seq(0,1,length.out=1000),
    y=interpolate(x=fig5a.addYB$bands$x, y= fig5a.addYB$bands$upr, xout=seq(0,1,length.out=1000), right = 1)#fig5a.addYB$bands$upr
  ), color = "purple",  linetype = lt)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))


# mod 3 ----
#
mod3 <-
glm(
  lbw ~ mager+ I(mager^2) + sex +  bmi + smoke + r1_diabetis +
    r2_hypertension + r3_prepreterm + preterm + r4_infertitlity +
    r5_precesar + momHS + more + inf,
  data = bw_train,
  family=binomial(link='cauchit')
)
summary(mod3)

mod3.pairs.ts <- tibble(
  "Y" = bw_test$lbw,
  "P" = predict(mod3,newdata = bw_test, type='response')
) %>%
  arrange(P)
mod3.pairs.ts

mod3.fig5 <- calibration_bands(
  x=mod3.pairs.ts$P,
  y=mod3.pairs.ts$Y,
  method = "round",
  digits = 3,
  nc = T
)
mod3.fig5

summary(mod3.fig5, iso_test = T)

fig5b.addYB <- calibration_bands(
  x=mod3.pairs.ts$P,
  y=mod3.pairs.ts$Y,
  method = "YB"
)

right <-
  autoplot(mod3.fig5,cut.bands = cb)+
  geom_line(mapping = aes(
    x=seq(0,1,length.out=1000),
    y=interpolate(x=fig5b.addYB$bands$x, y= fig5b.addYB$bands$lwr, xout=seq(0,1,length.out=1000), right = 0)#fig5b.addYB$bands$lwr
    ), color = "purple",  linetype = lt)+
  geom_line(mapping = aes(
    x=seq(0,1,length.out=1000),
    y=interpolate(x=fig5b.addYB$bands$x, y= fig5b.addYB$bands$upr, xout=seq(0,1,length.out=1000), right = 1)#fig5b.addYB$bands$upr
    ), color = "purple",  linetype = lt)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))



comb <-
ggarrange(fig1a.addYB, fig1b.addYB, left, right, nrow = 2, ncol = 2)


ggsave("Fig_S3.pdf", height = 8, width = 8)

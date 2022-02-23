rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)

# install calibrationband library
devtools::install_github("https://github.com/marius-cp/calibrationband"
                         ,ref="main"
                         # insert your access token below
                         ,auth_token = "5514ebdc4ba3a82b44b3298be724b8369dae5bc3"
                         ,dependencies = T
)
library(calibrationband)

bw_train <- readRDS("bw_train.RDS")
bw_test <- readRDS("bw_test.RDS")


# Fig 1 ----
tr.mod <- glm(
  lbw ~ mager+ I(mager^2) + sex +  bmi + smoke + r1_diabetis + r2_hypertension +
    r3_prepreterm + r4_infertitlity + r5_precesar + preterm  + momHS + more + inf,
  data = bw_train,
  family=binomial(link='probit')
  )
summary(tr.mod)

# pred-real pairs test (OUT-OF-SAMPLE PREDICTION) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pairs.ts <- tibble("Y" = bw_test$lbw,
                   "P" = predict(tr.mod,newdata = bw_test, type='response')) %>%
  arrange(P)
pairs.ts

fig1a <- calibration_bands(
  x=pairs.ts$P,
  y=pairs.ts$Y,
  method = "round",
  digits = 3,
  nc = F
  )

summary(fig1a)
fig1a$cal
p <-
autoplot(fig1a, approx.equi = F,cut.bands = T, diag = "red",  point=1000)
p

p+
  geom_line(aes(basep$x_upr, basep$upr), color = "green")+
  geom_line(aes(basep$x_lwr, basep$lwr), color = "green")+
  theme_bw()+
  xlim(0,.1)+
  ylim(0,.15)


basep <- plot(fig1a)
basep$x_upr

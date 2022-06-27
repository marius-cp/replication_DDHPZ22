rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggpubr)
library(ResourceSelection)

# install calibrationband library
# install.packages("devtools")
devtools::install_github("marius-cp/calibrationband")
library(calibrationband)

bw_train <- readRDS("bw_train.RDS")
bw_test <- readRDS("bw_test.RDS")


# Fig   ----
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

fig1$bands %>% dplyr::filter(lwr>upr) # no crossings

fig1a <-
  autoplot(fig1, approx.equi = 1000 ,cut.bands = T)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))

fig1a


fig1b <-
  autoplot(fig1, approx.equi = 1000,cut.bands = F)+
  coord_cartesian(xlim=c(0,.1),ylim=c(0,.15))+
  theme(aspect.ratio=1)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))


fig1b

Fig_1 <-
  ggarrange(fig1a, fig1b, nrow = 1)

start.time <- Sys.time()
standardBands <- calibration_bands(
  x=mod1.pairs.ts$P,
  y=mod1.pairs.ts$Y,
  method = "standard",
  #digits = 3,
  nc = F
)
end.time <- Sys.time()
(run.time <- end.time-start.time)
save(standardBands, file = "supplement_method_standard_mod1.Rdata")



autoplot(fig1)+
  geom_line(mapping = aes(standardBands$bands$x,standardBands$bands$lwr),color="green")+
  geom_line(mapping = aes(standardBands$bands$x,standardBands$bands$upr), color="green")+
  coord_cartesian(xlim=c(0,.1),ylim=c(0,.15))+
  theme(aspect.ratio=1)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))

ggsave("Fig_2_supplement.pdf", width = 4, height = 4)





rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggpubr)
library(ResourceSelection)

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
  nc = T
  )


basep <- plot(fig1)
summary(fig1)
print(fig1)

fig1a <-
autoplot(fig1, approx.equi = 1000 ,cut.bands = T)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))

fig1a


fig1b <-
  autoplot(fig1, approx.equi = 1000,cut.bands = T)+
  coord_cartesian(xlim=c(0,.1),ylim=c(0,.15))+
  theme(aspect.ratio=1)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))


fig1b

Fig_1 <-
ggarrange(fig1a, fig1b, nrow = 1)

ggsave("Fig_1.pdf", Fig_1, height = 4, width = 8)

fig1$bands %>%
  filter(x ==.05)
#   x    lwr    upr
# .05  .0466  .0663


mod1.pairs.tr <-
  tibble(
    "Y" = bw_train$lbw,
    "P" =predict(mod1,newdata = bw_train, type='response')
    )
HL.IS.mod1 <-  hoslem.test(x=mod1.pairs.tr$Y, y=mod1.pairs.tr$P, g=10);HL.IS.mod1
HL.OOS.mod1 <- hoslem.test(x=mod1.pairs.ts$Y, y=mod1.pairs.ts$P, g=10);HL.OOS.mod1
HL.OOS.mod1$statistic # = 231.8554
1 - pchisq(HL.OOS.mod1$statistic, 10) # =0
1 - pchisq(HL.OOS.mod1$statistic, 9) # =0

# Fig 5 ----
## a ----
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

Fig_5_left <-
autoplot(mod2.fig5, approx.equi = 1000 ,cut.bands = T)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))

mod2.pairs.tr <-
  tibble(
    "Y" = bw_train$lbw,
    "P" =predict(mod2,newdata = bw_train, type='response')
  )
HL.IS.mod2 <-  hoslem.test(x=mod2.pairs.tr$Y, y=mod2.pairs.tr$P, g=10);HL.IS.mod2 #28528
HL.OOS.mod2 <- hoslem.test(x=mod2.pairs.ts$Y, y=mod2.pairs.ts$P, g=10);HL.OOS.mod2
HL.OOS.mod2$statistic # =10327.12
1 - pchisq(HL.OOS.mod2$statistic, 10) # =0
1 - pchisq(HL.OOS.mod2$statistic, 9) # =0

# b ---
mod3 <-
  glm(
    lbw ~ mager+ I(mager^2) + sex +  bmi + smoke + r1_diabetis +
      r2_hypertension + r3_prepreterm + preterm + r4_infertitlity +
      r5_precesar + momHS + more + inf,
    data = bw_train,
    family=binomial(link='cauchit')
  )
summary(mod3)

## b ----
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

Fig_5_right <-
  autoplot(mod3.fig5, approx.equi = 1000 ,cut.bands = T)+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))

mod3.pairs.tr <-
  tibble(
    "Y" = bw_train$lbw,
    "P" =predict(mod3,newdata = bw_train, type='response')
  )
HL.IS.mod3 <-  hoslem.test(x=mod3.pairs.tr$Y, y=mod3.pairs.tr$P, g=10);HL.IS.mod3 #11273
HL.OOS.mod3 <- hoslem.test(x=mod3.pairs.ts$Y, y=mod3.pairs.ts$P, g=10);HL.OOS.mod3
HL.OOS.mod3$statistic # =4355.7
1 - pchisq(HL.OOS.mod3$statistic, 10) # =0
1 - pchisq(HL.OOS.mod3$statistic, 9) # =0




Fig_5 <-
  ggarrange(Fig_5_left, Fig_5_right, nrow = 1)
Fig_5

ggsave("Fig_5.pdf", Fig_5, height = 4, width = 8)

Fig_1

readme <-
  ggarrange(
    fig1a + ggtitle("Figure 1 left"),
    Fig_5_left+ ggtitle("Figure 5 left"),
    nrow = 1)
readme

ggsave("Fig_readme.png", readme, height = 4, width = 8)


# other ----
# compare number of obs
bind_rows(
  mod1.pairs.ts %>% mutate(M=1),
  mod2.pairs.ts %>% mutate(M=2),
  mod3.pairs.ts %>% mutate(M=3)
  ) %>%
  mutate(
    int = cut(
      P,
      c(0, .03, .04, 0.2, 1),
      include.lowest = TRUE
    )
  ) %>%
  group_by(M,int) %>%
  count() %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = int, values_from = n)

# # A tibble: 3 x 5
# M `[0,0.03]` `(0.03,0.04]` `(0.04,0.2]` `(0.2,1]`
# <dbl>      <int>         <int>        <int>     <int>
#   1     1     507807        230087       148680    113426
#   2     2     501514        104782       303276     90428
#   3     3       1416        885184          165    113235

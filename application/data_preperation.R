rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(haven)

# read orginal file and extract relevant data
# data is downloaded from: https://data.nber.org/natality/2017/natl2017.zip
# stata file (.dta) has nice labels of the variables
# for description see: https://data.nber.org/natality/2017/natl2017.pdf


# define educ factor variable
educ = c(
  "1"="grade8orless" ,  "2"="grade9to12",
  "3"="hs"           ,  "4"="somecollege",
  "5"="associatedeg" ,  "6"="bsc",
  "7"="msc"          ,  "8"="dr"
  )

bw_data_raw <- read_dta("../../natl2017.dta.zip") # insert path to zipped data set here



bw <-
  bw_data_raw %>%
  select(
    dob_yy, # birth year (2017)
    dob_mm, # birth month
    dbwt, # birth weight, detail in grams (edited) ; if not stated = 9999
    mager, #mother age, single years of age
    # maternal life style and health characteristics
    wtgain, # weight gain
    wtgain_rec, #  weight gain grouped
    cig_0, # number cigarettes before pregnancy
    cig_1,# number cigarettes 1st trimester
    cig_2,# number cigarettes 2nd trimester
    cig_3,# number cigarettes 3rd trimester
    cig0_r,# number cigarettes before pregnancy recode  (0=non,1=1-5 cigs, 2=6-10,...,6=unknown)
    cig1_r,# number cigarettes 1st trimester recode, see above.
    cig2_r,# number cigarettes 2nd trimester recode, see above.
    cig3_r,# number cigarettes 3rd trimester recode, see above.  # 1 pack = 20 cigarettes
    pwgt_r, #pre-pregnancy weight recode, if 99.9 = unkown
    m_ht_in, # mothers height in total inches
    bmi, # body mass index
    sex, # sex of infant
    combgest, # combgest:  combined gestation age, detail in week
    dplural, #plurality recode 1= single, 2 = twin, 3=tripple, ....
    rf_pdiab, #pre-pregnancy diabetes (diagnosis in this before preg.)
    rf_gdiab, # gestational diabetes=yes (diagnosis in this pregnancy)
    rf_phype,# pre pregnancy hypertension
    rf_ghype,# gestational hypertension
    rf_ehype,# hypertension eclampsia
    rf_ppterm,# previous preterm birth
    rf_inftr,# infertility treatment used
    rf_cesar,
    meduc,
    # infections present and/or treated during this  pregnancy:
    ip_gon, #infections present  gonorrhea
    ip_syph, #infections present syphilis
    ip_chlam,  #infections present chlamydia
    ip_hepatb,  #infections present hepatitis b
    ip_hepatc  #infections present hepatitis c
    ) %>%
  # now clear non recordings
  filter(
    # maternal life style and health characteristics
    cig_0 != 99,cig_1 != 99,cig_2 != 99, cig_3 != 99, # non reported cig
    cig0_r != 6, cig1_r != 6, cig2_r != 6, cig3_r != 6,# non reported cig recoded
    pwgt_r != 999,
    m_ht_in != 99, # non stated height
    bmi != 99.9,# non stated BMI
    wtgain != 99,  # unknown or not stated
    wtgain_rec != 9,
    # bw and gestational age
    dbwt != 9999, # not stated bw in grams,
    combgest != 99, # unknown gestation age detail in weeks
    # risk factors
    rf_pdiab != "u", # unknown or not stated risk factor
    rf_gdiab != "u",
    rf_phype != "u",
    rf_ghype != "u",
    rf_ehype!= "u",
    rf_ppterm!= "u",
    rf_inftr!= "u",
    rf_cesar!= "u",
    meduc != 9,
    # infections
    ip_gon!= "u", # unknown or not stated
    ip_syph!= "u",
    ip_chlam!= "u",
    ip_hepatb != "u",
    ip_hepatc  != "u"
    ) %>%
  mutate(
    id = 1:n(),
    lbw = as.numeric(dbwt<2500),
    # r1 = 1 if rf_pdiab=pre-pregnancy diabetes=yes or rf_gdiab=gestational diabetes=yes
    r1_diabetis = as.numeric(rf_pdiab == "Y" | rf_gdiab == "Y"),
    # r2 = 1 if prepreghypertension=y or rf_ghype=gestationalhypertension=y or f_ehype=hypertensioneclampsia=y
    r2_hypertension = as.numeric(rf_phype == "Y" | rf_ghype == "Y" | rf_ehype == "Y"),
    # r3 = 1 if rf_ppterm = previous preterm birth = yes
    r3_prepreterm = as.numeric(rf_ppterm=="Y"),
    # r4 = 1 if rf_inftr = infertility treatment used = yes / q: pregnancy resulted from infertility treatment-if yes, check
    # all that apply:... eg fertility-enhancing drugs or vitro fertilization (ivf),
    r4_infertitlity = as.numeric(rf_inftr=="Y"),
    # r5 = 1 if rf_cesar previous cesarean = yes / q: mother had a previous cesarean delivery
    r5_precesar = as.numeric(rf_cesar=="Y"),
    #combgest  combined gestation age detail in weeks
    # https://www.who.int/news-room/fact-sheets/detail/preterm-birth
    preterm = case_when(
      combgest < 28 ~ "extreme_preterm",
      combgest < 32 & combgest >= 28 ~ "very_preterm",
      combgest < 37 & combgest >= 32 ~ "late_preterm",
      combgest >= 37 ~ "no_preterm"
    ),
    meduc = recode(meduc, !!!educ),
    ym_birth = format(lubridate::make_date(dob_yy,dob_mm), "%y-%m")) %>%
  select(!c(dob_yy,dob_mm)) %>%
  select(id, ym_birth,dbwt, lbw, everything()) %>%
  mutate(
    momHS = ifelse(meduc %in% c("grade8orless"), 0, 1),
    AvgCigs= (cig_1+cig_2+cig_3)/3,
    smoke = case_when(
      AvgCigs > 0 & AvgCigs <=9 ~ "s_smoker",
      AvgCigs > 9 & AvgCigs <=20 ~ "m_smoker",
      AvgCigs > 20 ~ "xl_smoker",
      AvgCigs == 0 ~ "no"
      ),
    more = as.numeric(dplural > 2),
    inf = as.numeric(ip_gon == "Y" | ip_syph == "Y" |ip_chlam== "Y" | ip_hepatb == "Y" |ip_hepatc== "Y") )

set.seed(123456)
set.seed(1234567)
random.sort <-
  bw%>%
  mutate(rnd = rnorm(nrow(bw))) %>%
  arrange(rnd) %>%
  dplyr::select(!rnd)

bw_train <- random.sort[1000001:nrow(random.sort),]
bw_test <- random.sort[1:1000000,]

saveRDS(bw_train, "bw_train.RDS")
saveRDS(bw_test, "bw_test.RDS")

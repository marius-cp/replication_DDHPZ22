rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(doParallel)
library(givitiR)
library(ggplot2)
library(foreach)
source("../simulation/helpers.R")

# install calibrationband library
# install.packages("devtools")
devtools::install_github("marius-cp/calibrationband")
library(calibrationband)

# sim comp cost ----
n.set <- 2^seq(9,14,1)
misspec.set <- c("kink")
al = .05

# set up parallel
core.max <- 50
cl <- makeCluster(min(parallel::detectCores()-1, core.max) )
registerDoParallel(cl)
start.time <- Sys.time()
MCsim <- foreach(i = 1:300,
                 .errorhandling = "pass",
                 .packages=c(
                   "calibrationband", "givitiR", "dplyr", "tibble")
                 )%dopar%{


  s=0.8

  df <- tibble()
  for (n in n.set) {

  set.seed(i)

  x <- runif(n)

  for (misspec in misspec.set) {

    if(misspec=="S"){
      p <- function(x,s){p = 1/(1+((1/x*(1-x))^(s+1)));return(p)}
      dat <- tibble(pr=x, s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)
    } else if (misspec=="Step"){
      p <- function(x,s){
        sstar = 15-10*s
        p = 1/(2*sstar)+(1/(sstar))*floor(sstar*x) - as.numeric(x==1)*(1/(sstar))
        return(p)}
      dat <- tibble(pr=x, s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)
    } else if (misspec=="DGJ") {
      p <- function(x,s){p = x^(1-s);return(p)}
      dat <- tibble(pr=x, s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)
    } else if (misspec=="kink") {
      pc <- approxfun(y = c(0, 0.2, 1), x = c(0, 0.2+0.8*s , 1), ties = min)
      dat <- tibble(pr=x,s=s, cep = pc(pr), y=rbinom(n,1,cep))%>% arrange(pr)
    } else if (misspec=="disc") {
      p <- function(x,s){
        p= as.numeric(x<=.1|x>=.9)*x + as.numeric(x>.1 & x<.9)*((s/2)+x*(.5-s/2)/.5)
        return(p)}

      dat <- tibble(pr=x,s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)

    } else if (misspec == "noiso"){
      p <- function(x,s){
        a<-(2*s-1)# diagonal if -1, "max" non iso -1
        c<-4*(a+1)
        p =.5-a*(x-.5)+c*(x-.5)^3
        return(p)}
      dat <- tibble(pr=x, s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)
    }

  #dat <- tibble(pr=x,s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)

  band0 <-
      calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = 1, nc = T)

  band1 <-
    calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = 3, nc = T)

  band2 <-
    calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = 2, nc = T)

  band3 <-
    calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "standard")

  band4 <-
    calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "YB")


  df.append <- tibble(
    label = "time",
    method =c(band0$method, band1$method, band2$method, band3$method, band4$method),
    nc = c(band0$nc, band1$nc, band2$nc, band3$nc, band4$nc),
    digits = unlist(
      lapply(
        list(band0$digits, band1$digits, band2$digits, band3$digits, band4$digits),
        function(x) ifelse(is.null(x),0,as.numeric(x)))
      ),
    n=n,
    setup = misspec,
    s=s,
    value = as.numeric(c(band0$time, band1$time,band2$time,band3$time,band4$time)),
    x=NA
  ) %>%
  bind_rows(
    bind_rows(
      bandinfo(
        obj=band0, dist.x = "unif", s=s, k=Inf, setup = misspec, n=n
      )$out %>%  mutate(digits = band0$digits ),
      bandinfo(
        obj=band1, dist.x = "unif", s=s, k=Inf, setup = misspec, n=n
      )$out %>%  mutate(digits = band1$digits ),
      bandinfo(
        obj=band2, dist.x = "unif", s=s, k=Inf, setup = misspec, n=n
      )$out %>%  mutate(digits = band2$digits ),
      bandinfo(
        obj=band3, dist.x = "unif", s=s, k=Inf, setup = misspec, n=n
      )$out %>%  mutate(digits =  ifelse(is.null(band3$digits),0,as.numeric(band3$digits))),
      bandinfo(
        obj=band4, dist.x = "unif", s=s, k=Inf, setup = misspec, n=n
      )$out %>%  mutate(digits =  ifelse(is.null(band4$digits),0,as.numeric(band4$digits)))
    ) %>%
      select(!c(k, dist.x))
    )




  df <- bind_rows(df,df.append)


  }
  }
  df

}
stopCluster(cl)
end.time <- Sys.time()
(run.time <- end.time-start.time)# 17 h
check <- sapply(MCsim, function(x) inherits(x, 'error'))
out <- MCsim[!check]
MCsim[check]

dat <-do.call("rbind", out)
dat
saveRDS(dat,"simulation_compcost.rds")

# width plot ----

dat <- readRDS("simulation_compcost.rds") %>%
  dplyr::filter(digits != 1)

dat %>%
  filter(label == "width")

n_set = c(8192,16384)


p_width <-
dat %>%
  filter(
    label == "width",
    n %in% n_set
  ) %>%
  mutate(method = paste(method , digits)) %>%
  group_by(
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
    method = recode_factor(
      method,
      "standard 0" = "standard, i.e. K = Inf",
      "round 3" = "rounding with K=10^3",
      "round 2" = "rounding with K=10^2",
      "round 1" = "rounding with K=10^1",
      "YB 0" = "Yang & Baber",
      .ordered = T
    )
  ) %>%
  ggplot()+
  geom_line(
    aes(
      x=x, y=avg_width,
      color=method, linetype=method
    ),
    #size=1
  )+
  xlim(c(0,1))+
  facet_grid(n~.#setup,
             #labeller = labeller(n  = as_labeller(sample_sizes, label_parsed))
             )+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab("Average width")+
  scale_linetype_manual(values=c("solid","31","11","dotdash", 21))+
  scale_color_manual(
  values = c("black",RColorBrewer::brewer.pal(n=9,"YlOrRd")[c(7,5)],"purple")
)+
  theme_bw()+
 theme(
   legend.position = "bottom",
   legend.text = element_text(size = 10,
                              colour = "black",
                              margin = margin(r = 15, unit = "pt")
   ),
   legend.title = element_text(size = 10),
   legend.key.width = unit(3, "line"),
   legend.spacing.x = unit(0.1, "cm")
  )+
  guides(
    color = guide_legend(nrow = 1, title = " "),
    linetype = guide_legend(nrow = 1, title = " ")
  )

p_width




# time plot ----

avg <-
dat %>%
  dplyr::filter(label=="time") %>%
  mutate(method = paste(method , digits))

avg %>% filter(method=="YB 0" & label == "time")

avg %>%
  dplyr::filter(method=="standard 0") %>%
  group_by(method,n,setup,s) %>%
  summarise(avg = (mean(value)))

pdat <-
avg %>%
  group_by(method,n,setup,s) %>%
  summarise(avg = mean(value)) %>%
  ungroup() %>%
  mutate(
    transavg = lubridate::seconds_to_period(avg) %>% round(2),
    method = recode_factor(
      method,
      "standard 0" = "K = Inf        ",
      "round 3" = "K = 1000        ",
      "round 2" = "K = 100        ",
      "round 1" = "K = 10        ",
      "YB 0" = "Yang & Baber        ",
      .ordered = T
    )
  )
pdat$method %>% unique()

p_time <-
ggplot(pdat)+
  geom_line(
    aes(x=((n)),y=(avg),color = method, linetype = method)#, size =1
  )+
  # geom_point(
  #   aes(x=((n)),y=(avg),color = method, #shape = method,
  #       group = method)
  # )+
  #scale_y_log10()+
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x)(c(10^-4,10^-2,10^-0,10^2,10^3)),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
  scale_x_log10(
    breaks = 2^seq(min(log2(pdat$n)),max(log2(pdat$n)),1),
    labels= 2^seq(min(log2(pdat$n)),max(log2(pdat$n)),1)
  )+
  ylab("Average seconds to compute")+
  xlab(expression(paste('Sample size ', italic(n))))+
  theme_bw()+
  theme(
    #legend.position = "none"
  )+
  scale_linetype_manual(values=c("solid","31","11","dotdash", 21))+
  scale_color_manual(
    values = c("black",RColorBrewer::brewer.pal(n=9,"YlOrRd")[c(7,5)],"purple")
  )+
  guides(
    color = guide_legend(nrow = 1, title = " "),
    linetype = guide_legend(nrowl = 1, title = " ")
  )+
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.5, "line"),
    legend.spacing.x = unit(.01, "cm"),
  )


p_time


ggpubr::ggarrange(
  p_time,
  p_width,
  widths = c(1,1),
  common.legend = T,
  legend = "bottom"
)

ggsave("Fig_S2.pdf", width = 10, height = 5)


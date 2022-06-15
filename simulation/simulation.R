rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(doParallel)
library(givitiR)
library(foreach)
source("helpers.R")

# install calibrationband library
# install.packages("devtools")
devtools::install_github("marius-cp/calibrationband")
library(calibrationband)

# set of sample sizes
n.set <- 2^seq(9,15,1)
# restrictions on the number of unique prediction values
k.set <- c(Inf)
# misspecification set
s.set <- seq(0,1,.1)
# DGP (functional form of misspecification)
misspec.set <-  c("S", "Step", "DGJ", "kink", "disc", "noiso")
# distribution of prediction values (unif, logit-normal, beta)
dist.x <-"unif"
# parameters for beta distribution
b.param.1 <- .5
b.param.2 <- 2
# significance level
al = .05

# set up parallel
core.max <- 80
cl <- makeCluster(min(parallel::detectCores()-1, core.max) )
registerDoParallel(cl)
start.time <- Sys.time()

MCsim <- foreach(i = 1:1000,
                 .errorhandling = "pass",
                 .packages=c(
                   "calibrationband", "givitiR", "dplyr", "tibble", "logitnorm"
                             )
                 )%dopar%{
                   df <- tibble()
                   for (n in n.set){
                     for (k in k.set){
                       for (s in s.set) {
                         for (misspec in misspec.set) {
                           if(misspec=="DGJ" & s==1) next
                           set.seed(i)
                           if (k==Inf){
                             if(dist.x=="unif"){
                               x <- runif(n)
                             } else if (dist.x=="lognorm") {
                               x<-rlogitnorm(n,-1,1)
                             } else if (dist.x=="beta"){
                               x <- rbeta(n,b.param.1,b.param.2)
                             }
                           } else {
                             if (dist.x=="unif"){
                               x <- (sample(1:k, n, replace=TRUE) - 0.5)/k
                             } else if (dist.x=="lognorm") {
                               pdf <- logitnorm::dlogitnorm(((1:k)-0.5)/k,-1,1)
                               prob <- pdf/sum(pdf)
                               x <- (sample(1:k, n, replace=TRUE, prob=prob) - 0.5)/k
                             }  else if (dist.x=="beta") {
                               pdf <- dbeta(((1:k)-0.5)/k,b.param.1,b.param.2)
                               prob <- pdf/sum(pdf)
                               x <- (sample(1:k, n, replace=TRUE, prob=prob) - 0.5)/k
                             }
                           }

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

                           round <-
                           calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = 3, nc = F)

                           round.nc <-
                           calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = 3, nc = T)

                           yb <-
                           calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "YB")

                           isotonic.bands <-
                             bind_rows(
                               bandinfo(
                                 obj=round, dist.x = dist.x, s=s, k=k, setup = misspec, n=n
                                 )$out,
                               bandinfo(
                                 obj=round.nc, dist.x = dist.x, s=s, k=k, setup = misspec, n=n
                                 )$out %>%
                                 mutate(method="round.nc"),
                               bandinfo(
                                 obj=yb, dist.x = dist.x, s=s, k=k, setup = misspec, n=n
                                 )$out
                           )


                           # GIVITI
                           belt <- givitiCalibrationBelt(o=dat$y, e=dat$pr, devel = "external", confLevels = c(1-al))

                          givit <-
                             tibble(x_=belt$seqP,
                                    upr = belt$cbBoundByConfLevel[[1]]$U,
                                    lwr = belt$cbBoundByConfLevel[[1]]$L
                             ) %>%
                             filter(
                               x_ >= min(dat$pr)  & x_ <= max(dat$pr) # cut at lowest and highest fcast value, giviti does that by default
                             ) %>%
                             mutate(
                               s=s,
                               misspec=misspec,
                               condp.at.x = ifelse(misspec=="kink", pc(x_), p(x_,s)),
                               diag.in.band = ifelse(x_ <= upr & x_ >= lwr, 1, 0),#=1 if in
                               cep.in.band = ifelse(upr >= condp.at.x & lwr <= condp.at.x,1,0),#=1 if in
                               width = upr - lwr
                               ) %>%
                             summarise(
                               diag.in.band = all(diag.in.band==1),
                               cep.in.band = all(cep.in.band==1),
                               avg.width01 = mean(width)
                             )


                           out.givit <-
                             tibble(
                               coversCEP = givit$cep.in.band,
                               width01 = givit$avg.width01
                               #a.calibration = givit$diag.in.band
                             ) %>%
                             tidyr::pivot_longer(cols = c(coversCEP:width01),
                                                 names_to = "label",
                                                 values_to = "value")%>%
                             mutate(x=NA) %>%
                             mutate(
                               method = "GiViTI" ,
                               dist.x = dist.x,
                               s=s,
                               k=k,
                               setup = misspec,
                               n=n
                             )

                           df.append <-
                             bind_rows(
                               isotonic.bands,
                               out.givit
                             ) %>%
                             mutate(
                               seed = i
                             )

                           df <- bind_rows(df, df.append)



                         }# misspec loop
                       }# s loop
                      }#k loop
                   }#n loop
                   df
                 } # par loop
stopCluster(cl)
end.time <- Sys.time()
(run.time <- end.time-start.time)# Time difference of 18.46819 hours, 80 cores


check <- sapply(MCsim, function(x) inherits(x, 'error'))
out <- MCsim[!check]
MCsim[check]
dat <-do.call("rbind", out)
dat



saveRDS(MCsim, file = paste("sim_xdist_",dist.x,".rds", sep = ""))
saveRDS(dat, file = paste("sim_rbind_xdist_",dist.x,".rds", sep = ""))


Fig_3 <- dat %>%
  filter(k==Inf) %>%
  select(!k) %>%
  filter(
    label=="coversCEP",
    method %in% c("round","GiViTI")
  )
saveRDS(Fig_3 , file = "Fig_3.RDS")


Fig_4 <- dat %>%
  filter(k==Inf) %>%
  select(!k) %>%
  filter(
    s==.5,
    label!="coversCEP",
    method %in% c("round.nc","YB")
  )
saveRDS(Fig_4, file = "Fig_4.RDS")


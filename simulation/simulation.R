rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(logitnorm)
library(doMC)
library(doParallel)
library(givitiR)
source("helpers.R")
# install calibrationband library
devtools::install_github("https://github.com/marius-cp/calibrationband"
                         ,ref="main"
                         # insert your access token below
                         ,auth_token = "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"#"5514ebdc4ba3a82b44b3298be724b8369dae5bc3"
                         , dependencies = T
                         )
library(calibrationband)

# set of sample sizes
n.set <- 2^seq(9,15,1)
# restictions on the number of unique prediction values
k.set <- c(20, Inf)
# misspecification set
s.set <- seq(0,1,.1)
# DGP (functional form of misspecification)
misspec.set <- c("S", "Step", "DGJ", "kink", "disc")
# distribution of prediction values (unif, logit-normal, beta)
dist.x <-"unif"
# parameters for beta distribution
b.param.1 <- .5
b.param.2 <- 2
# significance level
al = .05

# s=.5
# misspec = "S"
# k=Inf
# dist.x= "unif"
# n=10000


# set up parallel
core.max <- 60
cl <-  min(parallel::detectCores()-1, core.max)
#registerDoMC(cores=cl)
#getDoParWorkers()

cl <- makeCluster(min(parallel::detectCores()-1, core.max) )
registerDoParallel(cl)

start.time <- Sys.time()

MCsim <- foreach(i = 1:500,
                 .errorhandling = "pass",
                 .packages=c("calibrationband", "givitiR",
                             "dplyr", "tibble", "logitnorm"
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
                               p = 1/(2*sstar)+(1/(sstar))*floor(sstar*x) - as.numeric(x==1)*(1/(sstar))#( 1/(2*s)+(1/s)*floor(s*x))
                               return(p)}
                             dat <- tibble(pr=x, s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)
                           } else if (misspec=="DGJ") {
                             p <- function(x,s){p = x^(1-s);return(p)}
                             dat <- tibble(pr=x, s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)
                           } else if (misspec=="kink") {#kink
                             #s=.5
                             pc <- approxfun(y = c(0, 0.2, 1), x = c(0, 0.2+0.8*s , 1), ties = min)
                             dat <- tibble(pr=x,s=s, cep = pc(pr), y=rbinom(n,1,cep))%>% arrange(pr)
                             #plot(y=p,x=pr)
                             #abline(a=0,b=1)
                           } else if (misspec=="disc") {
                             p <- function(x,s){
                               p= as.numeric(x<=.1|x>=.9)*x + as.numeric(x>.1 & x<.9)*((s/2)+x*(.5-s/2)/.5)
                               return(p)}

                             dat <- tibble(pr=x,s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)

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

                           #all.equal(
                           #bandinfo(obj=round, dist.x = dist.x, s=s, k=k, setup = misspec, n=n)$band.info$cep.at.x,
                           #pc(round$bands$x[ifelse(round$bands$x >= min(dat$pr)  & round$bands$x <= max(dat$pr) , TRUE,FALSE)])
                           #)



                           # function for GIVITI
                           belt <- givitiCalibrationBelt(o=dat$y, e=dat$pr, devel = "external", confLevels = c(1-al))

                           # sometimes shots the error (kink,s=.4, beta n=2048):
                           # Error in optim(par = theta, fn = fun, gr = grad, control = control.optim,  : initial value in 'vmmin' is not finite
                           givit <-
                             tibble(x_=belt$seqP,
                                    upr = belt$cbBoundByConfLevel[[1]]$U,
                                    lwr = belt$cbBoundByConfLevel[[1]]$L
                             ) %>%
                             filter(
                               x_ >= min(dat$pr)  & x_ <= max(dat$pr) # cut at lowest and highest fcast value
                             ) %>%
                             mutate(
                               s=s,
                               misspec=misspec,
                               condp.at.x = ifelse(misspec=="kink", pc(x_), p(x_,s)),
                               diag.in.band = ifelse(x_ <= upr & x_ >= lwr, 1, 0),#=1 if in
                               cep.in.band = ifelse(upr >= condp.at.x & lwr <= condp.at.x,1,0),#=1 if in
                               width = upr - lwr) %>%
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
                       #  }#dist loop
                     }#k loop
                   }#n loop
                   df
                 } # par loop
stopCluster(cl)
end.time <- Sys.time()
(run.time <- end.time-start.time)#500 reps, 100 cores, 10 hours for runif


check <- sapply(MCsim, function(x) inherits(x, 'error'))
out <- MCsim[!check]
MCsim[check]
dat <-do.call("rbind", out)
dat

dat %>% filter(method != "GiViTI") %>% filter(label=="coversCEP") %>% summarise(mean(value))

saveRDS(MCsim, file = paste("sim_xdist_A_",dist.x,".rds", sep = ""))

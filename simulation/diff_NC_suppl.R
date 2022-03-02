s=1
misspec = "disc"
k=20
dist.x= "unif"
n=2^11; n
n=2^12; n

set.seed(907)
set.seed(26)

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
al=.05
round <-
  calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = 3, nc = F)
round

round$bands %>%  mutate(
  cross = as.numeric(lwr > upr)
) %>% filter(cross==1)

round.nc <-
  calibration_bands(x=dat$pr, y=dat$y,alpha=al, method = "round", digits = 3, nc = T)
round.nc

ggpubr::ggarrange(autoplot(round), autoplot(round.nc))

autoplot(round.nc)

bandinfo(
  obj=round, dist.x = dist.x, s=s, k=k, setup = misspec, n=n
)
bandinfo(
  obj=round.nc, dist.x = dist.x, s=s, k=k, setup = misspec, n=n
)



# differences between bands ----

dif.in.round <-
  dat %>%
  filter(
    method%in%c("round","round.nc"),
    label == "width"
  ) %>%
  tidyr::pivot_wider(
    names_from = method,
    values_from = value
  ) %>%
  mutate(
    dif.round = as.numeric(round!=round.nc),
    relativ = round.nc/round,
    n=log2(n)
  ) %>%
  filter(
    #n %in% c(8192,32768),
    dif.round ==1
  ) %>%
  group_by(
    dist.x,
    n,
    k,
    s,
    setup
  ) %>%
  mutate("distrinct draws" = as.factor(n_distinct(seed)),
         s= as.factor(s))

dif.in.round %>%   group_by(n) %>%
  # .[complete.cases(.),] %>%
  ggplot(aes(x=n,y=(relativ), shape = `distrinct draws`, color = s))+
  geom_point()+
  facet_grid(k~setup, scales = "free_y")+
  scale_x_continuous(
    breaks = seq(min(dat.cov$n),max(dat.cov$n),2),
    labels = c(2^seq(min(dat.cov$n),max(dat.cov$n),2))
  )
# If there is a difference between the width of CB and CBnc, then obviously CBnc is wider. We here plot the relative difference between the two methods.



# wir haben bei nc methode immer round+fcasts values... klar ist der durchschnittl breite über alle x (fcast values) anders, da wir die riund mit dazu nehmen


ggg <-
dat %>%
  filter(setup == "S" & method == "GiViTI" & label  == "coversCEP" & k==Inf ) %>%
  group_by(
    dist.x,
    n,
    k,
    s,
    setup,
    method
  ) %>%
  summarise(
    cov.rate = mean(value)
  )


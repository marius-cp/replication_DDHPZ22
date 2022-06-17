rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)

misspec.set <- c("S", "Step", "DGJ", "kink", "disc", "noiso")
s.set <- c(0.3,0.7)
b.param.1 <- .5
b.param.2 <- 2

df=tibble()
n=5000
x <-seq(0,1, length.out=n)

for (s in s.set) {
  for (misspec in misspec.set) {

    if(misspec=="S"){
      p <- function(x,s){p = 1/(1+((1/x*(1-x))^(s+1)));return(p)}
      y = rbinom(n, 1, p(x,s))
      pr = x
      } else if (misspec=="Step"){
        p <- function(x,s){
        sstar = 15-10*s
        p = 1/(2*sstar)+(1/(sstar))*floor(sstar*x) - as.numeric(x==1)*(1/(sstar))
        return(p)
        }
        y <- rbinom(n, 1, p(x,s))
        pr = x
      } else if (misspec=="DGJ") {
          p <- function(x,s){p = x^(1-s);return(p)}
          y <- rbinom(n, 1, p(x,s))
          pr = x
          } else if (misspec=="disc") {
            p <- function(x,s){
              p= as.numeric(x<=.1|x>=.9)*x + as.numeric(x>.1 & x<.9)*((s/2)+x*(.5-s/2)/.5)
              return(p)
              }
            y <- rbinom(n, 1, p(x,s))
            pr = x
          } else if (misspec == "noiso"){
            p <- function(x,s){
              a<-(2*s-1)# diagonal if -1, "max" non iso -1
              c<-4*(a+1)
              p =.5-a*(x-.5)+c*(x-.5)^3
              return(p)
              }
        dat <- tibble(pr=x, s=s, cep = p(pr,s), y=rbinom(n,1,cep))%>% arrange(pr)
      }
     else {
              pc <- approxfun(y = c(0, 0.2, 1), x = c(0, 0.2+0.8*s , 1), ties = min)
              p <- pc(x)
              pr <- x
              y = rbinom(n, 1,p)
              }

     if(misspec=="DGJ" & s==1) next


    df.append <-
      tibble(
        misspec = misspec,
        y=y,#outcome
        p= ifelse(misspec=="kink", pc(x),p(x,s)),#CEP
        pr=pr,#prediction
        N = n,
        s = s
        )
    df <- bind_rows(df, df.append)

  }
}


df.p <- df %>%  mutate(s = as.factor(s))

step <-
  df.p %>% filter(misspec == "Step") %>%
  group_by(p,s) %>%
  summarise(mi =min(pr), ma=max(pr), misspec="Step") %>% arrange(s,p)




df.p <-
  bind_rows(df.p %>%  filter (misspec!="Step"), step)  %>%
  mutate(
    misspec = recode_factor(
      misspec,
      DGJ = "Monomial",
      S = "S-shaped",
      kink = "Kink",
      disc = "Disc",
      Step = "Step",
      noiso = "Wave"
    )
  )

si=.75

p <-
  ggplot(df.p)+
  geom_abline(intercept=0,slope=1, colour ="gray", size = .5)+
  geom_line(df.p %>%
              filter(misspec == "S-shaped" | misspec == "Monomial" | misspec == "Kink" | misspec == "Wave"),
            mapping = aes(x=pr,y=p, colour = s,  linetype=s), size = si)+
  geom_line(df.p %>% filter(misspec == "Disc" & pr <.1),
            mapping = aes(x=pr,y=p, colour = s,  linetype=s), size = si)+
  geom_line(df.p %>% filter(misspec == "Disc" & pr >.9),
            mapping = aes(x=pr,y=p, colour = s,  linetype=s), size = si)+
  geom_line(df.p %>% filter(misspec == "Disc" & pr <=.9  & pr >=.1 ),
            mapping = aes(x=pr,y=p, colour = s,  linetype=s), size = si)+
  geom_segment(df.p %>% filter(misspec == "Step"),
               mapping=aes(x=mi,y=p,yend=p,xend=ma, color = s, linetype=s), size = si)+
  facet_grid(.~misspec)+
  theme_bw()+
  theme(
    legend.position = "none",#"bottom",
    legend.text = element_text(
      size = 10,
      colour = "black",
      margin = margin(r = 20, unit = "pt")
      ),
    legend.title = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust=1,
      colour = "black",
      size = 10
        ),
    axis.text.y = element_text(colour = "black", size = 10),
    strip.text = element_text(face = "bold", size = 10),
    legend.key.width = unit(3, "line"),
    legend.spacing.x = unit(1, "mm"),
    plot.title = element_text(size = 10)
    )+
  guides(
    colour= guide_legend(
      nrow=1,
      title = expression(paste('shape parameter ', italic(s), ':     '))
    ),
    linetype= guide_legend(
      nrow=1,
      title = expression(paste('shape parameter ', italic(s), ':     '))
    )
    )+
  xlab(expression(paste('Forecast value ', italic(x))))+
  ylab(expression(paste('Calibration curve ', italic(p))))+
  scale_linetype_manual(values=c( "solid", "dotted", "12"))+
  scale_color_brewer(palette = "Set1")+
  coord_fixed(ratio =1)


p


ggsave("Fig_2.pdf", width = (10+2.5), height = 3)


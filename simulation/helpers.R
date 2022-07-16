bandinfo = function(obj,
                    dist.x,
                    s,
                    k,
                    setup,
                    n
                    ){
  band.info <-
    obj$bands %>%
    mutate(
      s=s,
      misspec=setup,
      cep.at.x = ifelse(misspec=="kink", pc(x), p(x,s)),
      cep.in.band = ifelse(upr >= cep.at.x & lwr <= cep.at.x,1,0),#=1 if in
      diag.in.band = ifelse(x <= upr & x >= lwr, 1, 0),#=1 if in
      width = upr - lwr
    ) %>%
    filter(
      x >= min(obj$cases$x)  & x <= max(obj$cases$x) # cut at lowest and highest fcast value
    )

  capture <-
    band.info %>%
    summarise(
      #diag.in.band = all(diag.in.band==1),
      cep.in.band = all(cep.in.band==1),
      avg.width01 = mean(width),
      crossing = any(width <0)
    )


  # use the bands at the filtered x to calculate the width at 0.02,0.04,0.1,...,0.98
  lwr_f <- approxfun(x=band.info$x, y= band.info$lwr, method = "constant", f=0)
  upr_f <- approxfun(x=band.info$x, y= band.info$upr, method = "constant", f=1)

  width.at <-
    tibble(
      x_ = seq(0.02,.98,1/25) ,
      lwr.at.x_ = lwr_f(x_),
      upr.at.x_ = upr_f(x_),
      width = upr.at.x_-lwr.at.x_
    )

  out <-
    tibble(
      coversCEP = capture$cep.in.band,
      crossing = capture$crossing,
      width01 = capture$avg.width01,
      #a.calibration = capture$diag.in.band
    ) %>%
    tidyr::pivot_longer(cols = c(coversCEP:width01),#cols = c(coversCEP:a.calibration),
                        names_to = "label",
                        values_to = "value")%>%
    mutate(x=NA) %>%
    add_row(tibble(label="width", value = width.at$width , x = width.at$x_)) %>%
    mutate(
      method = obj$method,
      dist.x = dist.x,
      s=s,
      k=k,
      setup = setup,
      n=n
    )
return(list(out=out, band.info=band.info, capture=capture, width.at=width.at))
}



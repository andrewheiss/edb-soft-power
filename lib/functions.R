# Helpful functions
library(tidyverse)
library(sandwich)
library(lmtest)

# ggplot theme
theme_edb <- function(base_size=9, base_family="Clear Sans Light") {
  update_geom_defaults("label", list(family="Clear Sans Light"))
  update_geom_defaults("text", list(family="Clear Sans Light"))
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          title=element_text(vjust=1.2, family="Clear Sans", face="bold"),
          plot.subtitle=element_text(family="Clear Sans Light"),
          plot.caption=element_text(family="Clear Sans Light",
                                    size=rel(0.8), colour="grey70"),
          panel.border = element_blank(), 
          axis.line=element_line(colour="grey50", size=0.2),
          #panel.grid=element_blank(), 
          axis.ticks=element_blank(),
          legend.position="bottom", 
          legend.title=element_text(size=rel(0.8)),
          axis.title=element_text(size=rel(0.8), family="Clear Sans", face="bold"),
          strip.text=element_text(size=rel(1), family="Clear Sans", face="bold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.spacing.y=unit(1.5, "lines"),
          legend.key=element_blank(),
          legend.spacing=unit(0.2, "lines"))
  
  ret
}

# Plot interrupted time series
plot.its <- function(model, var.name, var.title, y.title, plot.year = 2005) {
  # summary_dots <- list(
  #   n = ~ n(),
  #   variable = interp(~ mean(val, na.rm = T), val=as.name(var.name)),
  #   stdev = interp(~ sd(val, na.rm = T), val = as.name(var.name))
  # )
  
  if (plot.year == 2005) {
    plot.data <- edb.its %>%
      group_by(year.centered.2005) %>%
      summarise_(variable = interp(~mean(var, na.rm=TRUE), var=as.name(var.name)))
    
    newdata <- data_frame(year.centered.2005 = seq(min(edb.its$year.centered.2005),
                                                   max(edb.its$year.centered.2005), by=1),
                          ranked.2005 = year.centered.2005 >= 0,
                          gdpcap_ln_lag = mean(edb.its$gdpcap_ln_lag, na.rm=TRUE),
                          gdpgrowth_lag = mean(edb.its$gdpgrowth_lag, na.rm=TRUE),
                          pop_ln_lag = mean(edb.its$pop_ln_lag, na.rm=TRUE))
    
    plot.predict <- augment(model, newdata=newdata) %>% 
      rename(variable = .fitted,
             year.centered = year.centered.2005,
             ranked = ranked.2005)
  } else {
    plot.data <- edb.its %>%
      group_by(year.centered.2006) %>%
      summarise_(variable = interp(~mean(var, na.rm=TRUE), var=as.name(var.name)))
    
    newdata <- data_frame(year.centered.2006 = seq(min(edb.its$year.centered.2006),
                                                   max(edb.its$year.centered.2006), by=1),
                          ranked.2006 = year.centered.2006 >= 0,
                          gdpcap_ln_lag = mean(edb.its$gdpcap_ln_lag, na.rm=TRUE),
                          gdpgrowth_lag = mean(edb.its$gdpgrowth_lag, na.rm=TRUE),
                          pop_ln_lag = mean(edb.its$pop_ln_lag, na.rm=TRUE))
    
    plot.predict <- augment(model, newdata=newdata) %>% 
      rename(variable = .fitted,
             year.centered = year.centered.2006,
             ranked = ranked.2006)
  }
  
  ggplot(plot.predict, aes(x=year.centered, y=variable)) +
    geom_line() +
    geom_line(data=plot.predict, aes(colour=ranked), size=0.75) +
    geom_vline(xintercept=0) +
    scale_colour_manual(values=c("#0073D9", "#CC3340"),
                        labels=c("Not ranked    ", "Ranked"),
                        name=NULL) +
    labs(title=var.title, y=y.title, x=paste("Years since", plot.year)) +
    theme_edb()
}

# Calculate clustered robust standard errors
robust.clusterify <- function(model, dat, cluster) {
  attach(dat, warn.conflicts = F)
  not <- attr(model$model,"na.action")
  
  if( ! is.null(not)) {  # only drop the NA values if there are any left
    cluster <- cluster[-not]
    dat <- dat[-not,]
  }
  
  with(dat, {
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj <- apply(estfun(model),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
    coefs <- coeftest(model, vcovCL, type="HC1")  # HC1 or HC0 are close to Stata
    return(list(clcov=vcovCL, coefs=coefs))
  })
}

summary.corr <- function(x, y=NULL) {
  out <- data_frame(avg = mean(x, na.rm=TRUE), 
                    stdev = sd(x, na.rm=TRUE), 
                    std.error = stdev / sqrt(length(x)), 
                    lower = avg + (qnorm(0.025) * std.error), 
                    upper = avg + (qnorm(0.975) * std.error))
  
  if(length(y) > 0) {
    out$correlation <- cor(x, y, use="complete.obs")
  }
  
  out
}


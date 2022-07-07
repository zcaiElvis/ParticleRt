library("tidyverse")
library("dplyr")
library("zoo")


plot_case_Rt <- function(cases, R_t){
  compare_case_rt <- data.frame(idx = 1:nrow(cases), case=cases$New_cases, rt <- R_t$pred.mean.x)
  compare_case_rt %>%
    pivot_longer(cols=c(2,3))%>%
    ggplot(aes(x = idx))+
    geom_line(aes(y=value))+
    facet_wrap(facets = vars(name), scales="free_y")
  }





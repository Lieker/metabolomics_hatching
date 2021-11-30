hatch_summ <- function(dpi = "all",
                       input = "input/hatching_raw.csv") {
  source("scripts/hatch_calc.R")
  
  summ <- hatch_calc(dpi = dpi, input = input)

  summ <- summ %>% group_by(treatment) %>%
    summarize(mean_hatch = mean(hatchingpercent),
              stdev = sd(hatchingpercent),
              sterr = as.numeric(std.error(hatchingpercent)))
  
  return(summ)
}
  
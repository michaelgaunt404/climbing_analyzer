
library(tidymodels)

nested_boots = peak_force() %>%
  group_by(grip_type, hand) %>%
  nest() %>%
  mutate(boot = map(data,~{

    bootstraps(.x, times = 2000, apparent = TRUE) %>%
      mutate(model = map(splits, ~lm(value~rep_number , data = .x))
             ,coef_info = map(model, tidy)
             ,aug = map(model, augment))
  }))

nested_boots %>%
  unnest(cols = "boot") %>%
  unnest(cols = "coef_info") %>%
  ungroup() %>%
  filter(grip_type == "halfcrimp") %>%
  filter(term == "rep_number") %>%
  ggplot(aes(estimate)) +
  geom_histogram() +
  facet_grid(rows = vars(p.value < .05))
  geom_line(aes(y = .fitted, group = id), alpha = .1, col = "blue") +
  geom_point() +
  facet_grid(rows = vars(grip_type))

nested_boots %>%
  unnest(cols = "boot") %>%
  unnest(cols = "aug") %>%
  ungroup() %>%
  filter(grip_type == "halfcrimp") %>%
  ggplot( aes(rep_number , value )) +
  geom_line(aes(y = .fitted, group = id), alpha = .1, col = "blue") +
  geom_point() +
  facet_grid(rows = vars(grip_type))


lm(hp~disp, mtcars) %>%
  # tidy()
  augment()
boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)
boots


fit_nls_on_bootstrap <- function(split) {
  nls(mpg ~ k / wt + b, analysis(split), start = list(k = 1, b = 0))
}

boot_models <-
  boots %>%
  mutate(model = map(splits, fit_nls_on_bootstrap),
         coef_info = map(model, tidy))

boot_coefs <-
  boot_models %>%
  unnest(coef_info)

percentile_intervals <- int_pctl(boot_models, coef_info)
percentile_intervals


time = peak_force() %>%
  filter(grip_type  == "halfcrimp") %>%
  filter(hand == "right") %>%
  pull(value)

boot::tsboot(time, statistic = mean, R = 100)


lynx.fun <- function(tsb) {
  ar.fit <- ar(tsb, order.max = 25)
  tt = c(ar.fit$order, mean(tsb), tsb) %>%
    print()

  ttt <<- tt

  return(tt)
}

no_func = function(tsb){return(tsb)}

lynx.1 <- tsboot(log(lynx), lynx.fun, R = 1000, l = 20, sim = "geom")
lynx_2 <- tsboot(log(lynx), no_func, R = 1, l = 20, sim = "geom")
lynx.1$t0

data.frame(
  raw = log(lynx) %>% as.vector()
  ,ar = ttt[-c(1:2)]
  ,avg = lynx.1$t0[-c(1:2)]
) %>%
  mutate(seq = row_number()) %>%
  pivot_longer(
    cols = c(raw, ar, avg)
  ) %>%
  ggplot() +
  geom_line(aes(seq, value, color = name)) +
  facet_grid(rows = vars(name))
  geom_point(aes(seq, ar), color = "red")


log(lynx) %>% as.vector()

plot()

lynx.1$t0
lynx.1$t[,1] %>% mean()
lynx.1$data

lynx.1$



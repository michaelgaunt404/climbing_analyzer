weights <- c(70, 72, 71, 68, 74, 75, 69, 73, 72, 70)

# Frequentist Approach - Confidence Interval
set.seed(123)  # Setting seed for reproducibility
n_bootstrap <- 1000  # Number of bootstrap samples

# Bootstrap sampling and calculation of means
bootstrap_means <- replicate(n_bootstrap, mean(sample(weights, replace = TRUE)))

# Confidence interval (e.g., 95%)
conf_interval <- quantile(bootstrap_means, c(0.025, 0.975))

# Bayesian Approach - Credible Interval
# For simplicity, assuming a non-informative flat prior
# This is a basic illustration and may not be suitable for all cases

# Bootstrap sampling and calculation of means
bayesian_bootstrap_means <- replicate(n_bootstrap, mean(sample(weights, replace = TRUE)))



df = data.frame(weights = c(70, 72, 71, 68, 74, 75, 69, 73, 72, 70))

df %>%
  specify(response = weights) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>%
  get_confidence_interval(
    level = 0.95
  )


heights <- c(183, 192, 182, 183, 177, 185, 188, 188, 182, 185)

install.packages("bayesboot")
library(bayesboot)
b1 <- bayesboot(weights, mean, R = 8000, R2 = 10)
summary(b1)

plot(b1)


n_bootstrap <- 1000  # Number of bootstrap samples

# Prior distribution
prior_mean <- 73
prior_sd <- 5
prior_samples <- rnorm(n_bootstrap, mean = prior_mean, sd = prior_sd)

# Bootstrap sampling and calculation of means
bayesian_bootstrap_means <- numeric(n_bootstrap)
for (i in 1:n_bootstrap) {
  sampled_weights <- sample(weights, replace = TRUE)
  bayesian_bootstrap_means[i] <- mean(sampled_weights) + prior_samples[i]
}

# Credible interval (e.g., 95%)
credible_interval <- quantile(bayesian_bootstrap_means, c(0.025, 0.975))




#boot strap method---------------------------------------------------------

install.packages("tidymodels")
library(tidymodels)

tt_t = peak_force() %>%
  filter(grip_type != "john") %>%
  group_by(grip_type, hand) %>%
  nest() %>%
  mutate(boot = map(data, ~bootstraps(.x, times = 5000))) %>%
  dplyr::select(!data) %>%
  unnest(cols = boot) %>%
  mutate(boot_stats = map(splits, ~{

    temp_data = analysis(.x)$value

    data.frame(
      mean = mean(temp_data)
      ,sd = sd(temp_data)
    )
  }
  )) %>%
  unnest(cols = boot_stats)

quantiles = c(.025, .0975)

tt_t %>% summarise(mean = mean(mean))

conf = tt_t %>%
  summarise(
    across(c("mean"),
           purrr::map(
             quantiles,
             ~purrr::partial(DescTools::Quantile, probs = .x)
           ), .names = "{.col}_{quantiles}"
    )) %>%
  ungroup()


tt_t %>%
  ggplot() +
  geom_histogram(aes(mean, fill = hand), position = "dodge", binwidth = 1) +
  geom_ribbon(data = conf, aes(y = 0, ymax = Inf,
                               x = mean_0.025 ,
                               xmax = mean_0.0975)) +
  facet_grid(rows = vars(grip_type, hand), scales = "free")



tt_t %>%
  summarise_at(
    .vars = vars(mean, sd), .funs = list(q_ = qauntile)
    ,probs  = c(.9))


  summarize(vars(-group_cols(),
                   purrr::map(
                     quantiles,
                     ~ purrr::partial(
                       DescTools::Quantile,
                       probs = .x
                     )
                   ),
                   .names = "{.col}_qt_{quantiles}"
  )) %>%
  ungroup()
summarise(across(c('mean', 'sd'), quantile, .05))

peak_force() %>%
  group_by(grip_type, hand) %>%
  nest() %>%
  mutate(boot = map(data,~{

    .x %>%
      specify(response = value) %>%
      generate(reps = 1000, type = "bootstrap") %>%
      calculate(stat = "mean") %>%
      get_confidence_interval(
        level = 0.95
      )
  })) %>%
  unnest(cols = "boot")
mutate(stats = )

specify(response = value) %>%
  # Generate bootstrap samples
  generate(reps = 1000, type = "bootstrap") %>%
  # Calculate mean of each bootstrap sample
  calculate(stat = "mean") %>%
  get_confidence_interval(
    level = 0.95
  )

# bayes bootstrap method--------------------------------------------------------
bayes_processed = peak_force() %>%
  group_by(grip_type, hand) %>%
  nest() %>%
  mutate(bayes_boots = map(data
                           ,~{
                             boots = bayesboot(.x$value, weighted.mean, R = 4000, R2 = 3000, use.weights = T)
                             boots_smmry = boots %>% summary()

                             boots_smmry_df = data.frame(
                               values = boots_smmry$value
                               ,names = boots_smmry$measure) %>%
                               pivot_wider(names_from = names, values_from = values)

                             return(boots_smmry_df)
                           }
  )) %>%
  ungroup() %>%
  unnest(cols = bayes_boots) %>%
  mutate(across(where(is.double), \(x) round(x, 0))) %>%
  dplyr::select(!data)

temp_plot = bayes_processed %>%
  mutate(label_hand = str_glue("Average: {mean}")
         ,label_hdi = str_glue("HDI Band\nLow: {hdi.low}\nHigh: {hdi.high}")) %>%
  group_by(grip_type) %>%
  group_map(~{
    plot_ly(.x) %>%
      add_segments(
        x = ~hdi.low, xend = ~hdi.high,
        y = ~hand, yend = ~hand, color = ~hand
        ,showlegend = (.y == "halfcrimp")
        ,legendgroup = ~hand
        ,text = ~label_hdi, hoverinfo = "text"
      ) %>%
      add_markers(x = ~mean, y = ~hand, showlegend = F, size = 23
                  ,text = ~label_hand, hoverinfo = "text") %>%
      layout(yaxis = list(
        titlefont = list(size = 11)
        ,title = paste0(
          c(rep("&nbsp;", 20),
            paste("<b>", as.character(.y), "</b>"),
            rep("&nbsp;", 20)),
          collapse = "")
      ))
  }) %>%
subplot(
  .,
  nrows = length(.), margin = .05, shareX = T, shareY = T, titleY = T) %>%
  layout(
    xaxis = list(title = "Output Force (Lbf)")
  )




#boot strap method---------------------------------------------------------





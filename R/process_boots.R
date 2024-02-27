
process_boots = function(data){
# data = peak_force()

temp_data = data %>%
  group_by(grip_type, hand) %>%
  nest() %>%
  mutate(bayes_boots = map(data
                           ,~{
                             boots = bayesboot(.x$value, weighted.mean, R = 4000, use.weights = T)
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

return(temp_data)

}

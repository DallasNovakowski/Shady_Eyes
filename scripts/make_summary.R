make_summary <- function(data, dv, grouping1, grouping2, grouping3){
  data %>%
    group_by({{grouping1}},{{grouping2}},{{grouping3}}) %>%
    dplyr::summarise(
      n = n(),
      mean = round(mean({{dv}}, na.rm = TRUE),1),
      sd = sd({{dv}}, na.rm = TRUE),
      se = round(sd({{dv}}, na.rm = TRUE),1) /sqrt(n()),
      min = round(mean({{dv}}, na.rm = TRUE),1) - qnorm(0.975) * round(sd({{dv}}, na.rm = TRUE),1) /
        sqrt(n()),
      max = round(mean({{dv}}, na.rm = TRUE),1) + qnorm(0.975) * round(sd({{dv}}, na.rm = TRUE),1) /
        sqrt(n()),
      p25 = round(quantile({{dv}}, 0.25)),
      p50 = round(median({{dv}})),
      p75 = round(quantile({{dv}}, 0.75))
    )
}


#string variables
make_summary_string <- function(data, dv, grouping1, grouping2, grouping3){
  data %>%
    group_by(.data[[grouping1]],.data[[grouping2]],.data[[grouping3]]) %>%
    dplyr::summarise(
      n = n(),
      mean = round(mean(.data[[dv]], na.rm = TRUE),1),
      sd = sd(.data[[dv]], na.rm = TRUE),
      se = round(sd(.data[[dv]], na.rm = TRUE),1) /sqrt(n()),
      min = round(mean(.data[[dv]], na.rm = TRUE),1) - qnorm(0.975) * round(sd(.data[[dv]], na.rm = TRUE),1) /
        sqrt(n()),
      max = round(mean(.data[[dv]], na.rm = TRUE),1) + qnorm(0.975) * round(sd(.data[[dv]], na.rm = TRUE),1) /
        sqrt(n()),
      p25 = round(quantile(.data[[dv]], 0.25)),
      p50 = round(median(.data[[dv]])),
      p75 = round(quantile(.data[[dv]], 0.75))
    )
}


#merging regular summary stats with emmeans
merge_emmeans_summary <- function(summary_data,emmeans_tidy){
  summary_data$emmean <- emmeans_tidy$emmean
  summary_data$emmean_se <- emmeans_tidy$SE
  summary_data$emmean_loci <- emmeans_tidy$lower.CL
  summary_data$emmean_upci <- emmeans_tidy$upper.CL
  
  ## in hindsight, I'm not sure if this bit makes sense or is useful
  # summary_data <- transform(summary_data, emmean_sd = `emmean_se`*`n`, check.names = FALSE)   
  
  summary_data
}

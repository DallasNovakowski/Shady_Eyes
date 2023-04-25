

summarize_and_anova <- function(studyname, data, dv, grouping1, grouping2, grouping3){

  # dv_summary_data <- make_summary(data=data, dv={{dv}}, grouping1 = {{grouping1}}, grouping2 = {{grouping2}}, grouping3 = {{grouping3}})
  
  # anova fit - easystats style
  dv_fit <- aov(data = data, dv ~ grouping1*grouping2*grouping3)
  
  
  # dv_fit <- aov(data = data, data[[dv]] ~ data[[grouping1]]*data[[grouping2]]*data[[grouping3]])
  
  # produces output with string, but creates term values that are all messed
  # dv_fit <- aov(data = data, get(dv) ~ get(grouping1)*get(grouping2)*get(grouping3))
  
  # dv_fit <- aov({{dv}} ~ {{grouping1}}*{{grouping2}}*{{grouping3}})
  

  #----normality tests
  # 
  # #plot residuals and fitted
  # plot(dv_fit, 1)
  # 
  # #levene test, Homogeneity of Variance
  # leveneTest(data[[dv]] ~ data[[grouping1]]*data[[grouping2]]*data[[grouping3]], data = data)
  # 
  # #QQplot for residuals
  # plot(dv_fit, 2)
  # 
  # #assign residuals
  # dv_residuals <- residuals(object = dv_fit)
  # 
  # # Run Shapiro-Wilk test
  # shapiro.test(x = dv_residuals)
  # 
  
  
  
  #analyze anova
  # dv_anova <- car::Anova(dv_fit, type = 2)  ## emmeans Can't handle an object of class  “anova” 
  # # 
  # # #report results
  # # dv_anova %>%
  # #   report::report()
  # # 
  # # # retrieve other effects and stats
  # # sjstats::anova_stats(dv_anova)
  # # 
  # 
  # 
  # 
  # 
  # #Compute estimated marginal means
  # dv_emmeans_full <- emmeans::emmeans(dv_fit, specs = pairwise ~ get(grouping1):get(grouping3):get(grouping2))
  # 
  # data[[grouping1]]:data[[grouping3]]:data[[grouping2]]
  # dv_emmeans_tidy <- as.data.frame(dv_emmeans_full$emmeans)
  # 
  # # dv_emmeans_tidy <- dv_emmeans_tidy %>% rename(emmean_se = SE, ci_lo = lower.CL, ci_hi = upper.CL, emmean_df = df)
  # 
  # dv_emmeans_contrasts <- as.data.frame(dv_emmeans_full$contrasts)
  # 
  # 
  # dv_summary_data <- dv_summary_data[order(dv_summary_data$data[[grouping1]], dv_summary_data$data[[grouping3]]), ]
  # 
  # dv_emmeans_tidy <- dv_emmeans_tidy[order(dv_emmeans_tidy$data[[grouping1]], dv_emmeans_tidy$data[[grouping3]]), ]
  # 
  # dv_summary_data <- merge_emmeans_summary(summary_data=dv_summary_data,
  #                                          emmeans_tidy=dv_emmeans_tidy)
  # 
  # 
  # 
  # 
  
  return(list(#dv_summary_data, 
              dv_fit
              # , dv_anova
              # , dv_emmeans_full
              #, dv_emmeans_contrasts
              ))

}











# multipetal <- function(df, n) {
#   mutate(df, "petal.{n}" := Petal.Width * n)
# }





my_data <- data.frame(data$dv,data$grouping1, data$grouping2, data$grouping3)  

dv_fit <- aov(data = my_data, dv ~ grouping1*grouping2*grouping3)



#this one thing works
summary_data <- make_summary(data=data, dv={{dv}}, grouping1 = {{grouping1}}, grouping2 = {{grouping2}}, grouping3 = {{grouping3}})
summary_data




#pairwise comparisons
# TukeyHSD(dv_fit, which = "{{grouping1}}")




the_list <- list(summary_data, dv_emmeans_contrasts, dv_anova, dv_fit)

the_list





# obj_to_string <- function(v1) {
#   return(deparse(substitute(v1)))
# }

# string_dv <- obj_to_string({{dv}})


my_list_new <- c(my_list, L3 = list(my_element))   # Add element to list
my_list_new 

varname <- paste0(studyname, "_", string_dv)
assign(varname, 1:100, envir =.GlobalEnv)


# assign(paste0("variable_", studyname), 1:5)
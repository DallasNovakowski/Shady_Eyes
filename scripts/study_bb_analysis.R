
#----libraries-and-scripts----
library(haven) # read_sav
library(tidyverse)
library(surveytoolbox) # varl_tb
library(sjPlot) # view_df


#----import-data----
data <- haven::read_sav(here::here("data","shade_may_2017.sav"))


#----view-data-labels----

# view_df(data)


data_labels <- data %>%
  # .[,1:20] %>%
  surveytoolbox::varl_tb()


#----recode-data-labels----

data_labels$var_label <-  str_replace(data_labels$var_label, "Please rate the person pictured above on the following scales.-", "")

data_labels$var_label <-  str_replace(data_labels$var_label, 
                                      "This is important! Read carefully, please. In this study, we are interested in mental stimulat-", "atn_")

# remove everything before hyphen
data_labels$var_label[218:292] <- sub(".*\\-","",
                                        data_labels$var_label[218:292])

# remove everything before colon
data_labels$var_label<- sub(".*\\:","",
                                      data_labels$var_label)

# Remove everything after "variable_, and replace with dv"
data_labels$var <- str_replace(data_labels$var, "(Male_).*", paste0("Male_",data_labels$var_label))

data_labels$var <- str_replace(data_labels$var, "(Female_).*", paste0("Female_",data_labels$var_label))

data_labels$var <- str_replace(data_labels$var, "(Clear_)\\d+(.*)", paste0("Clear_",data_labels$var_label))

data_labels$var <- str_replace(data_labels$var, "(Half_)\\d+(.*)*", paste0("Half_",data_labels$var_label))

data_labels$var <- str_replace(data_labels$var, "(Full_)\\d+(.*)*", paste0("Full_",data_labels$var_label))



# rename misc variables with more informative labels
data_labels$var[1:12] <- data_labels$var_label[1:12]

data_labels$var[16:32] <- data_labels$var_label[16:32]



#----tidy-variable-names----

#clean variable names
data_labels$var <- janitor::make_clean_names(data_labels$var)

#replace dataset names with cleaned and clearer versions
names(data) <- data_labels$var

# rename demographic questions
data <- rename(data,  
               age = q31,
               eyeglasses = q32,
               sunglasses = q33,
               employed = q36,
               income = q37,
               education = q38,
               completion  = q39,
               
               clear_unprofessional = dv1c,
               clear_authentic = dv2c,
               clear_allsubmissive = dv3c,
               clear_allnotpushy = dv4c,
               
               half_unprofessional = dv1h,
               half_authentic = dv2h,
               half_allsubmissive = dv3h,
               half_allnotpushy = dv4h,    
               
               full_unprofessional = dv1f,
               full_authentic = dv2f,
               full_allsubmissive = dv3f,
               full_allnotpushy = dv4f,
               )

names(data) <-  str_replace(names(data), 
                                      "not_pushy", "notpushy")

names(data) <-  str_replace(names(data), 
                            "credibile", "credible")

names(data) <-  str_replace(names(data), 
                            "peacefull", "peaceful")

names(data) <-  str_replace(names(data), 
                            "geniune", "genuine")

#----reclass-variables----

data[,c("gender", "eyeglasses","sunglasses","income","education","completion")] <- 
  lapply(data[,c("gender", "eyeglasses","sunglasses","income","education","completion")], factor)

data$gender <- dplyr::recode(data$gender, "1" = "male" , "2" = "female")


# data[,c("age")] <- 
#   lapply(data[,c("age")], numeric)

data$age <- as.numeric(data$age)



#----attention-checks----

#filter for paperclip attention check, reducing n from 698 to 600
data <- filter(data, data$atn_paper_clip == 1)

# Did the person pictured in previous page wear glasses?
nottwo <- function(x) {
  x != 2
}

data <- data %>% 
  filter(if_all(c("mani_clear_f1", "mani_half_f1", "mani_full_f1", "mani_clear_m1", "mani_half_m1", "mani_full_m1"), nottwo ))

nrow(data)


# write.csv(data, file=here::here("data","shade_may_2017.csv"))




# ---- make-long-data ----

data <- data %>%
  select(contains(c("gender", "eyeglasses","sunglasses","income","education","completion", "full_", "clear_", "half_")))

data <- data %>%
  select(-contains(c("mani_", "clear_female", "half_female", "full_female", "clear_male", "half_male", "full_male")))


# data <- data %>%
#   select(contains(c("gender", "eyeglasses","sunglasses","income","education","completion", 
#                     "allsubmissive", "allnotpushy", "unprofessional", "authentic"
#                     )))
# 

data_long <- data %>%
  pivot_longer(cols = starts_with(c("clear_","half_","full_")), 
               names_to = c("glasses_shade",".value"), names_sep = "_")

# data %>% 
#   pivot_longer(-c(gender),
#                names_to = c("glasses_shade", ".value"), 
#                names_sep = "_")


# dvs <- colnames(data[grepl('security_consumed', colnames(data))])

# data_long <- data %>% pivot_longer(c("dv1c", "dv1h", "dv1f"), names_to = "glasses_shade", values_to = "unprofessional")

# data_long <- data %>% pivot_longer(consumed_vars, names_to = "round_num", values_to = "security_consumed")
# 
# data_long$security_spending <- data_long$security_consumed*2


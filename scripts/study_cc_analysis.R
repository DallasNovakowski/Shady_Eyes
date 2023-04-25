# Study 4 analysis

library(tidyverse)
library(repmis)
library(corrplot)
library(data.table)
library(psych)
library(Hmisc)

library(parallel)
library(nFactors)
library(polycor)
library(GPArotation)

library(repmis)
library(ggpubr)
library(car)
library("dplyr")
library(ggstatsplot)
library(ggthemes)

# library(ggpp) #dodge2nudge()
library(colorspace) #lighten()
library(ggdist)
library(cowplot)


source(file=here::here("scripts","make_summary.R"))


cbPalette <-c("#999999","#E69F00", "#56B4E9","#009E73",
              "#F0E442", "#0072B2", "#D55E00","#CC79A7")

cbPalette <- lighten(cbPalette, amount = 0.6, space = "HLS")


# source_data("https://github.com/DallasNovakowski/Shady_Eyes/blob/master/sunglasses_study4_clean1_11_26_2019.Rdata?raw=true")


#----read-data----
data <- read.csv(here::here("data","Legacy_SunglassesStudy4June_2017_namedheaders.csv"))

initial_n <- nrow(data)

#----screening-data----

#filter for paperclip attention check, reducing n from 698 to 600
data <- filter(data, data$atn_paperclip == 1)
nrow(data)

#"Please carefully look at this advertisement and then answer the following questions. Make sure you read all the information in the ad."
#Reduces n from 600 to 589
data <- filter(data, data$careful_check == 1)
nrow(data)

#using the Q42 as a filter (item that describes hypotheses- equivalent to reaching the finish of the study), reduces n from 589 to 564
data <- filter(data, data$Q42 == 1)
nrow(data)

# data <- filter(data, data$Finished == 1)
# nrow(data)

# data <- filter(data, data$p_glasses == 1)
# nrow(data)


# using the finished variable seems to be too conservative: generally, participants that scored 0 on the "Finished" variable had responses for all the variables
# Moreover, using the item "p_glasses" ("Did the person featured in the advertisement wear glasses?") might not be appropriate as a filter: some respondents 
# seemed to have interpreted the item as referring to perscription (i.e., clear) glasses
# With this in mind, let's use Q42 as our stopping point for attention checks


ggplot(data = data, aes(x = time_submit)) + geom_histogram(binwidth=1)

data_timeoutliers <- filter(data, data$time_submit < 100)
ggplot(data = data_timeoutliers, aes(x = time_submit)) +geom_histogram(binwidth=1)

# Looks like a group of respondents took < 10 seconds to look at the ad, let's filter this group out and run the analyses on the remainder

data <- filter(data, data$time_submit > 10)
nrow(data)
data <- as_tibble(data)

## So now we have a filtered sample of 492 (unless we also want to exclude people that took excessively long)



#Let's make some variable lists for easier data management down the road

#----grouping-variables----

data$model_pushyr <- as.numeric(dplyr::recode(data$model_pushy, "1" = "7" , "2" = "6", "3" = "5", 
                                              "4" = "4", "5" = "3", "6" = "2", "7" = "1"))

VarBase <- list()
VarBase$conditions <- c('female_clear', 'male_clear', 'female_med', 'female_dark', 'male_med', 'male_dark')
VarBase$demo <- c('gender','age', 'home_lang', 'english_length_nohome', 'wear_glasses', 'employment', 'annual_income', 'education')
VarBase$adrate <- names(data)[30:40]
VarBase$productrate <- names(data)[41:52]
VarBase$modelpers <- c("model_competent", "model_pushyr", names(data)[55:77])
VarBase$modelrate <- names(data)[78:90]
VarBase$consum <- names(data)[92:94]
VarBase$adsbeliefs <- names(data)[95:102]
VarBase$manipchecks <- names(data)[103:105]


#With the legacy pull in qualtrics, participants' assigned condition is coded as a binary variable, 
#but we need variables with 2+ levels (namely, shade), so let's create some new variables for analysis


#----recoding-variables----

#change missing values to zeros for condition assignments
data[,c(VarBase$conditions)][is.na(data[,c(VarBase$conditions)])] <- 0

#create variable for male model (1 = male, 0 = female)
data$model_gender <- rowSums(cbind(data$male_clear, data$male_med, data$male_dark))

data$clear_shade <- rowSums(cbind(data$male_clear, data$female_clear))
data$med_shade <- rowSums(cbind(data$male_med, data$female_med))
data$dark_shade <- rowSums(cbind(data$male_dark, data$female_dark))

VarBase$shadiness <- c('clear_shade', 'med_shade', 'dark_shade')

#create single variable for shadiness
condition_coding <- function(x, y, z){
  if(x == 1) shade <- 0 else if (y == 1) shade <- 1 else if (z==1) shade <- 2 else shade <- NA
}

data$shade <- mapply(condition_coding, x = data$clear_shade, y = data$med_shade, z = data$dark_shade)


VarBase$factors <- c('model_gender', 'shade')
VarBase$all <- c(VarBase$demo, VarBase$factors, VarBase$shadiness, VarBase$manipchecks, 
                 VarBase$adsbeliefs, VarBase$consum, VarBase$modelrate, VarBase$productrate, VarBase$adrate, VarBase$conditions)

#Now that we've 1) filtered for attention check, time to read the ad, and finishing the survey, 
#2) made some variable lists, and 3) made some new variables, let's put make a new dataset that doesn't make my eyes bleed as much


data <- data[,c(VarBase$demo, VarBase$factors, VarBase$shadiness, VarBase$manipchecks, VarBase$adsbeliefs, VarBase$consum, VarBase$modelpers, VarBase$modelrate, VarBase$productrate, VarBase$adrate, VarBase$conditions)]

# write.csv(data_reduced1, file=here::here("output","sunglasses_Study4_clean1_11_26_2019.csv"))
#           
# save(data_reduced1, VarBase, file=here::here("output","sunglasses_study4_clean1_11_26_2019.Rdata"))
#      
     # file="C:/Users/dalla/Google Drive/R Coding/Shady_Eyes/sunglasses_study4_clean1_11_26_2019.Rdata")



# ---data-exploration----

data <- tibble::rowid_to_column(data, "pnum")

#age
hist(data$age)

age_mean <- mean(data$age)
age_mean

age_sd <- sd(data$age)
age_sd


#gender
table(data$gender)

gen_tab <- table(data$gender)
gen_tab


table(data$employment)
hist(data$education)
hist(data$annual_income)


table(data$shade, data$male_model)


table(data$male_model)

table(data$shade)

hist(data$recommend)

ggplot(data, aes(recommend)) + geom_histogram()

ggplot(data, aes(wtb)) + geom_histogram()


data$value_assess <- as.character(data$value_assess)

data$value_assess <- sub(",","",data$value_assess)

data$value_assess_num <- as.numeric(data$value_assess)
VarBase$consum <- c(VarBase$consum, "value_assess_num")

ggplot(data, aes(value_assess_num)) + geom_histogram(binwidth=50)

ggplot(data, aes(y=value_assess_num)) + geom_boxplot()



# There are some substantial outliers in the value assessment variable (arguably to be labeled WTP). 
#If we run analyses on this variable, these values are likely to have high residuals and subsequently have high influence on the estimated slope


### SO: let's filter out those observations with value assessments >= $2000

data <- filter(data, data$value_assess_num < 2000)

ggplot(data, aes(y=value_assess_num)) + geom_boxplot()

nrow(data)




model_cor <-  cor(data[,VarBase$modelpers])

nrow(data)

data_reduced3 <- data[complete.cases(data),]
nrow(data_reduced3)


data <- data[complete.cases(data),]

condition_assignments <- table(data$shade, data$male_model, data$gender)


nona_model_cor <-  cor(data[,VarBase$modelpers])


###----factor-analysis-prep-and-EFA-1----


ev <- eigen(nona_model_cor)  
ap <- nFactors::parallel(subject=468, var=nrow(nona_model_cor), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

print(nS)
nS$Components$nparallel

nfact <- nS$Components$nparallel

modelpers_fa <- fa(r=nona_model_cor, covar=T, n.obs=nrow(data), nfactors=nfact, rotate="promax")

print(modelpers_fa, digits=2, cutoff=.3, sort=T)
print(modelpers_fa$loadings, digits=2, cutoff=.3, sort=T)



###----EFA-2-removing-cross-loaded-items----

VarBase$modelpers_nocross <- c("model_pushyr", "model_genuine", "model_fake", "model_sincere", "model_suspicious","model_phony", "model_professional","model_assertive", "model_knowledgeable", "model_experienced", "model_peaceful", "model_calm", "model_honest", "model_friendly", "model_respectful")

nocross_model_cor <-  cor(data[,VarBase$modelpers_nocross])

nocross_model_pers_fa <- fa(r=nocross_model_cor, covar=T, n.obs=nrow(data), nfactors=nfact, rotate="promax")


print(nocross_model_pers_fa, digits=2, cutoff=.3, sort=T)
print(nocross_model_pers_fa$loadings, digits=2, cutoff=.3, sort=T)



###----EFA-3-attempt-at-further-refinement----

VarBase$modelpers_nocross2 <- c("model_pushyr", "model_genuine", "model_fake", "model_sincere", "model_suspicious","model_phony", "model_professional","model_assertive", "model_knowledgeable", "model_experienced", "model_peaceful", "model_calm", "model_honest", "model_friendly")

nocross2_model_cor <-  cor(data[,VarBase$modelpers_nocross2])

nocross2_model_pers_fa <- fa(r=nocross2_model_cor, covar=T, n.obs=nrow(data), nfactors=nfact, rotate="promax")

print(nocross2_model_pers_fa, digits=2, cutoff=.3, sort=T)
print(nocross2_model_pers_fa$loadings, digits=2, cutoff=.3, sort=T)


## model attributes EFA summary

#Looks like MR2 is always going to have some degree of cross-loading, so let's go with EFA 2 for future analyses: it at least gives us 3 unique loadings.
# Call them MR1 = phony, MR2 = approachable, MR3 = expertise

fs <- factor.scores(data[,VarBase$modelpers_nocross], nocross_model_pers_fa)
fs <- fs$scores
data <- cbind(data, fs)

setnames(data, old = c("MR1", "MR2", "MR3"), new = c("phony", "approachable", "expertise"))

data <-dplyr::select (data, -c(VarBase$modelpers))
write.csv(data, here::here("output","sunglasses_Study4_clean2_11_27_2019.csv"))

save(data, VarBase, file=here::here("output","sunglasses_study4_clean2_11_27_2019.Rdata"))


## ----final-data-prep----

data$gender <- as.factor(data$gender)


data$shade <- factor(data$shade, 
                              levels = c(0, 1, 2),
                              labels = c("Clear", "Medium", "Dark"))

data$shade_num <- as.numeric(data$shade)

str(data$male_model)

data$model_gender <- factor(data$model_gender, 
                                   levels = c(0, 1),
                                   labels = c("Female Model", "Male Model"))

table(data$shade, data$model_gender, data$gender)


data_1 <- filter(data, data$gender == 1)
table_1 <- table(data_1$shade, data_1$model_gender)
# flextable::flextable(as.data.frame.matrix(table_1))


flextable_1<- table_1 %>% 
  as.data.frame.matrix() %>% 
  add_rownames() %>% 
  flextable::flextable() %>%
  flextable::save_as_docx( path = "scc_gender_1_flextable.docx")

data_2 <- filter(data, data$gender == 2)
table_2 <- table(data_2$shade, data_2$model_gender)
table_2 %>% 
  as.data.frame.matrix() %>% 
  add_rownames() %>% 
  flextable::flextable() %>%
  flextable::save_as_docx( path = "scc_gender_2_flextable.docx")





# ---- analysis -------------------------------------------------------------------




## ----phony----

# summarize stats on phony responses by condition
scc_phony_summary_data <- make_summary(data=data, dv=phony, grouping1 = shade, grouping2 = gender, grouping3 = model_gender)

# phony anova fit - easystats style
scc_phony_fit <- aov(phony ~ shade*model_gender*gender, data = data)

###----normality tests

#plot residuals and fitted
plot(scc_phony_fit, 1)

#levene test, Homogeneity of Variance
leveneTest(phony ~ shade*model_gender*gender, data = data)

#QQplot for residuals
plot(scc_phony_fit, 2)

#assign residuals
phony_residuals <- residuals(object = scc_phony_fit)

# Run Shapiro-Wilk test
shapiro.test(x = phony_residuals)




#analyze anova
scc_phony_anova <- car::Anova(scc_phony_fit, type = 2)  ## emmeans Can't handle an object of class  “anova” 

#report results
scc_phony_anova %>%
  report::report()

# retrieve other effects and stats
sjstats::anova_stats(scc_phony_anova)

#pairwise comparisons
# TukeyHSD(scc_phony_fit, which = "shade")




#Compute estimated marginal means
scc_phony_emmeans_full <- emmeans::emmeans(scc_phony_fit, specs = pairwise ~ shade:gender:model_gender)

scc_phony_emmeans_tidy <- as.data.frame(scc_phony_emmeans_full$emmeans)

scc_phony_emmeans_contrasts <- as.data.frame(scc_phony_emmeans_full$contrasts)


scc_phony_summary_data <- scc_phony_summary_data[order(scc_phony_summary_data$shade, scc_phony_summary_data$gender), ]

scc_phony_emmeans_tidy <- scc_phony_emmeans_tidy[order(scc_phony_emmeans_tidy$shade, scc_phony_emmeans_tidy$gender), ]

scc_phony_summary_data <- merge_emmeans_summary(summary_data=scc_phony_summary_data,
                                                    emmeans_tidy=scc_phony_emmeans_tidy)

scc_phony_summary_data

# means <- modelbased::estimate_means(scc_phony_fit, at = c("shade", "gender", "model_gender"))



#EZ
# scc_phony_anova_ez <-  afex::aov_ez(id = 'pnum',
                                           # dv='phony',
                                           # data=data,
                                           # type=2,
                                           # between = c('shade', "model_gender"))



# report::report_statistics(res.aov.phony)

#Mehdi's style
# but see https://talklab.psy.gla.ac.uk/tvw/catpred/#coding-scheme for notes on deviation coding

# scc_phony_lm <- lm(phony ~ shade*model_gender, data = data)
# # summary(scc_phony_lm)
# Anova(scc_phony_lm, type = 3) %>%
#   report::report()

# emmeans::emmeans(scc_phony_lm, ~ shade, contr = "pairwise", adjust = "bonferroni")


#treat shade as a numeric variable
scc_phony_lm_num <- lm(phony ~ shade_num*model_gender*gender, data = data)
summary(scc_phony_lm_num)





##----wtb----

# summarize stats on wtb responses by condition
scc_wtb_summary_data <- make_summary(data=data, dv=wtb, grouping1 = shade, grouping2 = gender, grouping3 = model_gender)

# wtb anova fit - easystats style
scc_wtb_fit <- aov(wtb ~ shade*model_gender*gender, data = data)



###----normality tests

#plot residuals and fitted
plot(scc_wtb_fit, 1)

#levene test, Homogeneity of Variance
leveneTest(wtb ~ shade*model_gender*gender, data = data)

#QQplot for residuals
plot(scc_wtb_fit, 2)

#assign residuals
wtb_residuals <- residuals(object = scc_wtb_fit)

# Run Shapiro-Wilk test
shapiro.test(x = wtb_residuals)




###----analyze anova
scc_wtb_anova <- car::Anova(scc_wtb_fit, type = 2)  ## emmeans Can't handle an object of class  “anova” 

#report results
scc_wtb_anova %>%
  report::report()

# retrieve other effects and stats
sjstats::anova_stats(scc_wtb_anova)

#pairwise comparisons
# TukeyHSD(scc_wtb_fit, which = "shade")



###----Compute estimated marginal means
scc_wtb_emmeans_full <- emmeans::emmeans(scc_wtb_fit, specs = pairwise ~ shade:gender:model_gender)

scc_wtb_emmeans_tidy <- as.data.frame(scc_wtb_emmeans_full$emmeans)

scc_wtb_emmeans_contrasts <- as.data.frame(scc_wtb_emmeans_full$contrasts)


scc_wtb_summary_data <- scc_wtb_summary_data[order(scc_wtb_summary_data$shade, scc_wtb_summary_data$gender), ]

scc_wtb_emmeans_tidy <- scc_wtb_emmeans_tidy[order(scc_wtb_emmeans_tidy$shade, scc_wtb_emmeans_tidy$gender), ]

scc_wtb_summary_data <- merge_emmeans_summary(summary_data=scc_wtb_summary_data,
                                                emmeans_tidy=scc_wtb_emmeans_tidy)

scc_wtb_summary_data


scc_wtb_lm_num <- lm(wtb ~ shade_num*model_gender*gender, data = data)
summary(scc_wtb_lm_num)




















#---- value_assess_num (Perceived perceived product value) ----


# summarize stats on value_assess_num responses by condition
scc_value_assess_num_summary_data <- make_summary(data=data, dv=value_assess_num, grouping1 = shade, grouping2 = gender, grouping3 = model_gender)

# value_assess_num anova fit - easystats style
scc_value_assess_num_fit <- aov(value_assess_num ~ shade*model_gender*gender, data = data)



###----normality tests

#plot residuals and fitted
plot(scc_value_assess_num_fit, 1)

#levene test, Homogeneity of Variance
leveneTest(value_assess_num ~ shade*model_gender*gender, data = data)

#QQplot for residuals
plot(scc_value_assess_num_fit, 2)

#assign residuals
value_assess_num_residuals <- residuals(object = scc_value_assess_num_fit)

# Run Shapiro-Wilk test
shapiro.test(x = value_assess_num_residuals)




###----analyze anova
scc_value_assess_num_anova <- car::Anova(scc_value_assess_num_fit, type = 2)  ## emmeans Can't handle an object of class  “anova” 
scc_value_assess_num_anova

#report results
scc_value_assess_num_anova %>%
  report::report()

# retrieve other effects and stats
sjstats::anova_stats(scc_value_assess_num_anova)

#pairwise comparisons
# TukeyHSD(scc_value_assess_num_fit, which = "shade")



###----Compute estimated marginal means
scc_value_assess_num_emmeans_full <- emmeans::emmeans(scc_value_assess_num_fit, specs = pairwise ~ shade:gender:model_gender)

scc_value_assess_num_emmeans_tidy <- as.data.frame(scc_value_assess_num_emmeans_full$emmeans)

scc_value_assess_num_emmeans_contrasts <- as.data.frame(scc_value_assess_num_emmeans_full$contrasts)


scc_value_assess_num_summary_data <- scc_value_assess_num_summary_data[order(scc_value_assess_num_summary_data$shade, scc_value_assess_num_summary_data$gender), ]

scc_value_assess_num_emmeans_tidy <- scc_value_assess_num_emmeans_tidy[order(scc_value_assess_num_emmeans_tidy$shade, scc_value_assess_num_emmeans_tidy$gender), ]

scc_value_assess_num_summary_data <- merge_emmeans_summary(summary_data=scc_value_assess_num_summary_data,
                                              emmeans_tidy=scc_value_assess_num_emmeans_tidy)

scc_value_assess_num_summary_data


scc_value_assess_num_lm_num <- lm(value_assess_num ~ shade_num*model_gender*gender, data = data)
summary(scc_value_assess_num_lm_num)










# ----recommend----


# summarize stats on recommend responses by condition
scc_recommend_summary_data <- make_summary(data=data, dv=recommend, grouping1 = shade, grouping2 = gender, grouping3 = model_gender)

# recommend anova fit - easystats style
scc_recommend_fit <- aov(recommend ~ shade*model_gender*gender, data = data)



###----normality tests

#plot residuals and fitted
plot(scc_recommend_fit, 1)

#levene test, Homogeneity of Variance
leveneTest(recommend ~ shade*model_gender*gender, data = data)

#QQplot for residuals
plot(scc_recommend_fit, 2)

#assign residuals
recommend_residuals <- residuals(object = scc_recommend_fit)

# Run Shapiro-Wilk test
shapiro.test(x = recommend_residuals)




###----analyze anova
scc_recommend_anova <- car::Anova(scc_recommend_fit, type = 2)  ## emmeans Can't handle an object of class  “anova” 

#report results
scc_recommend_anova %>%
  report::report()

# retrieve other effects and stats
sjstats::anova_stats(scc_recommend_anova)

#pairwise comparisons
# TukeyHSD(scc_recommend_fit, which = "shade")



###----Compute estimated marginal means
scc_recommend_emmeans_full <- emmeans::emmeans(scc_recommend_fit, specs = pairwise ~ shade:gender:model_gender)

scc_recommend_emmeans_tidy <- as.data.frame(scc_recommend_emmeans_full$emmeans)

scc_recommend_emmeans_contrasts <- as.data.frame(scc_recommend_emmeans_full$contrasts)


scc_recommend_summary_data <- scc_recommend_summary_data[order(scc_recommend_summary_data$shade, scc_recommend_summary_data$gender), ]

scc_recommend_emmeans_tidy <- scc_recommend_emmeans_tidy[order(scc_recommend_emmeans_tidy$shade, scc_recommend_emmeans_tidy$gender), ]

scc_recommend_summary_data <- merge_emmeans_summary(summary_data=scc_recommend_summary_data,
                                              emmeans_tidy=scc_recommend_emmeans_tidy)

scc_recommend_summary_data


scc_recommend_lm_num <- lm(recommend ~ shade_num*model_gender*gender, data = data)
summary(scc_recommend_lm_num)




#----visualization----

# recommendation

scc_recommend_shade_viofade <- ggplot(data = data, 
                              aes(y = recommend, x = shade, fill = cbPalette[2])) +  
  # density slab
  stat_slab(side = "both", adjust = 2, scale = 0.5,show.legend = F,
            aes(fill_ramp = stat(level)),.width = c(.50, 1)) + 
  #dot-whisker
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.05,show.legend = F, color = "grey40") +
  stat_summary(fun=mean, geom="point", size=2,show.legend = F, color = "grey40") +
  #mean text
  stat_summary(aes(label=round(..y..,1)), fun=mean, geom="text", size=5,
               vjust = 5) +
  
    stat_compare_means(comparisons = shade_comparisons, label.y = c(5.5, 6.2, 5)) + # Add pairwise comparisons p-value
  
  # theming
  ylab("Likelihood to recommend") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(1, 7), breaks = seq(1,7,3)) +
  theme(axis.title.y = element_text(size = rel(.7))) +
  theme_half_open() +  
  theme(axis.title = element_text(face="bold")) +
  scale_fill_ramp_discrete(range = c(0.5, 1), aesthetics = "fill_ramp")+
  scale_colour_manual(values = cbPalette[2], aesthetics = c("colour","fill")) +  
  guides(fill_ramp = "none") 

scc_recommend_shade_viofade


scc_recommend_gender_viofade <- ggplot(data = data, 
                               aes(y = recommend, x = model_gender, fill = cbPalette[2])) +  
  # density slab
  stat_slab(side = "both", adjust = 2, scale = 0.5,show.legend = F,
            aes(fill_ramp = stat(level)),.width = c(.50, 1)) + 
  #dot-whisker
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.05,show.legend = F, color = "grey40") +
  stat_summary(fun=mean, geom="point", size=2,show.legend = F, color = "grey40") +
  #mean text
  stat_summary(aes(label=round(..y..,1)), fun=mean, geom="text", size=5,
               vjust = 5) +
  
  stat_compare_means(label.y = 6.5) +
  
  # theming
  ylab("Likelihood to recommend") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(1, 7), breaks = seq(1,7,3)) +
  theme(axis.title.y = element_text(size = rel(.7))) +
  theme_half_open() +  
  theme(axis.title = element_text(face="bold")) +
  scale_fill_ramp_discrete(range = c(0.5, 1), aesthetics = "fill_ramp")+
  scale_colour_manual(values = cbPalette[2], aesthetics = c("colour","fill")) +  
  guides(fill_ramp = "none")

scc_recommend_gender_viofade



scc_recommend_int_viofade <- ggplot(data = data, 
                                      aes(y = recommend, 
                                          x = shade,
                                          fill = model_gender)) +
  
  stat_slab(side = "both", adjust = 2, scale = 0.5,
          position = position_dodge(width = .7), aes(fill_ramp = stat(level)),.width = c(.50, 1)) +
  
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.05,show.legend = F, color = "grey40", 
               position = position_dodge(width = .7)) +
  
  stat_summary(fun=mean, geom="point", size=2,show.legend = F, color = "grey40", 
               position = position_dodge(width = .7)) +
  #mean text
  stat_summary(aes(label=round(..y..,1)), fun=mean, geom="text",  
               position = position_dodge(width = .7), size=4, vjust = 5) +
  
  
  
  # ggpubr::geom_bracket(inherit.aes = FALSE, tip.length = 0.02, hjust = .5, #vjust = .6,
  #                      xmin = 2, xmax = 3, y.position = 6,label.size = 2.1,
  #                      label = paste0(scc_recommend_emmeans_contrasts[scc_recommend_emmeans_contrasts$contrast == 
  #                                                                       'unequal_merit - unequal_random', "p"])
  # ) + 
  
  
  theme_half_open() +  facet_grid(~ gender) + panel_border() + 
  
  
ylab("Likelihood to recommend") +
  xlab("Eyeglasses shade") +
  labs(fill = "Model gender") +

  scale_fill_ramp_discrete(range = c(0.5, 1), aesthetics = "fill_ramp")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(1, 7), breaks = seq(1,7,3)) +
  scale_colour_manual(values = cbPalette, aesthetics = c("colour","fill"))+
  guides(fill_ramp = "none") +

  theme(
    axis.title = element_text(face="bold", size = rel(1)),
    legend.title = element_text(face="bold", size = rel(1)),
        legend.position = "top",
    legend.justification = "center") 

# axis.title.y = element_text(size = rel(.7)),


scc_recommend_int_viofade

ggsave(here::here("figures", "scc_recommend_int_viofade.png"),
       width=10, height = 4.5, dpi=1000)


scc_value_assess_int_viofade <- ggplot(data = data, 
                                        aes(y = value_assess_num, 
                                            x = shade,
                                            fill = model_gender)) +
  
  stat_slab(side = "both", adjust = 2, scale = 0.6,
            position = position_dodge(width = .7), aes(fill_ramp = stat(level)),.width = c(.50, 1)) +
  
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.05,show.legend = F, color = "grey40", 
               position = position_dodge(width = .7)) +
  
  stat_summary(fun=mean, geom="point", size=2,show.legend = F, color = "grey40", 
               position = position_dodge(width = .7)) +
  #mean text
  stat_summary(aes(label=round(..y..,1)), fun=mean, geom="text",  
               position = position_dodge(width = .7), size=3.3, vjust = 4) +
  
  facet_grid(~ gender) +  
  
  
  ylab("Value assessment ($)") +
  xlab("Eyeglasses shade") +
  labs(fill = "Model gender") +
  
  scale_fill_ramp_discrete(range = c(0.5, 1), aesthetics = "fill_ramp")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, 1600)) +
  scale_colour_manual(values = cbPalette, aesthetics = c("colour","fill"))+
  guides(fill_ramp = "none") +  
  
  theme_half_open() +   panel_border() +
  
  theme(
    axis.title = element_text(face="bold", size = rel(1)),
    legend.title = element_text(face="bold", size = rel(1)),
    legend.position = 'top',
    legend.justification = "center"
    # legend.direction = "horizontal", 
    # legend.box = "horizontal"
    )


# , fill = guide_legend(title.position = "left", 
#                       hjust = 0.5, #centres the title horizontally
#                       title.hjust = 0.5) 


scc_value_assess_int_viofade

ggsave(here::here("figures", "scc_value_assess_int_viofade.png"),
       width=10, height = 4.5, dpi=1000)



scc_phony_int_viofade <- ggplot(data = data, 
                                        aes(y = phony, 
                                            x = shade,
                                            fill = model_gender)) +
  
  stat_slab(side = "both", adjust = 2, scale = 0.5,
            position = position_dodge(width = .7), aes(fill_ramp = stat(level)),.width = c(.50, 1)) +
  
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.05,show.legend = F, color = "grey40", 
               position = position_dodge(width = .7)) +
  
  stat_summary(fun=mean, geom="point", size=2,show.legend = F, color = "grey40", 
               position = position_dodge(width = .7)) +
  #mean text
  stat_summary(aes(label=round(..y..,1)), fun=mean, geom="text", 
               position = position_dodge(width = .7), size=4, vjust = 5) +
  
  theme_half_open() +  facet_grid(~ gender) + panel_border() + 
  
  
  ylab("Phony perceptions") +
  xlab("Eyeglasses shade") +
  labs(fill = "Model gender") +
  
  # scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(1, 7), breaks = seq(1,7,3)) +
  scale_fill_ramp_discrete(range = c(0.5, 1), aesthetics = "fill_ramp")+
  
  scale_colour_manual(values = cbPalette, aesthetics = c("colour","fill"))+
  guides(fill_ramp = "none") +
  
  theme(
    axis.title = element_text(face="bold", size = rel(1)),
    legend.title = element_text(face="bold", size = rel(1)),
    legend.position = "top",
    legend.justification = "center") 


scc_phony_int_viofade

ggsave(here::here("figures", "scc_phony_int_viofade.png"),
       width=10, height = 4.5, dpi=1000)




scc_wtb_int_viofade <- ggplot(data = data, 
                                    aes(y = wtb, 
                                        x = shade,
                                        fill = model_gender)) +
  
  stat_slab(side = "both", adjust = 2, scale = 0.5,
            position = position_dodge(width = .7), aes(fill_ramp = stat(level)),.width = c(.50, .95, 1)) +
  
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.05,show.legend = F, color = "grey40", 
               position = position_dodge(width = .7)) +
  
  stat_summary(fun=mean, geom="point", size=2,show.legend = F, color = "grey40", 
               position = position_dodge(width = .7)) +
  #mean text
  stat_summary(aes(label=round(..y..,1)), fun=mean, geom="text",  
               position = position_dodge(width = .7), size=4, vjust = 5) +
  
  theme_half_open() +  facet_grid(~ gender) + panel_border() + 
  
  
  ylab("Willingness to buy") +
  xlab("Eyeglasses shade") +
  labs(fill = "Model gender") +
  
  scale_fill_ramp_discrete(#range = c(0.5, 1), 
                           aesthetics = "fill_ramp")+

  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(1, 7), breaks = seq(1,7,3)) +
  scale_colour_manual(values = cbPalette, aesthetics = c("colour","fill"))+
  guides(fill_ramp = "none") +
  
  theme(
    axis.title = element_text(face="bold", size = rel(1)),
    legend.title = element_text(face="bold", size = rel(1)),
    legend.position = "top",
    legend.justification = "center") 


scc_wtb_int_viofade

ggsave(here::here("figures", "scc_wtb_int_viofade.png"),
       width=10, height = 4.5, dpi=1000)



# Summary
##The results indicate significant main effects of model gender and shade on perceived phoniness, with no interaction.
##Neither shade nor model gender had significant effects on willingness to buy, perceived product value, nor likelihood to recommend the product 
library(ggplot2)
library(stringr)
library(cowplot)
library("HH")
library(ordinal)
library(rstatix)
library(sandwich)
library(lmtest)   
library(emmeans)

sessionInfo()

Nboot = 50 ## set to 5000 for full run (it takes a long time though!)

#####################################################################################
###### data #########################################################################

getwd()
dir.create("output_20232024")
setwd("output_20232024")

dat1 <- read.csv("../data/questionnaire/2023/questionnaire_data.csv") ### 2023
dat1$session <- str_split_fixed(as.character(dat1$sheet), "_00", 2)[,1]
head(dat1)

dat2 <- read.csv("../data/questionnaire/2024/questionnaire_data_2024.csv") ### 2024
dat2$session <- str_split_fixed(as.character(dat2$sheet), "_00", 2)[,1]
head(dat2)

length(dat1[,1])
length(dat2[,1])

c(
length(subset(dat1, dat1$session == "Before_lectures") [,1]),
length(subset(dat1, dat1$session == "After_lectures") [,1]),
length(subset(dat1, dat1$session == "After_prac") [,1]),
length(subset(dat2, dat2$session == "Before_lectures") [,1]),
length(subset(dat2, dat2$session == "After_lectures") [,1]),
length(subset(dat2, dat2$session == "After_prac") [,1]))

# 252 278 190 183 169 197

################################################################################
#### info ######################################################################

# Q1. An understanding of insects is important for life scientists.
# Q2. Understanding insects has important practical applications.
# Q3. It is important that I improve my understanding of insects during my degree.
# Q4. Understanding insects will help me get a job after I graduate.
# Q5. I would like to study insects for my final year project.
# Q6. Insects are disgusting.
# Q7. I am afraid of insects.
# Q8. Insects are boring.
# Q9. Insect conservation is important.
# Q10. I am comfortable identifying insects to an order level.
# Q11. I can describe the anatomy of insects well.
# Q12. The practical session helped me to better understand insects.
# Q13. The practical session made me less afraid of insects.

# Degree Which degree course are you studying?
# Gender Which gender do you identify as?


#########################################################################################
### degree course
### Some students do not to the practical so exclude them (ES and OS) - 
### most of these are in 2023 (only one in 2024 as stopped offering it to them)
# wildlife conservation, Applied terrestrial and marine ecology, Enviromental science = ES (do not do the prac)

dat1$degree_type <- ifelse(dat1$Degree == "Biology", "BZ",
                           ifelse(dat1$Degree == "Biology with biotech", "BZ",
                                  ifelse(dat1$Degree == "Zoology", "BZ",
                                         ifelse(dat1$Degree == "Zoology with animal behav", "BZ",
                                                ifelse(dat1$Degree == "Zoology with animal management", "BZ",
                                                       ifelse(dat1$Degree == "Zoology with conservation", "BZ",
                                                              ifelse(dat1$Degree == "Zoology with herp", "BZ",
                                                                     ifelse(dat1$Degree == "Zoology with marine zoology", "BZ",
                                                                            ifelse(dat1$Degree == "Zoology with ornithology", "BZ",
                                                                                   ifelse(dat1$Degree == "Zoology with primatology", "BZ",  "OS/ES"))))))))))

dat1$degree_type <- ifelse(dat1$session == "After_prac", "BZ", dat1$degree_type) ## as some students didn't put the degree down, but are BZ if they came to the prac!
table(dat1$degree_type)


dat2$degree_type <- ifelse(dat2$Degree == "Biology", "BZ",
                           ifelse(dat2$Degree == "Biology with biotech", "BZ",
                                  ifelse(dat2$Degree == "Zoology", "BZ",
                                         ifelse(dat2$Degree == "Zoology with animal behav", "BZ",
                                                ifelse(dat2$Degree == "Zoology with animal management", "BZ",
                                                       ifelse(dat2$Degree == "Zoology with conservation", "BZ",
                                                              ifelse(dat2$Degree == "Zoology with herp", "BZ",
                                                                     ifelse(dat2$Degree == "Zoology with marine zoology", "BZ",
                                                                            ifelse(dat2$Degree == "Zoology with ornithology", "BZ",
                                                                                   ifelse(dat2$Degree == "Zoology with primatology", "BZ",  "OS/ES"))))))))))

dat2$degree_type <- ifelse(dat2$session == "After_prac", "BZ", dat2$degree_type) ## as some students didn't put the degree down, but are BZ if they came to the prac!
table(dat2$degree_type)

subset(dat2, dat2$degree_type == "OS/ES")


#################################################################################################
#### add year

dat1$year <- rep("2023", length(dat1[,1]))
dat2$year <- rep("2024", length(dat2[,1]))

#### join
dat_all <- rbind(dat1, dat2)

############################################################################################################################
## get rid of decimals (turn to NA - there are not many) 
## this is when students filled in two circles.


dat_all_BL_temp <- subset(dat_all, dat_all$session == "Before_lectures")
dat_all_AL_temp <- subset(dat_all, dat_all$session =="After_lectures")    
dat_all_AP_temp <- subset(dat_all, dat_all$session =="After_prac")  
table(c(dat_all$Q1, dat_all$Q2, dat_all$Q2, dat_all$Q3, dat_all$Q4, dat_all$Q5, dat_all$Q6, dat_all$Q7, dat_all$Q8, dat_all$Q9, dat_all$Q10, dat_all$Q11, dat_all$Q12, dat_all$Q13))

sum(table(c(dat_all$Q1, dat_all$Q2, dat_all$Q2, dat_all$Q3, dat_all$Q4, dat_all$Q5, dat_all$Q6, dat_all$Q7, dat_all$Q8, dat_all$Q9, dat_all$Q10, dat_all$Q11, dat_all$Q12, dat_all$Q13)))
#[1] 15956

sum(is.na((c(dat_all_BL_temp$Q1, dat_all_BL_temp$Q2, dat_all_BL_temp$Q2, dat_all_BL_temp$Q3, dat_all_BL_temp$Q4, dat_all_BL_temp$Q5, dat_all_BL_temp$Q6, dat_all_BL_temp$Q7, dat_all_BL_temp$Q8, dat_all_BL_temp$Q9, dat_all_BL_temp$Q10, dat_all_BL_temp$Q11,
             dat_all_AL_temp$Q1, dat_all_AL_temp$Q2, dat_all_AL_temp$Q2, dat_all_AL_temp$Q3, dat_all_AL_temp$Q4, dat_all_AL_temp$Q5, dat_all_AL_temp$Q6, dat_all_AL_temp$Q7, dat_all_AL_temp$Q8, dat_all_AL_temp$Q9, dat_all_AL_temp$Q10, dat_all_AL_temp$Q11,
             dat_all_AP_temp$Q1, dat_all_AP_temp$Q2, dat_all_AP_temp$Q2, dat_all_AP_temp$Q3, dat_all_AP_temp$Q4, dat_all_AP_temp$Q5, dat_all_AP_temp$Q6, dat_all_AP_temp$Q7, dat_all_AP_temp$Q8, dat_all_AP_temp$Q9, dat_all_AP_temp$Q10, dat_all_AP_temp$Q11, dat_all_AP_temp$Q12, dat_all_AP_temp$Q13
))))

## 46 = number of NA responses 

dat_all$Q1  <- ifelse(dat_all$Q1%%1==0,  dat_all$Q1, NA)
dat_all$Q2  <- ifelse(dat_all$Q2%%1==0,  dat_all$Q2, NA)
dat_all$Q3  <- ifelse(dat_all$Q3%%1==0,  dat_all$Q3, NA)
dat_all$Q4  <- ifelse(dat_all$Q4%%1==0,  dat_all$Q4, NA)
dat_all$Q5  <- ifelse(dat_all$Q5%%1==0,  dat_all$Q5, NA)
dat_all$Q6  <- ifelse(dat_all$Q6%%1==0,  dat_all$Q6, NA)
dat_all$Q7  <- ifelse(dat_all$Q7%%1==0,  dat_all$Q7, NA)
dat_all$Q8  <- ifelse(dat_all$Q8%%1==0,  dat_all$Q8, NA)
dat_all$Q9  <- ifelse(dat_all$Q9%%1==0,  dat_all$Q9, NA)
dat_all$Q10 <- ifelse(dat_all$Q10%%1==0, dat_all$Q10, NA)
dat_all$Q11 <- ifelse(dat_all$Q11%%1==0, dat_all$Q11, NA)
dat_all$Q12 <- ifelse(dat_all$Q12%%1==0, dat_all$Q12, NA)
dat_all$Q13 <- ifelse(dat_all$Q13%%1==0, dat_all$Q13, NA)

table(c(dat_all$Q1, dat_all$Q2, dat_all$Q2, dat_all$Q3, dat_all$Q4, dat_all$Q5, dat_all$Q6, dat_all$Q7, dat_all$Q8, dat_all$Q9, dat_all$Q10, dat_all$Q11, dat_all$Q12, dat_all$Q13))


dat_all_BL_temp <- subset(dat_all, dat_all$session == "Before_lectures")
dat_all_AL_temp <- subset(dat_all, dat_all$session =="After_lectures")    
dat_all_AP_temp <- subset(dat_all, dat_all$session =="After_prac")  
sum(is.na((c(dat_all_BL_temp$Q1, dat_all_BL_temp$Q2, dat_all_BL_temp$Q2, dat_all_BL_temp$Q3, dat_all_BL_temp$Q4, dat_all_BL_temp$Q5, dat_all_BL_temp$Q6, dat_all_BL_temp$Q7, dat_all_BL_temp$Q8, dat_all_BL_temp$Q9, dat_all_BL_temp$Q10, dat_all_BL_temp$Q11,
             dat_all_AL_temp$Q1, dat_all_AL_temp$Q2, dat_all_AL_temp$Q2, dat_all_AL_temp$Q3, dat_all_AL_temp$Q4, dat_all_AL_temp$Q5, dat_all_AL_temp$Q6, dat_all_AL_temp$Q7, dat_all_AL_temp$Q8, dat_all_AL_temp$Q9, dat_all_AL_temp$Q10, dat_all_AL_temp$Q11,
             dat_all_AP_temp$Q1, dat_all_AP_temp$Q2, dat_all_AP_temp$Q2, dat_all_AP_temp$Q3, dat_all_AP_temp$Q4, dat_all_AP_temp$Q5, dat_all_AP_temp$Q6, dat_all_AP_temp$Q7, dat_all_AP_temp$Q8, dat_all_AP_temp$Q9, dat_all_AP_temp$Q10, dat_all_AP_temp$Q11, dat_all_AP_temp$Q12, dat_all_AP_temp$Q13
))))

## 70
## 70 - 46 = 24 = number of decimal answers


### filter out non BZ
dat_all_BZ <- subset(dat_all, dat_all$degree_type == "BZ")
length(dat_all [,1])
length(dat_all_BZ[,1])


### degree tidy

dat_all_BZ$Degree <- gsub(" ", "_", dat_all_BZ$Degree)

### gender 
levels(as.factor(dat_all_BZ$Gender))

dat_all_BZ$Gendera <- ifelse(dat_all_BZ$Gender == "boy", "male", 
                             ifelse(dat_all_BZ$Gender == "female", "female",
                                    ifelse(dat_all_BZ$Gender == "Female", "female",
                                           ifelse(dat_all_BZ$Gender == "Exhausted but feisty female", "female",
                                                  ifelse(dat_all_BZ$Gender == "F", "female",
                                                         ifelse(dat_all_BZ$Gender == "Woman/Female", "female",
                                                                ifelse(dat_all_BZ$Gender == "XY", "male",
                                                                       ifelse(dat_all_BZ$Gender == "very manly man", "male",
                                                                              ifelse(dat_all_BZ$Gender == "Man", "male",
                                                                                     ifelse(dat_all_BZ$Gender == "Manly man-man male"  , "male",
                                                                                            ifelse(dat_all_BZ$Gender == "girl", "female",
                                                                                                   ifelse(dat_all_BZ$Gender == "", NA,
                                                                                                          ifelse(dat_all_BZ$Gender == "Girl", "female",
                                                                                                                 ifelse(dat_all_BZ$Gender == "lad", "male",
                                                                                                                        ifelse(dat_all_BZ$Gender == "male", "male",
                                                                                                                               ifelse(dat_all_BZ$Gender == "Male", "male",
                                                                                                                                      ifelse(dat_all_BZ$Gender == "man", "male",
                                                                                                                                             ifelse(dat_all_BZ$Gender == "IDK", NA,
                                                                                                                                                    ifelse(dat_all_BZ$Gender == "?", NA,
                                                                                                                                                           ifelse(dat_all_BZ$Gender == "the better one", NA,
                                                                                                                                                                  ifelse(dat_all_BZ$Gender == "prefer not to say", NA,
                                                                                                                                                                         ifelse(dat_all_BZ$Gender == "No", NA,
                                                                                                                                                                              ifelse(dat_all_BZ$Gender == "Woman", "female", "other" )))))))))))))))))))))))
                                                                                     



table(dat_all_BZ$Gender, useNA = ("ifany"))
table(dat_all_BZ$Gendera, useNA = ("ifany"))

subset(dat_all_BZ, dat_all_BZ$Gendera == "male")$ Gender
subset(dat_all_BZ, dat_all_BZ$Gendera == "other")$ Gender
subset(dat_all_BZ, is.na(dat_all_BZ$Gendera))$ Gender

##### count missing data
dat_all_BZ_BL_temp <- subset(dat_all_BZ, dat_all_BZ$session == "Before_lectures")
dat_all_BZ_AL_temp <- subset(dat_all_BZ, dat_all_BZ$session =="After_lectures")    
dat_all_BZ_AP_temp <- subset(dat_all_BZ, dat_all_BZ$session =="After_prac")  

table(dat_all_BZ_BL_temp$Gendera, useNA = ("ifany"))
table(dat_all_BZ_AL_temp$Gendera, useNA = ("ifany"))
table(dat_all_BZ_AP_temp$Gendera, useNA = ("ifany"))
table(dat_all_BZ$Gendera, useNA = ("ifany"))

write.csv(table(dat_all_BZ$Gender, useNA = ("ifany")), "dat_all_BZ_all_gender.csv")
write.csv(table(dat_all_BZ$Gendera, useNA = ("ifany")), "dat_all_BZ_grouped_gender.csv")

##################################################################################################################
### subset cat
dat_all_BZ_BL <- subset(dat_all_BZ, dat_all_BZ$session == "Before_lectures")
dat_all_BZ_AL <- subset(dat_all_BZ, dat_all_BZ$session =="After_lectures")    
dat_all_BZ_AP <- subset(dat_all_BZ, dat_all_BZ$session =="After_prac")  

length(dat_all_BZ_BL[,1])
length(dat_all_BZ_AL[,1])
length(dat_all_BZ_AP[,1])

### not needed as ordinal data
shapiro_results <- as.data.frame(cbind(
  c(shapiro.test(dat_all_BZ_BL$Q1)$p,  shapiro.test(dat_all_BZ_AL$Q1)$p,  shapiro.test(dat_all_BZ_AP$Q1)$p,
    shapiro.test(dat_all_BZ_BL$Q2)$p,  shapiro.test(dat_all_BZ_AL$Q2)$p,  shapiro.test(dat_all_BZ_AP$Q2)$p,
    shapiro.test(dat_all_BZ_BL$Q3)$p,  shapiro.test(dat_all_BZ_AL$Q3)$p,  shapiro.test(dat_all_BZ_AP$Q3)$p,
    shapiro.test(dat_all_BZ_BL$Q4)$p,  shapiro.test(dat_all_BZ_AL$Q4)$p,  shapiro.test(dat_all_BZ_AP$Q4)$p,
    shapiro.test(dat_all_BZ_BL$Q5)$p,  shapiro.test(dat_all_BZ_AL$Q5)$p,  shapiro.test(dat_all_BZ_AP$Q5)$p,
    shapiro.test(dat_all_BZ_BL$Q6)$p,  shapiro.test(dat_all_BZ_AL$Q6)$p,  shapiro.test(dat_all_BZ_AP$Q6)$p,
    shapiro.test(dat_all_BZ_BL$Q7)$p,  shapiro.test(dat_all_BZ_AL$Q7)$p,  shapiro.test(dat_all_BZ_AP$Q7)$p,
    shapiro.test(dat_all_BZ_BL$Q8)$p,  shapiro.test(dat_all_BZ_AL$Q8)$p,  shapiro.test(dat_all_BZ_AP$Q8)$p,
    shapiro.test(dat_all_BZ_BL$Q9)$p,  shapiro.test(dat_all_BZ_AL$Q9)$p,  shapiro.test(dat_all_BZ_AP$Q9)$p,
    shapiro.test(dat_all_BZ_BL$Q10)$p, shapiro.test(dat_all_BZ_AL$Q10)$p, shapiro.test(dat_all_BZ_AP$Q10)$p,
    shapiro.test(dat_all_BZ_BL$Q11)$p, shapiro.test(dat_all_BZ_AL$Q11)$p, shapiro.test(dat_all_BZ_AP$Q11)$p,
    shapiro.test(dat_all_BZ_AP$Q12)$p,
    shapiro.test(dat_all_BZ_AP$Q13)$p),
  
  c("Q1", "Q1", "Q1",
    "Q2", "Q2", "Q2",
    "Q3", "Q3", "Q3",
    "Q4", "Q4", "Q4",
    "Q5", "Q5", "Q5",
    "Q6", "Q6", "Q6",
    "Q7", "Q7", "Q7",
    "Q8", "Q8", "Q8",
    "Q9", "Q9", "Q9",
    "Q10", "Q10", "Q10",
    "Q11", "Q11", "Q11",
    "Q12", "Q13"),
  c("BL", "AL", "AP",
    "BL", "AL", "AP",
    "BL", "AL", "AP",
    "BL", "AL", "AP",
    "BL", "AL", "AP",
    "BL", "AL", "AP",
    "BL", "AL", "AP",
    "BL", "AL", "AP",
    "BL", "AL", "AP",
    "BL", "AL", "AP",
    "BL", "AL", "AP",
    "AP", "AP") ))




colnames(shapiro_results) <- c("p", "Q", "session")

max(as.numeric(shapiro_results$p)) ### 1.959526e-13 # all very non-normal
min(as.numeric(shapiro_results$p))


### subset gender

dat_all_BZ_males   <- subset(dat_all_BZ, dat_all_BZ$Gendera == "male")
dat_all_BZ_females <- subset(dat_all_BZ, dat_all_BZ$Gendera == "female")
dat_all_BZ_other   <- subset(dat_all_BZ, dat_all_BZ$Gendera == "other")

dat_all_BZ_males_BL <- subset(dat_all_BZ_males, dat_all_BZ_males$session == "Before_lectures")
dat_all_BZ_males_AL <- subset(dat_all_BZ_males, dat_all_BZ_males$session =="After_lectures")    
dat_all_BZ_males_AP <- subset(dat_all_BZ_males, dat_all_BZ_males$session =="After_prac")  

dat_all_BZ_females_BL <- subset(dat_all_BZ_females, dat_all_BZ_females$session == "Before_lectures")
dat_all_BZ_females_AL <- subset(dat_all_BZ_females, dat_all_BZ_females$session =="After_lectures")    
dat_all_BZ_females_AP <- subset(dat_all_BZ_females, dat_all_BZ_females$session =="After_prac")  

dat_all_BZ_other_BL <- subset(dat_all_BZ_other, dat_all_BZ_other$session == "Before_lectures")
dat_all_BZ_other_AL <- subset(dat_all_BZ_other, dat_all_BZ_other$session =="After_lectures")    
dat_all_BZ_other_AP <- subset(dat_all_BZ_other, dat_all_BZ_other$session =="After_prac")  




#######################################################################################################

count_12345_in_vector <- function(vect, subset_name){
  out_df <- c(
    sum(na.omit(vect) == 1),
    sum(na.omit(vect) == 2),
    sum(na.omit(vect) == 3),
    sum(na.omit(vect) == 4),
    sum(na.omit(vect) == 5)
  )
  out_df <- t(as.data.frame(out_df))
  colnames(out_df) <- c("Strongly Disagree","Disagree",
                        "Neutral","Agree","Strongly Agree")
  rownames(out_df) <- c(subset_name)  
  return(out_df)
}


levels(as.factor(dat_all_BZ$session))

Q1_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q1, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q1, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q1, "After practical")))

Q2_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q2, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q2, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q2, "After practical")))

Q3_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q3, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q3, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q3, "After practical")))

Q4_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q4, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q4, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q4, "After practical")))

Q5_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q5, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q5, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q5, "After practical")))

Q6_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q6, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q6, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q6, "After practical")))

Q7_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q7, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q7, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q7, "After practical")))

Q8_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q8, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q8, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q8, "After practical")))

Q9_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q9, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q9, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q9, "After practical")))

Q10_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q10, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q10, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q10, "After practical")))

Q11_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_BL$Q11, "Before lectures"),
  count_12345_in_vector(dat_all_BZ_AL$Q11, "After lectures"),
  count_12345_in_vector(dat_all_BZ_AP$Q11, "After practical")))

Q12_BLALAP <- as.data.frame(rbind(count_12345_in_vector(dat_all_BZ_AP$Q12, ""))) ## only asked after the prac
Q13_BLALAP <- as.data.frame(rbind(count_12345_in_vector(dat_all_BZ_AP$Q13, ""))) ## only asked after the prac



All_Q_BLALAP <- as.data.frame(rbind(
  Q1_BLALAP,
  Q2_BLALAP,
  Q3_BLALAP,
  Q4_BLALAP,
  Q5_BLALAP,
  Q6_BLALAP,
  Q7_BLALAP,
  Q8_BLALAP,
  Q9_BLALAP,
  Q10_BLALAP,
  Q11_BLALAP,
  Q12_BLALAP,
  Q13_BLALAP
))

All_Q_BLALAP$Q <- c("Q1", "Q1", "Q1",
                    "Q2", "Q2", "Q2",
                    "Q3", "Q3", "Q3",
                    "Q4", "Q4", "Q4",
                    "Q5", "Q5", "Q5",
                    "Q6", "Q6", "Q6",
                    "Q7", "Q7", "Q7",
                    "Q8", "Q8", "Q8",
                    "Q9", "Q9", "Q9",
                    "Q10", "Q10", "Q10",
                    "Q11", "Q11", "Q11",
                    "Q12", "Q13")

All_Q_BLALAP$session <- c("BL", "AL", "AP",
                          "BL", "AL", "AP",
                          "BL", "AL", "AP",
                          "BL", "AL", "AP",
                          "BL", "AL", "AP",
                          "BL", "AL", "AP",
                          "BL", "AL", "AP",
                          "BL", "AL", "AP",
                          "BL", "AL", "AP",
                          "BL", "AL", "AP",
                          "BL", "AL", "AP",
                          "AP", "AP") 

All_Q_BLALAP$total <-     All_Q_BLALAP$`Strongly Disagree` +
                          All_Q_BLALAP$`Disagree`+
                          All_Q_BLALAP$`Neutral` +
                          All_Q_BLALAP$`Agree` +
                          All_Q_BLALAP$`Strongly Agree` 

All_Q_BLALAP$`Strongly Disagree %` <- (All_Q_BLALAP$`Strongly Disagree` / All_Q_BLALAP$total) * 100
All_Q_BLALAP$`Disagree %`          <- (All_Q_BLALAP$`Disagree` / All_Q_BLALAP$total) * 100
All_Q_BLALAP$`Neutral %`           <- (All_Q_BLALAP$`Neutral` / All_Q_BLALAP$total) * 100
All_Q_BLALAP$`Agree %`             <- (All_Q_BLALAP$`Agree` / All_Q_BLALAP$total) * 100
All_Q_BLALAP$`Strongly Agree %`    <- (All_Q_BLALAP$`Strongly Agree` / All_Q_BLALAP$total) * 100

write.csv(All_Q_BLALAP, "All_Q_BLALAP.csv", row.names = FALSE)


out_width  = 7
out_height = 3

pdf("Q1_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q1_BLALAP,  as.percent=TRUE, main = "An understanding of insects is important for life scientists.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q2_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q2_BLALAP,  as.percent=TRUE, main = "Understanding insects has important practical applications.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q3_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q3_BLALAP,  as.percent=TRUE, main = "It is important that I improve my understanding of insects during my degree", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q4_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q4_BLALAP,  as.percent=TRUE, main = "Understanding insects will help me get a job after I graduate.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q5_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q5_BLALAP,  as.percent=TRUE, main = "I would like to study insects for my final year project.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q6_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q6_BLALAP,  as.percent=TRUE, main = "Insects are disgusting.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q7_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q7_BLALAP,  as.percent=TRUE, main = "I am afraid of insects.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q8_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q8_BLALAP,  as.percent=TRUE, main = "Insects are boring.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q9_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q9_BLALAP,  as.percent=TRUE, main = "Insect conservation is important.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q10_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q10_BLALAP, as.percent=TRUE, main = "I am comfortable identifying insects to an order level.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q11_BLALAP_LP_20232024.pdf", width = out_width, height = out_height)
plot.likert(Q11_BLALAP, as.percent=TRUE, main = "I can describe the anatomy of insects well.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q12_BLALAP_LP_20232024.pdf", width = out_width, height = 2.5)
plot.likert(Q12_BLALAP, as.percent=TRUE, main = "The practical session helped me to better understand insects.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q13_BLALAP_LP_20232024.pdf", width = out_width, height = 2.5)
plot.likert(Q13_BLALAP, as.percent=TRUE, main = "The practical session made me less afraid of insects.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?



#####################################################################################################################
##### USE ordinal logistic regression with robust standard errors to deal with the the fact I have mostly repeated-measures data but no way of tracking it as all questionnaires were anonomysed.
##### If I use basic wilcoxon I will inflate the false positives if students give more similar answers than expected for independent students
##### Will do all Qs sep, then emmeans, then FDR correct across all Qs.

OLR_tests <- function(BL_v, AL_v, AP_v, Q){
  Q_df <- as.data.frame(cbind(
    c(BL_v, AL_v, AP_v),
    c(rep("BL", length(BL_v)), rep("AL", length(AL_v)), rep("AP", length(AP_v)))))
    
  colnames(Q_df) <- c("score", "session")
  Q_df$score    <- ordered(Q_df$score, levels = c(1, 2, 3, 4, 5))
  Q_df$session  <- as.factor(Q_df$session)
  
  model_Q <- clm(score ~ session, data = Q_df)

  #### Robust standard errors (Huber-White Sandwich Estimator)
  Q_robust_vcov    <- sandwich(model_Q)
  Q_robust_results <- coeftest(model_Q, vcov. = Q_robust_vcov)
  ## pairwise using the robust matrix
  Q_robust_matrix   <- as.matrix(Q_robust_vcov)
  Q_session_emmeans <- emmeans(model_Q, ~ session, vcov. = Q_robust_matrix)

  Q_pairs_result <- as.data.frame(pairs(Q_session_emmeans, adjust = "none")) ### no adjust here as adjust with all Qs
  Q_pairs_result$Q <- c(Q,Q,Q)
  
  return(Q_pairs_result)
}

dat_all_BZ_all_OLRout <- rbind(
  OLR_tests(dat_all_BZ_BL$Q1,  dat_all_BZ_AL$Q1,  dat_all_BZ_AP$Q1,  "Q1"),
  OLR_tests(dat_all_BZ_BL$Q2,  dat_all_BZ_AL$Q2,  dat_all_BZ_AP$Q2,  "Q2"),
  OLR_tests(dat_all_BZ_BL$Q3,  dat_all_BZ_AL$Q3,  dat_all_BZ_AP$Q3,  "Q3"),
  OLR_tests(dat_all_BZ_BL$Q4,  dat_all_BZ_AL$Q4,  dat_all_BZ_AP$Q4,  "Q4"),
  OLR_tests(dat_all_BZ_BL$Q5,  dat_all_BZ_AL$Q5,  dat_all_BZ_AP$Q5,  "Q5"),
  OLR_tests(dat_all_BZ_BL$Q6,  dat_all_BZ_AL$Q6,  dat_all_BZ_AP$Q6,  "Q6"),
  OLR_tests(dat_all_BZ_BL$Q7,  dat_all_BZ_AL$Q7,  dat_all_BZ_AP$Q7,  "Q7"),
  OLR_tests(dat_all_BZ_BL$Q8,  dat_all_BZ_AL$Q8,  dat_all_BZ_AP$Q8,  "Q8"),
  OLR_tests(dat_all_BZ_BL$Q9,  dat_all_BZ_AL$Q9,  dat_all_BZ_AP$Q9,  "Q9"),
  OLR_tests(dat_all_BZ_BL$Q10, dat_all_BZ_AL$Q10, dat_all_BZ_AP$Q10, "Q10"),
  OLR_tests(dat_all_BZ_BL$Q11, dat_all_BZ_AL$Q11, dat_all_BZ_AP$Q11, "Q11"))

dat_all_BZ_all_OLRout$FDRall <- p.adjust(dat_all_BZ_all_OLRout$p, method = "fdr")
write.csv(dat_all_BZ_all_OLRout, "dat_all_BZ_all_OLRout.csv")

##################################################################################################
################# effect sizes with bootstapped CIs

get_effectsize <- function(BL_v, AL_v, AP_v, Q, X){
  WT_BL_v_AL_df <- data.frame(
    score = c(BL_v, AL_v),
    group = ordered(factor(c(rep("BL", length(BL_v)), rep("AL", length(AL_v)))), levels = c("AL", "BL"))
  )
  
  WT_BL_v_AL_result <- as.data.frame(cohens_d(
    data = WT_BL_v_AL_df, 
    formula = score ~ group, 
    paired = FALSE,
    ci = TRUE, 
    nboot = X, 
    ci.type = "perc"
  ))

  
  WT_BL_v_AP_df <- data.frame(
    score = c(BL_v, AP_v),
    group = ordered(factor(c(rep("BL", length(BL_v)), rep("AP", length(AP_v)))), levels = c("AP", "BL"))
  )
  
  WT_BL_v_AP_result <- as.data.frame(cohens_d(
    data = WT_BL_v_AP_df, 
    formula = score ~ group, 
    paired = FALSE,
    ci = TRUE, 
    nboot = X, 
    ci.type = "perc"
  ))
  
  
  WT_AL_v_AP_df <- data.frame(
    score = c(AL_v, AP_v),
    group = ordered(factor(c(rep("AL", length(AL_v)), rep("AP", length(AP_v)))), levels = c("AP", "AL"))
  )
  
  WT_AL_v_AP_result <- as.data.frame(cohens_d(
    data = WT_AL_v_AP_df, 
    formula = score ~ group, 
    paired = FALSE,
    ci = TRUE, 
    nboot = X, 
    ci.type = "perc"
  ))
  
  out_table <- as.data.frame(rbind(
    WT_BL_v_AL_result,
    WT_BL_v_AP_result,
    WT_AL_v_AP_result))
  
  out_table$Q <- c(rep(Q, 3))
  return(out_table)
}


use_seed = 42
set.seed(use_seed )

dat_all_BZ_all_effectsizes <- rbind(
  get_effectsize(dat_all_BZ_BL$Q1,  dat_all_BZ_AL$Q1,  dat_all_BZ_AP$Q1,  "Q1", Nboot),
  get_effectsize(dat_all_BZ_BL$Q2,  dat_all_BZ_AL$Q2,  dat_all_BZ_AP$Q2,  "Q2", Nboot),
  get_effectsize(dat_all_BZ_BL$Q3,  dat_all_BZ_AL$Q3,  dat_all_BZ_AP$Q3,  "Q3", Nboot),
  get_effectsize(dat_all_BZ_BL$Q4,  dat_all_BZ_AL$Q4,  dat_all_BZ_AP$Q4,  "Q4", Nboot),
  get_effectsize(dat_all_BZ_BL$Q5,  dat_all_BZ_AL$Q5,  dat_all_BZ_AP$Q5,  "Q5", Nboot),
  get_effectsize(dat_all_BZ_BL$Q6,  dat_all_BZ_AL$Q6,  dat_all_BZ_AP$Q6,  "Q6", Nboot),
  get_effectsize(dat_all_BZ_BL$Q7,  dat_all_BZ_AL$Q7,  dat_all_BZ_AP$Q7,  "Q7", Nboot),
  get_effectsize(dat_all_BZ_BL$Q8,  dat_all_BZ_AL$Q8,  dat_all_BZ_AP$Q8,  "Q8", Nboot),
  get_effectsize(dat_all_BZ_BL$Q9,  dat_all_BZ_AL$Q9,  dat_all_BZ_AP$Q9,  "Q9", Nboot),
  get_effectsize(dat_all_BZ_BL$Q10, dat_all_BZ_AL$Q10, dat_all_BZ_AP$Q10, "Q10", Nboot),
  get_effectsize(dat_all_BZ_BL$Q11, dat_all_BZ_AL$Q11, dat_all_BZ_AP$Q11, "Q11", Nboot))

write.csv(dat_all_BZ_all_effectsizes, paste("dat_all_BZ_all_effectsizes", Nboot, "seed", use_seed, ".csv", sep = ""))


#######################################################################################################################
### gender



OLR_gender_tests <- function(male_v, female_v, other_v, Q){
  Q_df <- as.data.frame(cbind(
    c(male_v, female_v, other_v),
    c(rep("male", length(male_v)), rep("female", length(female_v)), rep("other", length(other_v)))))
  
  colnames(Q_df) <- c("score", "gender")
  Q_df$score    <- ordered(Q_df$score, levels = c(1, 2, 3, 4, 5))
  Q_df$gender   <- as.factor(Q_df$gender)
  
  model_Q <- clm(score ~ gender, data = Q_df)
  
  #### Robust standard errors (Huber-White Sandwich Estimator)
  Q_robust_vcov    <- sandwich(model_Q)
  Q_robust_results <- coeftest(model_Q, vcov. = Q_robust_vcov)
  ## pairwise using the robust matrix
  Q_robust_matrix   <- as.matrix(Q_robust_vcov)
  Q_gender_emmeans <- emmeans(model_Q, ~ gender, vcov. = Q_robust_matrix)
  
  Q_pairs_result <- as.data.frame(pairs(Q_gender_emmeans, adjust = "none")) ### no adjust here as adjust with all Qs
  Q_pairs_result$Q <- c(Q,Q,Q)
  
  return(Q_pairs_result)
}


gender_all_OLRout <- rbind(
  OLR_gender_tests(dat_all_BZ_males_BL$Q1,  dat_all_BZ_females_BL$Q1, dat_all_BZ_other_BL$Q1, "Q1_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q1,  dat_all_BZ_females_AL$Q1, dat_all_BZ_other_AL$Q1, "Q1_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q1,  dat_all_BZ_females_AP$Q1, dat_all_BZ_other_AP$Q1, "Q1_AP"),
  OLR_gender_tests(dat_all_BZ_males_BL$Q2,  dat_all_BZ_females_BL$Q2, dat_all_BZ_other_BL$Q2, "Q2_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q2,  dat_all_BZ_females_AL$Q2, dat_all_BZ_other_AL$Q2, "Q2_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q2,  dat_all_BZ_females_AP$Q2, dat_all_BZ_other_AP$Q2, "Q2_AP"),
  OLR_gender_tests(dat_all_BZ_males_BL$Q3,  dat_all_BZ_females_BL$Q3, dat_all_BZ_other_BL$Q3, "Q3_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q3,  dat_all_BZ_females_AL$Q3, dat_all_BZ_other_AL$Q3, "Q3_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q3,  dat_all_BZ_females_AP$Q3, dat_all_BZ_other_AP$Q3, "Q3_AP"),
  OLR_gender_tests(dat_all_BZ_males_BL$Q4,  dat_all_BZ_females_BL$Q4, dat_all_BZ_other_BL$Q4, "Q4_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q4,  dat_all_BZ_females_AL$Q4, dat_all_BZ_other_AL$Q4, "Q4_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q4,  dat_all_BZ_females_AP$Q4, dat_all_BZ_other_AP$Q4, "Q4_AP"),
  OLR_gender_tests(dat_all_BZ_males_BL$Q5,  dat_all_BZ_females_BL$Q5, dat_all_BZ_other_BL$Q5, "Q5_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q5,  dat_all_BZ_females_AL$Q5, dat_all_BZ_other_AL$Q5, "Q5_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q5,  dat_all_BZ_females_AP$Q5, dat_all_BZ_other_AP$Q5, "Q5_AP"),
  OLR_gender_tests(dat_all_BZ_males_BL$Q6,  dat_all_BZ_females_BL$Q6, dat_all_BZ_other_BL$Q6, "Q6_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q6,  dat_all_BZ_females_AL$Q6, dat_all_BZ_other_AL$Q6, "Q6_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q6,  dat_all_BZ_females_AP$Q6, dat_all_BZ_other_AP$Q6, "Q6_AP"),
  OLR_gender_tests(dat_all_BZ_males_BL$Q7,  dat_all_BZ_females_BL$Q7, dat_all_BZ_other_BL$Q7, "Q7_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q7,  dat_all_BZ_females_AL$Q7, dat_all_BZ_other_AL$Q7, "Q7_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q7,  dat_all_BZ_females_AP$Q7, dat_all_BZ_other_AP$Q7, "Q7_AP"),
  OLR_gender_tests(dat_all_BZ_males_BL$Q8,  dat_all_BZ_females_BL$Q8, dat_all_BZ_other_BL$Q8, "Q8_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q8,  dat_all_BZ_females_AL$Q8, dat_all_BZ_other_AL$Q8, "Q8_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q8,  dat_all_BZ_females_AP$Q8, dat_all_BZ_other_AP$Q8, "Q8_AP"),
  OLR_gender_tests(dat_all_BZ_males_BL$Q9,  dat_all_BZ_females_BL$Q9, dat_all_BZ_other_BL$Q9, "Q9_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q9,  dat_all_BZ_females_AL$Q9, dat_all_BZ_other_AL$Q9, "Q9_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q9,  dat_all_BZ_females_AP$Q9, dat_all_BZ_other_AP$Q9, "Q9_AP"),
  OLR_gender_tests(dat_all_BZ_males_BL$Q10,  dat_all_BZ_females_BL$Q10, dat_all_BZ_other_BL$Q10, "Q10_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q10,  dat_all_BZ_females_AL$Q10, dat_all_BZ_other_AL$Q10, "Q10_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q10,  dat_all_BZ_females_AP$Q10, dat_all_BZ_other_AP$Q10, "Q10_AP"),
  OLR_gender_tests(dat_all_BZ_males_BL$Q11,  dat_all_BZ_females_BL$Q11, dat_all_BZ_other_BL$Q11, "Q11_BL"),
  OLR_gender_tests(dat_all_BZ_males_AL$Q11,  dat_all_BZ_females_AL$Q11, dat_all_BZ_other_AL$Q11, "Q11_AL"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q11,  dat_all_BZ_females_AP$Q11, dat_all_BZ_other_AP$Q11, "Q11_AP"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q12,  dat_all_BZ_females_AP$Q12, dat_all_BZ_other_AP$Q12, "Q12_AP"),
  OLR_gender_tests(dat_all_BZ_males_AP$Q13,  dat_all_BZ_females_AP$Q13, dat_all_BZ_other_AP$Q13, "Q13_AP"))

gender_all_OLRout <- subset(gender_all_OLRout, gender_all_OLRout$contrast == "female - male") ### best to just use males and females as too few 'other'
gender_all_OLRout$FDRall <- p.adjust(gender_all_OLRout$p, method = "fdr")
write.csv(gender_all_OLRout, "all_20232024_gender_all_OLRout.csv")




##################################################################################################
################# effect sizes with bootstapped CIs

get_effectsize_gender <- function(male_v, female_v, other_v, Q, X){
  
  WT_male_v_female_df <- data.frame(
    score = c(male_v, female_v),
    group = ordered(factor(c(rep("male", length(male_v)), rep("female", length(female_v)))), levels = c("female", "male"))
  )
  
  WT_male_v_female_result <- as.data.frame(cohens_d(
    data = WT_male_v_female_df, 
    formula = score ~ group, 
    paired = FALSE,
    ci = TRUE, 
    nboot = X, 
    ci.type = "perc"
  ))
  
  
  WT_male_v_other_df <- data.frame(
    score = c(male_v, other_v),
    group = ordered(factor(c(rep("male", length(male_v)), rep("other", length(other_v)))), levels = c("other", "male"))
  )
  
  WT_male_v_other_result <- as.data.frame(cohens_d(
    data = WT_male_v_other_df, 
    formula = score ~ group, 
    paired = FALSE,
    ci = TRUE, 
    nboot = X, 
    ci.type = "perc"
  ))
  
  
  WT_female_v_other_df <- data.frame(
    score = c(female_v, other_v),
    group = ordered(factor(c(rep("female", length(female_v)), rep("other", length(other_v)))), levels = c("other", "female"))
  )
  
  WT_female_v_other_result <- as.data.frame(cohens_d(
    data = WT_female_v_other_df, 
    formula = score ~ group, 
    paired = FALSE,
    ci = TRUE, 
    nboot = X, 
    ci.type = "perc"
  ))
  
  out_table <- as.data.frame(rbind(
    WT_male_v_female_result,
    WT_male_v_other_result,
    WT_female_v_other_result))
  
  out_table$Q <- c(rep(Q, 3))
  return(out_table)
}

use_seed = 42
set.seed(use_seed )

gender_all_effectsizes  <- rbind(
  get_effectsize_gender(dat_all_BZ_males_BL$Q1,  dat_all_BZ_females_BL$Q1, dat_all_BZ_other_BL$Q1, "Q1_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q1,  dat_all_BZ_females_AL$Q1, dat_all_BZ_other_AL$Q1, "Q1_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q1,  dat_all_BZ_females_AP$Q1, dat_all_BZ_other_AP$Q1, "Q1_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_BL$Q2,  dat_all_BZ_females_BL$Q2, dat_all_BZ_other_BL$Q2, "Q2_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q2,  dat_all_BZ_females_AL$Q2, dat_all_BZ_other_AL$Q2, "Q2_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q2,  dat_all_BZ_females_AP$Q2, dat_all_BZ_other_AP$Q2, "Q2_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_BL$Q3,  dat_all_BZ_females_BL$Q3, dat_all_BZ_other_BL$Q3, "Q3_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q3,  dat_all_BZ_females_AL$Q3, dat_all_BZ_other_AL$Q3, "Q3_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q3,  dat_all_BZ_females_AP$Q3, dat_all_BZ_other_AP$Q3, "Q3_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_BL$Q4,  dat_all_BZ_females_BL$Q4, dat_all_BZ_other_BL$Q4, "Q4_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q4,  dat_all_BZ_females_AL$Q4, dat_all_BZ_other_AL$Q4, "Q4_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q4,  dat_all_BZ_females_AP$Q4, dat_all_BZ_other_AP$Q4, "Q4_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_BL$Q5,  dat_all_BZ_females_BL$Q5, dat_all_BZ_other_BL$Q5, "Q5_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q5,  dat_all_BZ_females_AL$Q5, dat_all_BZ_other_AL$Q5, "Q5_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q5,  dat_all_BZ_females_AP$Q5, dat_all_BZ_other_AP$Q5, "Q5_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_BL$Q6,  dat_all_BZ_females_BL$Q6, dat_all_BZ_other_BL$Q6, "Q6_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q6,  dat_all_BZ_females_AL$Q6, dat_all_BZ_other_AL$Q6, "Q6_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q6,  dat_all_BZ_females_AP$Q6, dat_all_BZ_other_AP$Q6, "Q6_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_BL$Q7,  dat_all_BZ_females_BL$Q7, dat_all_BZ_other_BL$Q7, "Q7_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q7,  dat_all_BZ_females_AL$Q7, dat_all_BZ_other_AL$Q7, "Q7_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q7,  dat_all_BZ_females_AP$Q7, dat_all_BZ_other_AP$Q7, "Q7_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_BL$Q8,  dat_all_BZ_females_BL$Q8, dat_all_BZ_other_BL$Q8, "Q8_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q8,  dat_all_BZ_females_AL$Q8, dat_all_BZ_other_AL$Q8, "Q8_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q8,  dat_all_BZ_females_AP$Q8, dat_all_BZ_other_AP$Q8, "Q8_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_BL$Q9,  dat_all_BZ_females_BL$Q9, dat_all_BZ_other_BL$Q9, "Q9_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q9,  dat_all_BZ_females_AL$Q9, dat_all_BZ_other_AL$Q9, "Q9_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q9,  dat_all_BZ_females_AP$Q9, dat_all_BZ_other_AP$Q9, "Q9_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_BL$Q10,  dat_all_BZ_females_BL$Q10, dat_all_BZ_other_BL$Q10, "Q10_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q10,  dat_all_BZ_females_AL$Q10, dat_all_BZ_other_AL$Q10, "Q10_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q10,  dat_all_BZ_females_AP$Q10, dat_all_BZ_other_AP$Q10, "Q10_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_BL$Q11,  dat_all_BZ_females_BL$Q11, dat_all_BZ_other_BL$Q11, "Q11_BL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AL$Q11,  dat_all_BZ_females_AL$Q11, dat_all_BZ_other_AL$Q11, "Q11_AL", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q11,  dat_all_BZ_females_AP$Q11, dat_all_BZ_other_AP$Q11, "Q11_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q12,  dat_all_BZ_females_AP$Q12, dat_all_BZ_other_AP$Q12, "Q12_AP", Nboot),
  get_effectsize_gender(dat_all_BZ_males_AP$Q13,  dat_all_BZ_females_AP$Q13, dat_all_BZ_other_AP$Q13, "Q13_AP", Nboot))

gender_all_effectsizes$group <- paste(gender_all_effectsizes$group1, gender_all_effectsizes$group2)
gender_all_effectsizes <- subset(gender_all_effectsizes, gender_all_effectsizes$group == "female male") ### best to just use males and females as too few 'other'

write.csv(gender_all_effectsizes, paste("gender_all_effectsizes", Nboot, "seed", use_seed, ".csv", sep = ""))

############################################################################################################
###### plot Qs with sig effects
### plotting with other, but not tested

Q6_gender_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_males_BL$Q6, "males_BL_Q6"),
  count_12345_in_vector(dat_all_BZ_females_BL$Q6, "females_BL_Q6"),
  count_12345_in_vector(dat_all_BZ_other_BL$Q6, "other_BL_Q6"),
  count_12345_in_vector(dat_all_BZ_males_AL$Q6, "males_AL_Q6"),
  count_12345_in_vector(dat_all_BZ_females_AL$Q6, "females_AL_Q6"), 
  count_12345_in_vector(dat_all_BZ_other_AL$Q6, "other_AL_Q6"), 
  count_12345_in_vector(dat_all_BZ_males_AP$Q6, "males_AP_Q6"),
  count_12345_in_vector(dat_all_BZ_females_AP$Q6, "females_AP_Q6"),
  count_12345_in_vector(dat_all_BZ_other_AP$Q6, "other_AP_Q6")))

Q7_gender_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_males_BL$Q7, "males_BL_Q7"),
  count_12345_in_vector(dat_all_BZ_females_BL$Q7, "females_BL_Q7"),
  count_12345_in_vector(dat_all_BZ_other_BL$Q7, "other_BL_Q7"),
  count_12345_in_vector(dat_all_BZ_males_AL$Q7, "males_AL_Q7"),
  count_12345_in_vector(dat_all_BZ_females_AL$Q7, "females_AL_Q7"), 
  count_12345_in_vector(dat_all_BZ_other_AL$Q7, "other_AL_Q7"), 
  count_12345_in_vector(dat_all_BZ_males_AP$Q7, "males_AP_Q7"),
  count_12345_in_vector(dat_all_BZ_females_AP$Q7, "females_AP_Q7"),
  count_12345_in_vector(dat_all_BZ_other_AP$Q7, "other_AP_Q7")))

Q8_gender_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_males_BL$Q8, "males_BL_Q8"),
  count_12345_in_vector(dat_all_BZ_females_BL$Q8, "females_BL_Q8"),
  count_12345_in_vector(dat_all_BZ_other_BL$Q8, "other_BL_Q8"),
  count_12345_in_vector(dat_all_BZ_males_AL$Q8, "males_AL_Q8"),
  count_12345_in_vector(dat_all_BZ_females_AL$Q8, "females_AL_Q8"), 
  count_12345_in_vector(dat_all_BZ_other_AL$Q8, "other_AL_Q8"), 
  count_12345_in_vector(dat_all_BZ_males_AP$Q8, "males_AP_Q8"),
  count_12345_in_vector(dat_all_BZ_females_AP$Q8, "females_AP_Q8"),
  count_12345_in_vector(dat_all_BZ_other_AP$Q8, "other_AP_Q8")))

Q11_gender_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_males_BL$Q11, "males_BL_Q11"),
  count_12345_in_vector(dat_all_BZ_females_BL$Q11, "females_BL_Q11"),
  count_12345_in_vector(dat_all_BZ_other_BL$Q11, "other_BL_Q11"),
  count_12345_in_vector(dat_all_BZ_males_AL$Q11, "males_AL_Q11"),
  count_12345_in_vector(dat_all_BZ_females_AL$Q11, "females_AL_Q11"), 
  count_12345_in_vector(dat_all_BZ_other_AL$Q11, "other_AL_Q11"), 
  count_12345_in_vector(dat_all_BZ_males_AP$Q11, "males_AP_Q11"),
  count_12345_in_vector(dat_all_BZ_females_AP$Q11, "females_AP_Q11"),
  count_12345_in_vector(dat_all_BZ_other_AP$Q11, "other_AP_Q11")))

out_height_g = 6


pdf("Q6_gender_BLALAP_LP.pdf", width = out_width, height = out_height_g)
plot.likert(Q6_gender_BLALAP, as.percent=TRUE, main = "Insects are disgusting.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q7_gender_BLALAP_LP.pdf", width = out_width, height = out_height_g)
plot.likert(Q7_gender_BLALAP, as.percent=TRUE, main = "I am afraid of insects.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q8_gender_BLALAP_LP.pdf", width = out_width, height = out_height_g)
plot.likert(Q8_gender_BLALAP, as.percent=TRUE, main = "Insects are boring.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

pdf("Q11_gender_BLALAP_LP.pdf", width = out_width, height = out_height_g)
plot.likert(Q11_gender_BLALAP, as.percent=TRUE, main = "I can describe the anatomy of insects well.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

###########################################################################################
### interaction test with ordinal regression for all Qs


##### Q1
Q1_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q1,
    dat_all_BZ_females_BL$Q1,
    dat_all_BZ_males_AL$Q1, 
    dat_all_BZ_females_AL$Q1,
    dat_all_BZ_males_AP$Q1,
    dat_all_BZ_females_AP$Q1),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q1)),
    rep("female", length(dat_all_BZ_females_BL$Q1)),
    rep("male", length(dat_all_BZ_males_AL$Q1)), 
    rep("female", length(dat_all_BZ_females_AL$Q1)),
    rep("male", length(dat_all_BZ_males_AP$Q1)),
    rep("female", length(dat_all_BZ_females_AP$Q1))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q1)),
    rep("BL", length(dat_all_BZ_females_BL$Q1)),
    rep("AL", length(dat_all_BZ_males_AL$Q1)), 
    rep("AL", length(dat_all_BZ_females_AL$Q1)),
    rep("AP", length(dat_all_BZ_males_AP$Q1)),
    rep("AP", length(dat_all_BZ_females_AP$Q1)))))



colnames(Q1_gender_long) <- c("score", "gender", "session")
Q1_gender_long$score <- ordered(Q1_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q1_gender_long$session  <- ordered(Q1_gender_long$session,  levels = c("BL", "AL", "AP"))
Q1_gender_long$gender <- as.factor(Q1_gender_long$gender )

str(Q1_gender_long)

model_Q1_gender <- clm(score ~ gender + session, data = Q1_gender_long)
model_Q1_gender_interaction <- clm(score ~ gender * session, data = Q1_gender_long)
Q1_gender_robust_vcov    <- sandwich(model_Q1_gender_interaction)
Q1_gender_robust_waldtest <- waldtest(model_Q1_gender, model_Q1_gender_interaction, vcov = Q1_gender_robust_vcov )



##### Q2
Q2_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q2,
    dat_all_BZ_females_BL$Q2,
    dat_all_BZ_males_AL$Q2, 
    dat_all_BZ_females_AL$Q2,
    dat_all_BZ_males_AP$Q2,
    dat_all_BZ_females_AP$Q2),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q2)),
    rep("female", length(dat_all_BZ_females_BL$Q2)),
    rep("male", length(dat_all_BZ_males_AL$Q2)), 
    rep("female", length(dat_all_BZ_females_AL$Q2)),
    rep("male", length(dat_all_BZ_males_AP$Q2)),
    rep("female", length(dat_all_BZ_females_AP$Q2))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q2)),
    rep("BL", length(dat_all_BZ_females_BL$Q2)),
    rep("AL", length(dat_all_BZ_males_AL$Q2)), 
    rep("AL", length(dat_all_BZ_females_AL$Q2)),
    rep("AP", length(dat_all_BZ_males_AP$Q2)),
    rep("AP", length(dat_all_BZ_females_AP$Q2)))))



colnames(Q2_gender_long) <- c("score", "gender", "session")
Q2_gender_long$score <- ordered(Q2_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q2_gender_long$session  <- ordered(Q2_gender_long$session,  levels = c("BL", "AL", "AP"))
Q2_gender_long$gender <- as.factor(Q2_gender_long$gender )

str(Q2_gender_long)

model_Q2_gender <- clm(score ~ gender + session, data = Q2_gender_long)
model_Q2_gender_interaction <- clm(score ~ gender * session, data = Q2_gender_long)
Q2_gender_robust_vcov    <- sandwich(model_Q2_gender_interaction)
Q2_gender_robust_waldtest <- waldtest(model_Q2_gender, model_Q2_gender_interaction, vcov = Q2_gender_robust_vcov )


##### Q3
Q3_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q3,
    dat_all_BZ_females_BL$Q3,
    dat_all_BZ_males_AL$Q3, 
    dat_all_BZ_females_AL$Q3,
    dat_all_BZ_males_AP$Q3,
    dat_all_BZ_females_AP$Q3),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q3)),
    rep("female", length(dat_all_BZ_females_BL$Q3)),
    rep("male", length(dat_all_BZ_males_AL$Q3)), 
    rep("female", length(dat_all_BZ_females_AL$Q3)),
    rep("male", length(dat_all_BZ_males_AP$Q3)),
    rep("female", length(dat_all_BZ_females_AP$Q3))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q3)),
    rep("BL", length(dat_all_BZ_females_BL$Q3)),
    rep("AL", length(dat_all_BZ_males_AL$Q3)), 
    rep("AL", length(dat_all_BZ_females_AL$Q3)),
    rep("AP", length(dat_all_BZ_males_AP$Q3)),
    rep("AP", length(dat_all_BZ_females_AP$Q3)))))



colnames(Q3_gender_long) <- c("score", "gender", "session")
Q3_gender_long$score <- ordered(Q3_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q3_gender_long$session  <- ordered(Q3_gender_long$session,  levels = c("BL", "AL", "AP"))
Q3_gender_long$gender <- as.factor(Q3_gender_long$gender )

str(Q3_gender_long)

model_Q3_gender <- clm(score ~ gender + session, data = Q3_gender_long)
model_Q3_gender_interaction <- clm(score ~ gender * session, data = Q3_gender_long)
Q3_gender_robust_vcov    <- sandwich(model_Q3_gender_interaction)
Q3_gender_robust_waldtest <- waldtest(model_Q3_gender, model_Q3_gender_interaction, vcov = Q3_gender_robust_vcov )


##### Q4
Q4_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q4,
    dat_all_BZ_females_BL$Q4,
    dat_all_BZ_males_AL$Q4, 
    dat_all_BZ_females_AL$Q4,
    dat_all_BZ_males_AP$Q4,
    dat_all_BZ_females_AP$Q4),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q4)),
    rep("female", length(dat_all_BZ_females_BL$Q4)),
    rep("male", length(dat_all_BZ_males_AL$Q4)), 
    rep("female", length(dat_all_BZ_females_AL$Q4)),
    rep("male", length(dat_all_BZ_males_AP$Q4)),
    rep("female", length(dat_all_BZ_females_AP$Q4))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q4)),
    rep("BL", length(dat_all_BZ_females_BL$Q4)),
    rep("AL", length(dat_all_BZ_males_AL$Q4)), 
    rep("AL", length(dat_all_BZ_females_AL$Q4)),
    rep("AP", length(dat_all_BZ_males_AP$Q4)),
    rep("AP", length(dat_all_BZ_females_AP$Q4)))))



colnames(Q4_gender_long) <- c("score", "gender", "session")
Q4_gender_long$score <- ordered(Q4_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q4_gender_long$session  <- ordered(Q4_gender_long$session,  levels = c("BL", "AL", "AP"))
Q4_gender_long$gender <- as.factor(Q4_gender_long$gender )

str(Q4_gender_long)

model_Q4_gender <- clm(score ~ gender + session, data = Q4_gender_long)
model_Q4_gender_interaction <- clm(score ~ gender * session, data = Q4_gender_long)
Q4_gender_robust_vcov    <- sandwich(model_Q4_gender_interaction)
Q4_gender_robust_waldtest <- waldtest(model_Q4_gender, model_Q4_gender_interaction, vcov = Q4_gender_robust_vcov )


##### Q6
Q6_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q6,
    dat_all_BZ_females_BL$Q6,
    dat_all_BZ_males_AL$Q6, 
    dat_all_BZ_females_AL$Q6,
    dat_all_BZ_males_AP$Q6,
    dat_all_BZ_females_AP$Q6),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q6)),
    rep("female", length(dat_all_BZ_females_BL$Q6)),
    rep("male", length(dat_all_BZ_males_AL$Q6)), 
    rep("female", length(dat_all_BZ_females_AL$Q6)),
    rep("male", length(dat_all_BZ_males_AP$Q6)),
    rep("female", length(dat_all_BZ_females_AP$Q6))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q6)),
    rep("BL", length(dat_all_BZ_females_BL$Q6)),
    rep("AL", length(dat_all_BZ_males_AL$Q6)), 
    rep("AL", length(dat_all_BZ_females_AL$Q6)),
    rep("AP", length(dat_all_BZ_males_AP$Q6)),
    rep("AP", length(dat_all_BZ_females_AP$Q6)))))



##### Q5
Q5_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q5,
    dat_all_BZ_females_BL$Q5,
    dat_all_BZ_males_AL$Q5, 
    dat_all_BZ_females_AL$Q5,
    dat_all_BZ_males_AP$Q5,
    dat_all_BZ_females_AP$Q5),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q5)),
    rep("female", length(dat_all_BZ_females_BL$Q5)),
    rep("male", length(dat_all_BZ_males_AL$Q5)), 
    rep("female", length(dat_all_BZ_females_AL$Q5)),
    rep("male", length(dat_all_BZ_males_AP$Q5)),
    rep("female", length(dat_all_BZ_females_AP$Q5))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q5)),
    rep("BL", length(dat_all_BZ_females_BL$Q5)),
    rep("AL", length(dat_all_BZ_males_AL$Q5)), 
    rep("AL", length(dat_all_BZ_females_AL$Q5)),
    rep("AP", length(dat_all_BZ_males_AP$Q5)),
    rep("AP", length(dat_all_BZ_females_AP$Q5)))))



colnames(Q5_gender_long) <- c("score", "gender", "session")
Q5_gender_long$score <- ordered(Q5_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q5_gender_long$session  <- ordered(Q5_gender_long$session,  levels = c("BL", "AL", "AP"))
Q5_gender_long$gender <- as.factor(Q5_gender_long$gender )

str(Q5_gender_long)

model_Q5_gender <- clm(score ~ gender + session, data = Q5_gender_long)
model_Q5_gender_interaction <- clm(score ~ gender * session, data = Q5_gender_long)
Q5_gender_robust_vcov    <- sandwich(model_Q5_gender_interaction)
Q5_gender_robust_waldtest <- waldtest(model_Q5_gender, model_Q5_gender_interaction, vcov = Q5_gender_robust_vcov )

colnames(Q6_gender_long) <- c("score", "gender", "session")
Q6_gender_long$score <- ordered(Q6_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q6_gender_long$session  <- ordered(Q6_gender_long$session,  levels = c("BL", "AL", "AP"))
Q6_gender_long$gender <- as.factor(Q6_gender_long$gender )

str(Q6_gender_long)

model_Q6_gender <- clm(score ~ gender + session, data = Q6_gender_long)
model_Q6_gender_interaction <- clm(score ~ gender * session, data = Q6_gender_long)
Q6_gender_robust_vcov    <- sandwich(model_Q6_gender_interaction)
Q6_gender_robust_waldtest <- waldtest(model_Q6_gender, model_Q6_gender_interaction, vcov = Q6_gender_robust_vcov )

##### Q7
Q7_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q7,
    dat_all_BZ_females_BL$Q7,
    dat_all_BZ_males_AL$Q7, 
    dat_all_BZ_females_AL$Q7,
    dat_all_BZ_males_AP$Q7,
    dat_all_BZ_females_AP$Q7),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q7)),
    rep("female", length(dat_all_BZ_females_BL$Q7)),
    rep("male", length(dat_all_BZ_males_AL$Q7)), 
    rep("female", length(dat_all_BZ_females_AL$Q7)),
    rep("male", length(dat_all_BZ_males_AP$Q7)),
    rep("female", length(dat_all_BZ_females_AP$Q7))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q7)),
    rep("BL", length(dat_all_BZ_females_BL$Q7)),
    rep("AL", length(dat_all_BZ_males_AL$Q7)), 
    rep("AL", length(dat_all_BZ_females_AL$Q7)),
    rep("AP", length(dat_all_BZ_males_AP$Q7)),
    rep("AP", length(dat_all_BZ_females_AP$Q7)))))



colnames(Q7_gender_long) <- c("score", "gender", "session")
Q7_gender_long$score <- ordered(Q7_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q7_gender_long$session  <- ordered(Q7_gender_long$session,  levels = c("BL", "AL", "AP"))
Q7_gender_long$gender <- as.factor(Q7_gender_long$gender )

str(Q7_gender_long)

model_Q7_gender <- clm(score ~ gender + session, data = Q7_gender_long)
model_Q7_gender_interaction <- clm(score ~ gender * session, data = Q7_gender_long)
Q7_gender_robust_vcov    <- sandwich(model_Q7_gender_interaction)
Q7_gender_robust_waldtest <- waldtest(model_Q7_gender, model_Q7_gender_interaction, vcov = Q7_gender_robust_vcov )

##### Q8
Q8_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q8,
    dat_all_BZ_females_BL$Q8,
    dat_all_BZ_males_AL$Q8, 
    dat_all_BZ_females_AL$Q8,
    dat_all_BZ_males_AP$Q8,
    dat_all_BZ_females_AP$Q8),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q8)),
    rep("female", length(dat_all_BZ_females_BL$Q8)),
    rep("male", length(dat_all_BZ_males_AL$Q8)), 
    rep("female", length(dat_all_BZ_females_AL$Q8)),
    rep("male", length(dat_all_BZ_males_AP$Q8)),
    rep("female", length(dat_all_BZ_females_AP$Q8))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q8)),
    rep("BL", length(dat_all_BZ_females_BL$Q8)),
    rep("AL", length(dat_all_BZ_males_AL$Q8)), 
    rep("AL", length(dat_all_BZ_females_AL$Q8)),
    rep("AP", length(dat_all_BZ_males_AP$Q8)),
    rep("AP", length(dat_all_BZ_females_AP$Q8)))))



colnames(Q8_gender_long) <- c("score", "gender", "session")
Q8_gender_long$score <- ordered(Q8_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q8_gender_long$session  <- ordered(Q8_gender_long$session,  levels = c("BL", "AL", "AP"))
Q8_gender_long$gender <- as.factor(Q8_gender_long$gender )

model_Q8_gender <- clm(score ~ gender + session, data = Q8_gender_long)
model_Q8_gender_interaction <- clm(score ~ gender * session, data = Q8_gender_long)
Q8_gender_robust_vcov    <- sandwich(model_Q8_gender_interaction)
Q8_gender_robust_waldtest <- waldtest(model_Q8_gender, model_Q8_gender_interaction, vcov = Q8_gender_robust_vcov )



##### Q9
Q9_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q9,
    dat_all_BZ_females_BL$Q9,
    dat_all_BZ_males_AL$Q9, 
    dat_all_BZ_females_AL$Q9,
    dat_all_BZ_males_AP$Q9,
    dat_all_BZ_females_AP$Q9),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q9)),
    rep("female", length(dat_all_BZ_females_BL$Q9)),
    rep("male", length(dat_all_BZ_males_AL$Q9)), 
    rep("female", length(dat_all_BZ_females_AL$Q9)),
    rep("male", length(dat_all_BZ_males_AP$Q9)),
    rep("female", length(dat_all_BZ_females_AP$Q9))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q9)),
    rep("BL", length(dat_all_BZ_females_BL$Q9)),
    rep("AL", length(dat_all_BZ_males_AL$Q9)), 
    rep("AL", length(dat_all_BZ_females_AL$Q9)),
    rep("AP", length(dat_all_BZ_males_AP$Q9)),
    rep("AP", length(dat_all_BZ_females_AP$Q9)))))



colnames(Q9_gender_long) <- c("score", "gender", "session")
Q9_gender_long$score <- ordered(Q9_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q9_gender_long$session  <- ordered(Q9_gender_long$session,  levels = c("BL", "AL", "AP"))
Q9_gender_long$gender <- as.factor(Q9_gender_long$gender )

str(Q9_gender_long)

model_Q9_gender <- clm(score ~ gender + session, data = Q9_gender_long)
model_Q9_gender_interaction <- clm(score ~ gender * session, data = Q9_gender_long)
Q9_gender_robust_vcov    <- sandwich(model_Q9_gender_interaction)
Q9_gender_robust_waldtest <- waldtest(model_Q9_gender, model_Q9_gender_interaction, vcov = Q9_gender_robust_vcov )



##### Q10
Q10_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q10,
    dat_all_BZ_females_BL$Q10,
    dat_all_BZ_males_AL$Q10, 
    dat_all_BZ_females_AL$Q10,
    dat_all_BZ_males_AP$Q10,
    dat_all_BZ_females_AP$Q10),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q10)),
    rep("female", length(dat_all_BZ_females_BL$Q10)),
    rep("male", length(dat_all_BZ_males_AL$Q10)), 
    rep("female", length(dat_all_BZ_females_AL$Q10)),
    rep("male", length(dat_all_BZ_males_AP$Q10)),
    rep("female", length(dat_all_BZ_females_AP$Q10))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q10)),
    rep("BL", length(dat_all_BZ_females_BL$Q10)),
    rep("AL", length(dat_all_BZ_males_AL$Q10)), 
    rep("AL", length(dat_all_BZ_females_AL$Q10)),
    rep("AP", length(dat_all_BZ_males_AP$Q10)),
    rep("AP", length(dat_all_BZ_females_AP$Q10)))))



colnames(Q10_gender_long) <- c("score", "gender", "session")
Q10_gender_long$score <- ordered(Q10_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q10_gender_long$session  <- ordered(Q10_gender_long$session,  levels = c("BL", "AL", "AP"))
Q10_gender_long$gender <- as.factor(Q10_gender_long$gender )

str(Q10_gender_long)

model_Q10_gender <- clm(score ~ gender + session, data = Q10_gender_long)
model_Q10_gender_interaction <- clm(score ~ gender * session, data = Q10_gender_long)
Q10_gender_robust_vcov    <- sandwich(model_Q10_gender_interaction)
Q10_gender_robust_waldtest <- waldtest(model_Q10_gender, model_Q10_gender_interaction, vcov = Q10_gender_robust_vcov )





##### Q11
Q11_gender_long <- as.data.frame(cbind(
  
  c(
    dat_all_BZ_males_BL$Q11,
    dat_all_BZ_females_BL$Q11,
    dat_all_BZ_males_AL$Q11, 
    dat_all_BZ_females_AL$Q11,
    dat_all_BZ_males_AP$Q11,
    dat_all_BZ_females_AP$Q11),
  
  c(
    rep("male", length(dat_all_BZ_males_BL$Q11)),
    rep("female", length(dat_all_BZ_females_BL$Q11)),
    rep("male", length(dat_all_BZ_males_AL$Q11)), 
    rep("female", length(dat_all_BZ_females_AL$Q11)),
    rep("male", length(dat_all_BZ_males_AP$Q11)),
    rep("female", length(dat_all_BZ_females_AP$Q11))),
  
  c(
    rep("BL", length(dat_all_BZ_males_BL$Q11)),
    rep("BL", length(dat_all_BZ_females_BL$Q11)),
    rep("AL", length(dat_all_BZ_males_AL$Q11)), 
    rep("AL", length(dat_all_BZ_females_AL$Q11)),
    rep("AP", length(dat_all_BZ_males_AP$Q11)),
    rep("AP", length(dat_all_BZ_females_AP$Q11)))))



colnames(Q11_gender_long) <- c("score", "gender", "session")
Q11_gender_long$score <- ordered(Q11_gender_long$score, levels = c(1, 2, 3, 4, 5))
Q11_gender_long$session  <- ordered(Q11_gender_long$session,  levels = c("BL", "AL", "AP"))
Q11_gender_long$gender <- as.factor(Q11_gender_long$gender )

str(Q11_gender_long)

model_Q11_gender <- clm(score ~ gender + session, data = Q11_gender_long)
model_Q11_gender_interaction <- clm(score ~ gender * session, data = Q11_gender_long)
Q11_gender_robust_vcov    <- sandwich(model_Q11_gender_interaction)
Q11_gender_robust_waldtest <- waldtest(model_Q11_gender, model_Q11_gender_interaction, vcov = Q11_gender_robust_vcov )






writeLines(
  c("Q1", capture.output(Q1_gender_robust_waldtest),
    "Q2", capture.output(Q2_gender_robust_waldtest),
    "Q3", capture.output(Q3_gender_robust_waldtest),
    "Q4", capture.output(Q4_gender_robust_waldtest),
    "Q5", capture.output(Q5_gender_robust_waldtest),
    "Q6", capture.output(Q6_gender_robust_waldtest),
    "Q7", capture.output(Q7_gender_robust_waldtest),
    "Q8", capture.output(Q8_gender_robust_waldtest),
    "Q9", capture.output(Q9_gender_robust_waldtest),
    "Q10", capture.output(Q10_gender_robust_waldtest),
    "Q11", capture.output(Q11_gender_robust_waldtest),
    "FDR vals:",
    
    capture.output(p.adjust(
      c(
        0.01397,
        0.8153,
        0.6817,
        0.7775,
        0.9902,
        0.4826,
        0.4053,
        0.5466,
        0.3308,      
        0.3426,          
        0.062), method = "fdr"))), "gender_OReg_interaction_waldtest_out.txt")







### all non-sig - no need for FDR corr.              


####################################################################################################
### year



OLR_year_tests <- function(Q_table, Q){
  
  Y2023_Q <- subset(Q_table, Q_table$year == 2023)
  Y2024_Q <- subset(Q_table, Q_table$year == 2024)
  
  Y2023_Q_BL <- subset(Y2023_Q, Y2023_Q$session == "Before_lectures")        
  Y2023_Q_AL <- subset(Y2023_Q, Y2023_Q$session == "After_lectures")        
  Y2023_Q_AP <- subset(Y2023_Q, Y2023_Q$session == "After_prac")
  
  Y2024_Q_BL <- subset(Y2024_Q, Y2024_Q$session == "Before_lectures")        
  Y2024_Q_AL <- subset(Y2024_Q, Y2024_Q$session == "After_lectures")        
  Y2024_Q_AP <- subset(Y2024_Q, Y2024_Q$session == "After_prac")
  
  Y2023_Q_BL_v = eval(parse(text=paste('Y2023_Q_BL','$',Q,sep='')))
  Y2023_Q_AL_v = eval(parse(text=paste('Y2023_Q_AL','$',Q,sep='')))  
  Y2023_Q_AP_v = eval(parse(text=paste('Y2023_Q_AP','$',Q,sep='')))
  
  Y2024_Q_BL_v = eval(parse(text=paste('Y2024_Q_BL','$',Q,sep='')))
  Y2024_Q_AL_v = eval(parse(text=paste('Y2024_Q_AL','$',Q,sep='')))  
  Y2024_Q_AP_v = eval(parse(text=paste('Y2024_Q_AP','$',Q,sep='')))  
  
  Q_df_BL <- as.data.frame(cbind(
    c(Y2023_Q_BL_v, Y2024_Q_BL_v),
    c(rep("2023", length(Y2023_Q_BL_v)), rep("2024", length(Y2024_Q_BL_v)) )))
  
  colnames(Q_df_BL) <- c("score", "year")
  Q_df_BL$score    <- ordered(Q_df_BL$score, levels = c(1, 2, 3, 4, 5))
  Q_df_BL$year   <- as.factor(Q_df_BL$year)
  
  model_Q_BL <- clm(score ~ year, data = Q_df_BL)
  
  #### Robust standard errors (Huber-White Sandwich Estimator)
  Q_BL_robust_vcov    <- sandwich(model_Q_BL)
  Q_BL_robust_results <- coeftest(model_Q_BL, vcov. = Q_BL_robust_vcov)
  ## pairwise using the robust matrix
  Q_BL_robust_matrix   <- as.matrix(Q_BL_robust_vcov)
  Q_BL_year_emmeans <- emmeans(model_Q_BL, ~ year, vcov. = Q_BL_robust_matrix)
  Q_BL_pairs_result <- as.data.frame(pairs(Q_BL_year_emmeans, adjust = "none")) ### no adjust here as adjust with all Qs

  Q_df_AL <- as.data.frame(cbind(
    c(Y2023_Q_AL_v, Y2024_Q_AL_v),
    c(rep("2023", length(Y2023_Q_AL_v)), rep("2024", length(Y2024_Q_AL_v)) )))
  
  colnames(Q_df_AL) <- c("score", "year")
  Q_df_AL$score    <- ordered(Q_df_AL$score, levels = c(1, 2, 3, 4, 5))
  Q_df_AL$year   <- as.factor(Q_df_AL$year)
  
  model_Q_AL <- clm(score ~ year, data = Q_df_AL)
  
  #### Robust standard errors (Huber-White Sandwich Estimator)
  Q_AL_robust_vcov    <- sandwich(model_Q_AL)
  Q_AL_robust_results <- coeftest(model_Q_AL, vcov. = Q_AL_robust_vcov)
  ## pairwise using the robust matrix
  Q_AL_robust_matrix   <- as.matrix(Q_AL_robust_vcov)
  Q_AL_year_emmeans <- emmeans(model_Q_AL, ~ year, vcov. = Q_AL_robust_matrix)
  Q_AL_pairs_result <- as.data.frame(pairs(Q_AL_year_emmeans, adjust = "none")) ### no adjust here as adjust with all Qs
  
  Q_df_AP <- as.data.frame(cbind(
    c(Y2023_Q_AP_v, Y2024_Q_AP_v),
    c(rep("2023", length(Y2023_Q_AP_v)), rep("2024", length(Y2024_Q_AP_v)) )))
  
  colnames(Q_df_AP) <- c("score", "year")
  Q_df_AP$score    <- ordered(Q_df_AP$score, levels = c(1, 2, 3, 4, 5))
  Q_df_AP$year   <- as.factor(Q_df_AP$year)
  
  model_Q_AP <- clm(score ~ year, data = Q_df_AP)
  
  #### Robust standard errors (Huber-White Sandwich Estimator)
  Q_AP_robust_vcov    <- sandwich(model_Q_AP)
  Q_AP_robust_results <- coeftest(model_Q_AP, vcov. = Q_AP_robust_vcov)
  ## pairwise using the robust matrix
  Q_AP_robust_matrix   <- as.matrix(Q_AP_robust_vcov)
  Q_AP_year_emmeans <- emmeans(model_Q_AP, ~ year, vcov. = Q_AP_robust_matrix)
  Q_AP_pairs_result <- as.data.frame(pairs(Q_AP_year_emmeans, adjust = "none")) ### no adjust here as adjust with all Qs
  
  
  Q_pairs_result <- rbind(Q_BL_pairs_result, Q_AL_pairs_result, Q_AP_pairs_result)
  
  
  Q_pairs_result$Q <- c(Q,Q,Q)
  Q_pairs_result$session <- c("BL", "AL", "AP")
  
  return(Q_pairs_result)
}


OLR_year_tests_Q12Q13 <- function(Q_table, Q){
  
  Y2023_Q <- subset(Q_table, Q_table$year == 2023)
  Y2024_Q <- subset(Q_table, Q_table$year == 2024)
  
  Y2023_Q_AP <- subset(Y2023_Q, Y2023_Q$session == "After_prac")
  Y2024_Q_AP <- subset(Y2024_Q, Y2024_Q$session == "After_prac")

  Y2023_Q_AP_v = eval(parse(text=paste('Y2023_Q_AP','$',Q,sep='')))
  Y2024_Q_AP_v = eval(parse(text=paste('Y2024_Q_AP','$',Q,sep='')))  

  Q_df_AP <- as.data.frame(cbind(
    c(Y2023_Q_AP_v, Y2024_Q_AP_v),
    c(rep("2023", length(Y2023_Q_AP_v)), rep("2024", length(Y2024_Q_AP_v)) )))
  
  colnames(Q_df_AP) <- c("score", "year")
  Q_df_AP$score    <- ordered(Q_df_AP$score, levels = c(1, 2, 3, 4, 5))
  Q_df_AP$year   <- as.factor(Q_df_AP$year)
  
  model_Q_AP <- clm(score ~ year, data = Q_df_AP)
  
  #### Robust standard errors (Huber-White Sandwich Estimator)
  Q_AP_robust_vcov    <- sandwich(model_Q_AP)
  Q_AP_robust_results <- coeftest(model_Q_AP, vcov. = Q_AP_robust_vcov)
  ## pairwise using the robust matrix
  Q_AP_robust_matrix   <- as.matrix(Q_AP_robust_vcov)
  Q_AP_year_emmeans <- emmeans(model_Q_AP, ~ year, vcov. = Q_AP_robust_matrix)
  Q_AP_pairs_result <- as.data.frame(pairs(Q_AP_year_emmeans, adjust = "none")) ### no adjust here as adjust with all Qs
  
  
  Q_pairs_result <- rbind(Q_AP_pairs_result)
  
  
  Q_pairs_result$Q <- c(Q)
  Q_pairs_result$session <- c( "AP")
  
  return(Q_pairs_result)
}

year_all_OLRout <- rbind(
  OLR_year_tests(dat_all_BZ, "Q1"),
  OLR_year_tests(dat_all_BZ, "Q2"),  
  OLR_year_tests(dat_all_BZ, "Q3"),
  OLR_year_tests(dat_all_BZ, "Q4"),  
  OLR_year_tests(dat_all_BZ, "Q5"),  
  OLR_year_tests(dat_all_BZ, "Q6"),  
  OLR_year_tests(dat_all_BZ, "Q7"),  
  OLR_year_tests(dat_all_BZ, "Q8"),  
  OLR_year_tests(dat_all_BZ, "Q9"),  
  OLR_year_tests(dat_all_BZ, "Q10"),  
  OLR_year_tests(dat_all_BZ, "Q11"),  
  OLR_year_tests_Q12Q13(dat_all_BZ, "Q12"),
  OLR_year_tests_Q12Q13(dat_all_BZ, "Q13"))

year_all_OLRout$FDRall <- p.adjust(year_all_OLRout$p, method = "fdr")
write.csv(year_all_OLRout, "year_all_OLRout.csv")



##################################################################################################
################# effect sizes with bootstapped CIs

get_effectsize_year <- function(Q_table, Q, X){
    
    Y2023_Q <- subset(Q_table, Q_table$year == 2023)
    Y2024_Q <- subset(Q_table, Q_table$year == 2024)
    
    Y2023_Q_BL <- subset(Y2023_Q, Y2023_Q$session == "Before_lectures")        
    Y2023_Q_AL <- subset(Y2023_Q, Y2023_Q$session == "After_lectures")        
    Y2023_Q_AP <- subset(Y2023_Q, Y2023_Q$session == "After_prac")
    
    Y2024_Q_BL <- subset(Y2024_Q, Y2024_Q$session == "Before_lectures")        
    Y2024_Q_AL <- subset(Y2024_Q, Y2024_Q$session == "After_lectures")        
    Y2024_Q_AP <- subset(Y2024_Q, Y2024_Q$session == "After_prac")
    
    Y2023_Q_BL_v = eval(parse(text=paste('Y2023_Q_BL','$',Q,sep='')))
    Y2023_Q_AL_v = eval(parse(text=paste('Y2023_Q_AL','$',Q,sep='')))  
    Y2023_Q_AP_v = eval(parse(text=paste('Y2023_Q_AP','$',Q,sep='')))
    
    Y2024_Q_BL_v = eval(parse(text=paste('Y2024_Q_BL','$',Q,sep='')))
    Y2024_Q_AL_v = eval(parse(text=paste('Y2024_Q_AL','$',Q,sep='')))  
    Y2024_Q_AP_v = eval(parse(text=paste('Y2024_Q_AP','$',Q,sep='')))  
    
    WT_2023_BL_v_2024_BL_df <- data.frame(
      score = c(Y2023_Q_BL_v, Y2024_Q_BL_v),
      group = ordered(factor(c(rep("2023", length(Y2023_Q_BL_v)), rep("2024", length(Y2024_Q_BL_v)))), levels = c("2023", "2024"))
    )
    
    WT_2023_BL_v_2024_BL_result <- as.data.frame(cohens_d(
      data = WT_2023_BL_v_2024_BL_df, 
      formula = score ~ group, 
      paired = FALSE,
      ci = TRUE, 
      nboot = X, 
      ci.type = "perc"
    ))
    
    WT_2023_AL_v_2024_AL_df <- data.frame(
      score = c(Y2023_Q_AL_v, Y2024_Q_AL_v),
      group = ordered(factor(c(rep("2023", length(Y2023_Q_AL_v)), rep("2024", length(Y2024_Q_AL_v)))), levels = c("2023", "2024"))
    )
    
    WT_2023_AL_v_2024_AL_result <- as.data.frame(cohens_d(
      data = WT_2023_AL_v_2024_AL_df, 
      formula = score ~ group, 
      paired = FALSE,
      ci = TRUE, 
      nboot = X, 
      ci.type = "perc"
    ))
    
    WT_2023_AP_v_2024_AP_df <- data.frame(
      score = c(Y2023_Q_AP_v, Y2024_Q_AP_v),
      group = ordered(factor(c(rep("2023", length(Y2023_Q_AP_v)), rep("2024", length(Y2024_Q_AP_v)))), levels = c("2023", "2024"))
    )
    
    WT_2023_AP_v_2024_AP_result <- as.data.frame(cohens_d(
      data = WT_2023_AP_v_2024_AP_df, 
      formula = score ~ group, 
      paired = FALSE,
      ci = TRUE, 
      nboot = X, 
      ci.type = "perc"
    ))
    
    out_table <- as.data.frame(rbind(WT_2023_BL_v_2024_BL_result, WT_2023_AL_v_2024_AL_result, WT_2023_AP_v_2024_AP_result))
  
  out_table$Q <- c(rep(Q, 3))
  out_table$session <- c("BL", "AL", "AP")
  return(out_table)
}

get_effectsize_year_Q12Q13 <- function(Q_table, Q, X){
  
  Y2023_Q <- subset(Q_table, Q_table$year == 2023)
  Y2024_Q <- subset(Q_table, Q_table$year == 2024)
  
  Y2023_Q_AP <- subset(Y2023_Q, Y2023_Q$session == "After_prac")
  Y2024_Q_AP <- subset(Y2024_Q, Y2024_Q$session == "After_prac")
  
  Y2023_Q_AP_v = eval(parse(text=paste('Y2023_Q_AP','$',Q,sep='')))
  Y2024_Q_AP_v = eval(parse(text=paste('Y2024_Q_AP','$',Q,sep='')))  
  
  WT_2023_AP_v_2024_AP_df <- data.frame(
    score = c(Y2023_Q_AP_v, Y2024_Q_AP_v),
    group = ordered(factor(c(rep("2023", length(Y2023_Q_AP_v)), rep("2024", length(Y2024_Q_AP_v)))), levels = c("2023", "2024"))
  )
  
  WT_2023_AP_v_2024_AP_result <- as.data.frame(cohens_d(
    data = WT_2023_AP_v_2024_AP_df, 
    formula = score ~ group, 
    paired = FALSE,
    ci = TRUE, 
    nboot = X, 
    ci.type = "perc"
  ))
  
  out_table <- as.data.frame(rbind(WT_2023_AP_v_2024_AP_result))
  
  out_table$Q <- c(rep(Q, 1))
  out_table$session <- c("AP")
  return(out_table)
}

use_seed = 42
set.seed(use_seed )

year_all_effectsizes <- rbind(
  get_effectsize_year(dat_all_BZ, "Q1", Nboot),
  get_effectsize_year(dat_all_BZ, "Q2", Nboot),  
  get_effectsize_year(dat_all_BZ, "Q3", Nboot),
  get_effectsize_year(dat_all_BZ, "Q4", Nboot),  
  get_effectsize_year(dat_all_BZ, "Q5", Nboot),  
  get_effectsize_year(dat_all_BZ, "Q6", Nboot),  
  get_effectsize_year(dat_all_BZ, "Q7", Nboot),  
  get_effectsize_year(dat_all_BZ, "Q8", Nboot),  
  get_effectsize_year(dat_all_BZ, "Q9", Nboot),  
  get_effectsize_year(dat_all_BZ, "Q10", Nboot),  
  get_effectsize_year(dat_all_BZ, "Q11", Nboot),  
  get_effectsize_year_Q12Q13(dat_all_BZ, "Q12", Nboot),
  get_effectsize_year_Q12Q13(dat_all_BZ, "Q13", Nboot))

write.csv(year_all_effectsizes, paste("year_all_effectsizes", Nboot, "seed", use_seed, ".csv", sep = ""))



############### test for an interaction for all Qs


dat_all_BZ_2023 <- subset(dat_all_BZ, dat_all_BZ$year == 2023)
dat_all_BZ_2024 <- subset(dat_all_BZ, dat_all_BZ$year == 2024)

dat_all_BZ_2023_BL <- subset(dat_all_BZ_2023, dat_all_BZ_2023$session == "Before_lectures")        
dat_all_BZ_2023_AL <- subset(dat_all_BZ_2023, dat_all_BZ_2023$session == "After_lectures")        
dat_all_BZ_2023_AP <- subset(dat_all_BZ_2023, dat_all_BZ_2023$session == "After_prac")

dat_all_BZ_2024_BL <- subset(dat_all_BZ_2024, dat_all_BZ_2024$session == "Before_lectures")        
dat_all_BZ_2024_AL <- subset(dat_all_BZ_2024, dat_all_BZ_2024$session == "After_lectures")        
dat_all_BZ_2024_AP <- subset(dat_all_BZ_2024, dat_all_BZ_2024$session == "After_prac")


Q1_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q1,
    dat_all_BZ_2024_BL$Q1,
    dat_all_BZ_2023_AL$Q1, 
    dat_all_BZ_2024_AL$Q1,
    dat_all_BZ_2023_AP$Q1,
    dat_all_BZ_2024_AP$Q1),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q1)),
    rep("2024", length(dat_all_BZ_2024_BL$Q1)),
    rep("2023", length(dat_all_BZ_2023_AL$Q1)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q1)),
    rep("2023", length(dat_all_BZ_2023_AP$Q1)),
    rep("2024", length(dat_all_BZ_2024_AP$Q1))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q1)),
    rep("BL", length(dat_all_BZ_2024_BL$Q1)),
    rep("AL", length(dat_all_BZ_2023_AL$Q1)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q1)),
    rep("AP", length(dat_all_BZ_2023_AP$Q1)),
    rep("AP", length(dat_all_BZ_2024_AP$Q1)))))



colnames(Q1_year_long) <- c("score", "year", "session")
Q1_year_long$score    <- ordered(Q1_year_long$score, levels = c(1, 2, 3, 4, 5))
Q1_year_long$session  <- as.factor(Q1_year_long$session)
Q1_year_long$year     <- as.factor(Q1_year_long$year )

model_Q1_year             <- clm(score ~ year + session, data = Q1_year_long)
model_Q1_year_interaction <- clm(score ~ year * session, data = Q1_year_long)
Q1_year_robust_vcov    <- sandwich(model_Q1_year_interaction)
Q1_year_robust_waldtest <- waldtest(model_Q1_year, model_Q1_year_interaction, vcov = Q1_year_robust_vcov )


Q2_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q2,
    dat_all_BZ_2024_BL$Q2,
    dat_all_BZ_2023_AL$Q2, 
    dat_all_BZ_2024_AL$Q2,
    dat_all_BZ_2023_AP$Q2,
    dat_all_BZ_2024_AP$Q2),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q2)),
    rep("2024", length(dat_all_BZ_2024_BL$Q2)),
    rep("2023", length(dat_all_BZ_2023_AL$Q2)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q2)),
    rep("2023", length(dat_all_BZ_2023_AP$Q2)),
    rep("2024", length(dat_all_BZ_2024_AP$Q2))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q2)),
    rep("BL", length(dat_all_BZ_2024_BL$Q2)),
    rep("AL", length(dat_all_BZ_2023_AL$Q2)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q2)),
    rep("AP", length(dat_all_BZ_2023_AP$Q2)),
    rep("AP", length(dat_all_BZ_2024_AP$Q2)))))



colnames(Q2_year_long) <- c("score", "year", "session")
Q2_year_long$score    <- ordered(Q2_year_long$score, levels = c(1, 2, 3, 4, 5))
Q2_year_long$session  <- as.factor(Q2_year_long$session)
Q2_year_long$year     <- as.factor(Q2_year_long$year )

model_Q2_year             <- clm(score ~ year + session, data = Q2_year_long)
model_Q2_year_interaction <- clm(score ~ year * session, data = Q2_year_long)
Q2_year_robust_vcov    <- sandwich(model_Q2_year_interaction)
Q2_year_robust_waldtest <- waldtest(model_Q2_year, model_Q2_year_interaction, vcov = Q2_year_robust_vcov )

Q3_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q3,
    dat_all_BZ_2024_BL$Q3,
    dat_all_BZ_2023_AL$Q3, 
    dat_all_BZ_2024_AL$Q3,
    dat_all_BZ_2023_AP$Q3,
    dat_all_BZ_2024_AP$Q3),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q3)),
    rep("2024", length(dat_all_BZ_2024_BL$Q3)),
    rep("2023", length(dat_all_BZ_2023_AL$Q3)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q3)),
    rep("2023", length(dat_all_BZ_2023_AP$Q3)),
    rep("2024", length(dat_all_BZ_2024_AP$Q3))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q3)),
    rep("BL", length(dat_all_BZ_2024_BL$Q3)),
    rep("AL", length(dat_all_BZ_2023_AL$Q3)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q3)),
    rep("AP", length(dat_all_BZ_2023_AP$Q3)),
    rep("AP", length(dat_all_BZ_2024_AP$Q3)))))



colnames(Q3_year_long) <- c("score", "year", "session")
Q3_year_long$score    <- ordered(Q3_year_long$score, levels = c(1, 2, 3, 4, 5))
Q3_year_long$session  <- as.factor(Q3_year_long$session)
Q3_year_long$year     <- as.factor(Q3_year_long$year )

model_Q3_year             <- clm(score ~ year + session, data = Q3_year_long)
model_Q3_year_interaction <- clm(score ~ year * session, data = Q3_year_long)
Q3_year_robust_vcov    <- sandwich(model_Q3_year_interaction)
Q3_year_robust_waldtest <- waldtest(model_Q3_year, model_Q3_year_interaction, vcov = Q3_year_robust_vcov )


Q4_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q4,
    dat_all_BZ_2024_BL$Q4,
    dat_all_BZ_2023_AL$Q4, 
    dat_all_BZ_2024_AL$Q4,
    dat_all_BZ_2023_AP$Q4,
    dat_all_BZ_2024_AP$Q4),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q4)),
    rep("2024", length(dat_all_BZ_2024_BL$Q4)),
    rep("2023", length(dat_all_BZ_2023_AL$Q4)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q4)),
    rep("2023", length(dat_all_BZ_2023_AP$Q4)),
    rep("2024", length(dat_all_BZ_2024_AP$Q4))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q4)),
    rep("BL", length(dat_all_BZ_2024_BL$Q4)),
    rep("AL", length(dat_all_BZ_2023_AL$Q4)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q4)),
    rep("AP", length(dat_all_BZ_2023_AP$Q4)),
    rep("AP", length(dat_all_BZ_2024_AP$Q4)))))



colnames(Q4_year_long) <- c("score", "year", "session")
Q4_year_long$score    <- ordered(Q4_year_long$score, levels = c(1, 2, 3, 4, 5))
Q4_year_long$session  <- as.factor(Q4_year_long$session)
Q4_year_long$year     <- as.factor(Q4_year_long$year )

model_Q4_year             <- clm(score ~ year + session, data = Q4_year_long)
model_Q4_year_interaction <- clm(score ~ year * session, data = Q4_year_long)
Q4_year_robust_vcov    <- sandwich(model_Q4_year_interaction)
Q4_year_robust_waldtest <- waldtest(model_Q4_year, model_Q4_year_interaction, vcov = Q4_year_robust_vcov )


Q5_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q5,
    dat_all_BZ_2024_BL$Q5,
    dat_all_BZ_2023_AL$Q5, 
    dat_all_BZ_2024_AL$Q5,
    dat_all_BZ_2023_AP$Q5,
    dat_all_BZ_2024_AP$Q5),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q5)),
    rep("2024", length(dat_all_BZ_2024_BL$Q5)),
    rep("2023", length(dat_all_BZ_2023_AL$Q5)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q5)),
    rep("2023", length(dat_all_BZ_2023_AP$Q5)),
    rep("2024", length(dat_all_BZ_2024_AP$Q5))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q5)),
    rep("BL", length(dat_all_BZ_2024_BL$Q5)),
    rep("AL", length(dat_all_BZ_2023_AL$Q5)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q5)),
    rep("AP", length(dat_all_BZ_2023_AP$Q5)),
    rep("AP", length(dat_all_BZ_2024_AP$Q5)))))



colnames(Q5_year_long) <- c("score", "year", "session")
Q5_year_long$score    <- ordered(Q5_year_long$score, levels = c(1, 2, 3, 4, 5))
Q5_year_long$session  <- as.factor(Q5_year_long$session)
Q5_year_long$year     <- as.factor(Q5_year_long$year )

model_Q5_year             <- clm(score ~ year + session, data = Q5_year_long)
model_Q5_year_interaction <- clm(score ~ year * session, data = Q5_year_long)
Q5_year_robust_vcov    <- sandwich(model_Q5_year_interaction)
Q5_year_robust_waldtest <- waldtest(model_Q5_year, model_Q5_year_interaction, vcov = Q5_year_robust_vcov )


Q6_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q6,
    dat_all_BZ_2024_BL$Q6,
    dat_all_BZ_2023_AL$Q6, 
    dat_all_BZ_2024_AL$Q6,
    dat_all_BZ_2023_AP$Q6,
    dat_all_BZ_2024_AP$Q6),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q6)),
    rep("2024", length(dat_all_BZ_2024_BL$Q6)),
    rep("2023", length(dat_all_BZ_2023_AL$Q6)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q6)),
    rep("2023", length(dat_all_BZ_2023_AP$Q6)),
    rep("2024", length(dat_all_BZ_2024_AP$Q6))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q6)),
    rep("BL", length(dat_all_BZ_2024_BL$Q6)),
    rep("AL", length(dat_all_BZ_2023_AL$Q6)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q6)),
    rep("AP", length(dat_all_BZ_2023_AP$Q6)),
    rep("AP", length(dat_all_BZ_2024_AP$Q6)))))



colnames(Q6_year_long) <- c("score", "year", "session")
Q6_year_long$score    <- ordered(Q6_year_long$score, levels = c(1, 2, 3, 4, 5))
Q6_year_long$session  <- as.factor(Q6_year_long$session)
Q6_year_long$year     <- as.factor(Q6_year_long$year )

model_Q6_year             <- clm(score ~ year + session, data = Q6_year_long)
model_Q6_year_interaction <- clm(score ~ year * session, data = Q6_year_long)
Q6_year_robust_vcov    <- sandwich(model_Q6_year_interaction)
Q6_year_robust_waldtest <- waldtest(model_Q6_year, model_Q6_year_interaction, vcov = Q6_year_robust_vcov )


Q7_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q7,
    dat_all_BZ_2024_BL$Q7,
    dat_all_BZ_2023_AL$Q7, 
    dat_all_BZ_2024_AL$Q7,
    dat_all_BZ_2023_AP$Q7,
    dat_all_BZ_2024_AP$Q7),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q7)),
    rep("2024", length(dat_all_BZ_2024_BL$Q7)),
    rep("2023", length(dat_all_BZ_2023_AL$Q7)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q7)),
    rep("2023", length(dat_all_BZ_2023_AP$Q7)),
    rep("2024", length(dat_all_BZ_2024_AP$Q7))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q7)),
    rep("BL", length(dat_all_BZ_2024_BL$Q7)),
    rep("AL", length(dat_all_BZ_2023_AL$Q7)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q7)),
    rep("AP", length(dat_all_BZ_2023_AP$Q7)),
    rep("AP", length(dat_all_BZ_2024_AP$Q7)))))



colnames(Q7_year_long) <- c("score", "year", "session")
Q7_year_long$score    <- ordered(Q7_year_long$score, levels = c(1, 2, 3, 4, 5))
Q7_year_long$session  <- as.factor(Q7_year_long$session)
Q7_year_long$year     <- as.factor(Q7_year_long$year )

model_Q7_year             <- clm(score ~ year + session, data = Q7_year_long)
model_Q7_year_interaction <- clm(score ~ year * session, data = Q7_year_long)
Q7_year_robust_vcov    <- sandwich(model_Q7_year_interaction)
Q7_year_robust_waldtest <- waldtest(model_Q7_year, model_Q7_year_interaction, vcov = Q7_year_robust_vcov )


Q8_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q8,
    dat_all_BZ_2024_BL$Q8,
    dat_all_BZ_2023_AL$Q8, 
    dat_all_BZ_2024_AL$Q8,
    dat_all_BZ_2023_AP$Q8,
    dat_all_BZ_2024_AP$Q8),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q8)),
    rep("2024", length(dat_all_BZ_2024_BL$Q8)),
    rep("2023", length(dat_all_BZ_2023_AL$Q8)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q8)),
    rep("2023", length(dat_all_BZ_2023_AP$Q8)),
    rep("2024", length(dat_all_BZ_2024_AP$Q8))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q8)),
    rep("BL", length(dat_all_BZ_2024_BL$Q8)),
    rep("AL", length(dat_all_BZ_2023_AL$Q8)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q8)),
    rep("AP", length(dat_all_BZ_2023_AP$Q8)),
    rep("AP", length(dat_all_BZ_2024_AP$Q8)))))



colnames(Q8_year_long) <- c("score", "year", "session")
Q8_year_long$score    <- ordered(Q8_year_long$score, levels = c(1, 2, 3, 4, 5))
Q8_year_long$session  <- as.factor(Q8_year_long$session)
Q8_year_long$year     <- as.factor(Q8_year_long$year )

model_Q8_year             <- clm(score ~ year + session, data = Q8_year_long)
model_Q8_year_interaction <- clm(score ~ year * session, data = Q8_year_long)
Q8_year_robust_vcov    <- sandwich(model_Q8_year_interaction)
Q8_year_robust_waldtest <- waldtest(model_Q8_year, model_Q8_year_interaction, vcov = Q8_year_robust_vcov )


Q9_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q9,
    dat_all_BZ_2024_BL$Q9,
    dat_all_BZ_2023_AL$Q9, 
    dat_all_BZ_2024_AL$Q9,
    dat_all_BZ_2023_AP$Q9,
    dat_all_BZ_2024_AP$Q9),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q9)),
    rep("2024", length(dat_all_BZ_2024_BL$Q9)),
    rep("2023", length(dat_all_BZ_2023_AL$Q9)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q9)),
    rep("2023", length(dat_all_BZ_2023_AP$Q9)),
    rep("2024", length(dat_all_BZ_2024_AP$Q9))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q9)),
    rep("BL", length(dat_all_BZ_2024_BL$Q9)),
    rep("AL", length(dat_all_BZ_2023_AL$Q9)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q9)),
    rep("AP", length(dat_all_BZ_2023_AP$Q9)),
    rep("AP", length(dat_all_BZ_2024_AP$Q9)))))



colnames(Q9_year_long) <- c("score", "year", "session")
Q9_year_long$score    <- ordered(Q9_year_long$score, levels = c(1, 2, 3, 4, 5))
Q9_year_long$session  <- as.factor(Q9_year_long$session)
Q9_year_long$year     <- as.factor(Q9_year_long$year )

model_Q9_year             <- clm(score ~ year + session, data = Q9_year_long)
model_Q9_year_interaction <- clm(score ~ year * session, data = Q9_year_long)
Q9_year_robust_vcov    <- sandwich(model_Q9_year_interaction)
Q9_year_robust_waldtest <- waldtest(model_Q9_year, model_Q9_year_interaction, vcov = Q9_year_robust_vcov )


Q10_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q10,
    dat_all_BZ_2024_BL$Q10,
    dat_all_BZ_2023_AL$Q10, 
    dat_all_BZ_2024_AL$Q10,
    dat_all_BZ_2023_AP$Q10,
    dat_all_BZ_2024_AP$Q10),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q10)),
    rep("2024", length(dat_all_BZ_2024_BL$Q10)),
    rep("2023", length(dat_all_BZ_2023_AL$Q10)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q10)),
    rep("2023", length(dat_all_BZ_2023_AP$Q10)),
    rep("2024", length(dat_all_BZ_2024_AP$Q10))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q10)),
    rep("BL", length(dat_all_BZ_2024_BL$Q10)),
    rep("AL", length(dat_all_BZ_2023_AL$Q10)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q10)),
    rep("AP", length(dat_all_BZ_2023_AP$Q10)),
    rep("AP", length(dat_all_BZ_2024_AP$Q10)))))



colnames(Q10_year_long) <- c("score", "year", "session")
Q10_year_long$score    <- ordered(Q10_year_long$score, levels = c(1, 2, 3, 4, 5))
Q10_year_long$session  <- as.factor(Q10_year_long$session)
Q10_year_long$year     <- as.factor(Q10_year_long$year )

model_Q10_year             <- clm(score ~ year + session, data = Q10_year_long)
model_Q10_year_interaction <- clm(score ~ year * session, data = Q10_year_long)
Q10_year_robust_vcov    <- sandwich(model_Q10_year_interaction)
Q10_year_robust_waldtest <- waldtest(model_Q10_year, model_Q10_year_interaction, vcov = Q10_year_robust_vcov )


Q11_year_long <- as.data.frame(cbind(
  c(
    dat_all_BZ_2023_BL$Q11,
    dat_all_BZ_2024_BL$Q11,
    dat_all_BZ_2023_AL$Q11, 
    dat_all_BZ_2024_AL$Q11,
    dat_all_BZ_2023_AP$Q11,
    dat_all_BZ_2024_AP$Q11),
  
  c(
    rep("2023", length(dat_all_BZ_2023_BL$Q11)),
    rep("2024", length(dat_all_BZ_2024_BL$Q11)),
    rep("2023", length(dat_all_BZ_2023_AL$Q11)), 
    rep("2024", length(dat_all_BZ_2024_AL$Q11)),
    rep("2023", length(dat_all_BZ_2023_AP$Q11)),
    rep("2024", length(dat_all_BZ_2024_AP$Q11))),
  
  c(
    rep("BL", length(dat_all_BZ_2023_BL$Q11)),
    rep("BL", length(dat_all_BZ_2024_BL$Q11)),
    rep("AL", length(dat_all_BZ_2023_AL$Q11)), 
    rep("AL", length(dat_all_BZ_2024_AL$Q11)),
    rep("AP", length(dat_all_BZ_2023_AP$Q11)),
    rep("AP", length(dat_all_BZ_2024_AP$Q11)))))



colnames(Q11_year_long) <- c("score", "year", "session")
Q11_year_long$score    <- ordered(Q11_year_long$score, levels = c(1, 2, 3, 4, 5))
Q11_year_long$session  <- as.factor(Q11_year_long$session)
Q11_year_long$year     <- as.factor(Q11_year_long$year )

model_Q11_year             <- clm(score ~ year + session, data = Q11_year_long)
model_Q11_year_interaction <- clm(score ~ year * session, data = Q11_year_long)
Q11_year_robust_vcov    <- sandwich(model_Q11_year_interaction)
Q11_year_robust_waldtest <- waldtest(model_Q11_year, model_Q11_year_interaction, vcov = Q11_year_robust_vcov )







writeLines(
  c("Q1", capture.output(Q1_year_robust_waldtest),
    "Q2", capture.output(Q2_year_robust_waldtest),
    "Q3", capture.output(Q3_year_robust_waldtest),
    "Q4", capture.output(Q4_year_robust_waldtest),
    "Q5", capture.output(Q5_year_robust_waldtest),
    "Q6", capture.output(Q6_year_robust_waldtest),
    "Q7", capture.output(Q7_year_robust_waldtest),
    "Q8", capture.output(Q8_year_robust_waldtest),
    "Q9", capture.output(Q9_year_robust_waldtest),
    "Q10", capture.output(Q10_year_robust_waldtest),
    "Q11", capture.output(Q11_year_robust_waldtest)), "Year_OReg_interaction_out.txt")

                 
### FDR corr.

p.adjust(c(0.9412,
         0.7189,
         0.6552,
         0.2603,
         0.6449,
         0.225,
         0.5675,
         0.4398,
         0.3822,
         0.1416,
         0.6583), method = "BH")
                 
              
















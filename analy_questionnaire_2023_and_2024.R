library(ggplot2)
library(stringr)
library(cowplot)
#install.packages("HH")
library("HH")

#####################################################################################
###### data #########################################################################

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


wilcox_test_tests_wFDR <- function(BL_v, AL_v, AP_v, Q){

  WT_BL_v_AL_v <- wilcox.test(BL_v, AL_v)
  WT_BL_v_AP_v <- wilcox.test(BL_v, AP_v)
  WT_AL_v_AP_v <- wilcox.test(AL_v, AP_v)
  
  print( WT_BL_v_AL_v)
  print( WT_BL_v_AP_v)
  print( WT_AL_v_AP_v)
  
  stats_table <- as.data.frame(cbind(
    c(Q,Q,Q),
    c("BLvAL", "BLvAP", "ALvAP"),
    c(WT_BL_v_AL_v$p.value, WT_BL_v_AP_v$p.value, WT_AL_v_AP_v$p.value)))
  
  colnames(stats_table) <- c("Q", "comp", "p")
  
  stats_table$FDR <- p.adjust(stats_table$p, method = "fdr")
  
  return(stats_table)
  
}

dat_all_BZ_all_stats <- rbind(
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q1,  dat_all_BZ_AL$Q1,  dat_all_BZ_AP$Q1, " Q1"),
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q2,  dat_all_BZ_AL$Q2,  dat_all_BZ_AP$Q2,  "Q2"),
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q3,  dat_all_BZ_AL$Q3,  dat_all_BZ_AP$Q3,  "Q3"),
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q4,  dat_all_BZ_AL$Q4,  dat_all_BZ_AP$Q4,  "Q4"),
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q5,  dat_all_BZ_AL$Q5,  dat_all_BZ_AP$Q5,  "Q5"),
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q6,  dat_all_BZ_AL$Q6,  dat_all_BZ_AP$Q6,  "Q6"),
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q7,  dat_all_BZ_AL$Q7,  dat_all_BZ_AP$Q7,  "Q7"),
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q8,  dat_all_BZ_AL$Q8,  dat_all_BZ_AP$Q8,  "Q8"),
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q9,  dat_all_BZ_AL$Q9,  dat_all_BZ_AP$Q9,  "Q9"),
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q10, dat_all_BZ_AL$Q10, dat_all_BZ_AP$Q10, "Q10"),
  wilcox_test_tests_wFDR(dat_all_BZ_BL$Q11, dat_all_BZ_AL$Q11, dat_all_BZ_AP$Q11, "Q11"))

dat_all_BZ_all_stats$FDRall <- p.adjust(dat_all_BZ_all_stats$p, method = "fdr")

write.csv(dat_all_BZ_all_stats, "dat_all_BZ_all_stats.csv")



#######################################################################################################################
### gender

Q4_gender_BLALAP <- as.data.frame(rbind(
  count_12345_in_vector(dat_all_BZ_males_BL$Q4, "males_BL_Q4"),
  count_12345_in_vector(dat_all_BZ_females_BL$Q4, "females_BL_Q4"),
  count_12345_in_vector(dat_all_BZ_other_BL$Q4, "other_BL_Q4"),
  count_12345_in_vector(dat_all_BZ_males_AL$Q4, "males_AL_Q4"),
  count_12345_in_vector(dat_all_BZ_females_AL$Q4, "females_AL_Q4"), 
  count_12345_in_vector(dat_all_BZ_other_AL$Q4, "other_AL_Q4"), 
  count_12345_in_vector(dat_all_BZ_males_AP$Q4, "males_AP_Q4"),
  count_12345_in_vector(dat_all_BZ_females_AP$Q4, "females_AP_Q4"),
  count_12345_in_vector(dat_all_BZ_other_AP$Q4, "other_AP_Q4")))

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



out_height_g = 6

pdf("Q4_gender_BLALAP_LP.pdf", width = out_width, height = out_height_g)
plot.likert(Q4_gender_BLALAP, as.percent=TRUE, main = "Understanding insects will help me get a job after I graduate.", xlim=c(-100,120))
dev.off()
getwd() ## where has my plot gone....?

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




wilcox_test_gender <- function(male_v, female_v, other_v, Q){
  
  WT_male_v_female_v  <- wilcox.test(male_v,   female_v)
  WT_male_v_other_v   <- wilcox.test(male_v,   other_v)
  WT_female_v_other_v <- wilcox.test(female_v, other_v)
  
  print(WT_male_v_female_v)
  print(WT_male_v_other_v )
  print(WT_female_v_other_v)
  
  stats_table <- as.data.frame(cbind(
    c(Q,Q,Q),
    c("malevfemale", "malevother", "femalevother"),
    c(WT_male_v_female_v$p.value, WT_male_v_other_v$p.value, WT_female_v_other_v$p.value)))
  
  colnames(stats_table) <- c("Q", "comp", "p")
  
  stats_table$FDR <- p.adjust(stats_table$p, method = "fdr")
  
  return(stats_table)
  
}

gender_stats <- rbind(
  wilcox_test_gender(dat_all_BZ_males_BL$Q1,  dat_all_BZ_females_BL$Q1, dat_all_BZ_other_BL$Q1, "Q1_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q1,  dat_all_BZ_females_AL$Q1, dat_all_BZ_other_AL$Q1, "Q1_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q1,  dat_all_BZ_females_AP$Q1, dat_all_BZ_other_AP$Q1, "Q1_AP"),
  wilcox_test_gender(dat_all_BZ_males_BL$Q2,  dat_all_BZ_females_BL$Q2, dat_all_BZ_other_BL$Q2, "Q2_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q2,  dat_all_BZ_females_AL$Q2, dat_all_BZ_other_AL$Q2, "Q2_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q2,  dat_all_BZ_females_AP$Q2, dat_all_BZ_other_AP$Q2, "Q2_AP"),
  wilcox_test_gender(dat_all_BZ_males_BL$Q3,  dat_all_BZ_females_BL$Q3, dat_all_BZ_other_BL$Q3, "Q3_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q3,  dat_all_BZ_females_AL$Q3, dat_all_BZ_other_AL$Q3, "Q3_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q3,  dat_all_BZ_females_AP$Q3, dat_all_BZ_other_AP$Q3, "Q3_AP"),
  wilcox_test_gender(dat_all_BZ_males_BL$Q4,  dat_all_BZ_females_BL$Q4, dat_all_BZ_other_BL$Q4, "Q4_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q4,  dat_all_BZ_females_AL$Q4, dat_all_BZ_other_AL$Q4, "Q4_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q4,  dat_all_BZ_females_AP$Q4, dat_all_BZ_other_AP$Q4, "Q4_AP"),
  wilcox_test_gender(dat_all_BZ_males_BL$Q5,  dat_all_BZ_females_BL$Q5, dat_all_BZ_other_BL$Q5, "Q5_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q5,  dat_all_BZ_females_AL$Q5, dat_all_BZ_other_AL$Q5, "Q5_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q5,  dat_all_BZ_females_AP$Q5, dat_all_BZ_other_AP$Q5, "Q5_AP"),
  wilcox_test_gender(dat_all_BZ_males_BL$Q6,  dat_all_BZ_females_BL$Q6, dat_all_BZ_other_BL$Q6, "Q6_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q6,  dat_all_BZ_females_AL$Q6, dat_all_BZ_other_AL$Q6, "Q6_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q6,  dat_all_BZ_females_AP$Q6, dat_all_BZ_other_AP$Q6, "Q6_AP"),
  wilcox_test_gender(dat_all_BZ_males_BL$Q7,  dat_all_BZ_females_BL$Q7, dat_all_BZ_other_BL$Q7, "Q7_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q7,  dat_all_BZ_females_AL$Q7, dat_all_BZ_other_AL$Q7, "Q7_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q7,  dat_all_BZ_females_AP$Q7, dat_all_BZ_other_AP$Q7, "Q7_AP"),
  wilcox_test_gender(dat_all_BZ_males_BL$Q8,  dat_all_BZ_females_BL$Q8, dat_all_BZ_other_BL$Q8, "Q8_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q8,  dat_all_BZ_females_AL$Q8, dat_all_BZ_other_AL$Q8, "Q8_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q8,  dat_all_BZ_females_AP$Q8, dat_all_BZ_other_AP$Q8, "Q8_AP"),
  wilcox_test_gender(dat_all_BZ_males_BL$Q9,  dat_all_BZ_females_BL$Q9, dat_all_BZ_other_BL$Q9, "Q9_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q9,  dat_all_BZ_females_AL$Q9, dat_all_BZ_other_AL$Q9, "Q9_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q9,  dat_all_BZ_females_AP$Q9, dat_all_BZ_other_AP$Q9, "Q9_AP"),
  wilcox_test_gender(dat_all_BZ_males_BL$Q10,  dat_all_BZ_females_BL$Q10, dat_all_BZ_other_BL$Q10, "Q10_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q10,  dat_all_BZ_females_AL$Q10, dat_all_BZ_other_AL$Q10, "Q10_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q10,  dat_all_BZ_females_AP$Q10, dat_all_BZ_other_AP$Q10, "Q10_AP"),
  wilcox_test_gender(dat_all_BZ_males_BL$Q11,  dat_all_BZ_females_BL$Q11, dat_all_BZ_other_BL$Q11, "Q11_BL"),
  wilcox_test_gender(dat_all_BZ_males_AL$Q11,  dat_all_BZ_females_AL$Q11, dat_all_BZ_other_AL$Q11, "Q11_AL"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q11,  dat_all_BZ_females_AP$Q11, dat_all_BZ_other_AP$Q11, "Q11_AP"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q12,  dat_all_BZ_females_AP$Q12, dat_all_BZ_other_AP$Q12, "Q12_AP"),
  wilcox_test_gender(dat_all_BZ_males_AP$Q13,  dat_all_BZ_females_AP$Q13, dat_all_BZ_other_AP$Q13, "Q13_AP"))


gender_stats$FDRall <- p.adjust(gender_stats$p, method = "fdr")

write.csv(gender_stats, "all_20232024_gender_stats.csv")


####################################################################################################
### year


wilcox_test_year <- function(Q_table, Q){
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
  
  WT_2023_2024_BL   <- wilcox.test(Y2023_Q_BL_v,   Y2024_Q_BL_v)
  WT_2023_2024_AL   <- wilcox.test(Y2023_Q_AL_v,   Y2024_Q_AL_v)
  WT_2023_2024_AP   <- wilcox.test(Y2023_Q_AP_v,   Y2024_Q_AP_v)
  
  print(  WT_2023_2024_BL)
  
  stats_table <- as.data.frame(cbind(
    c(Q,Q,Q),
    c("BL", "AL", "AP"),
    c(WT_2023_2024_BL$p.value, WT_2023_2024_AL$p.value, WT_2023_2024_AP$p.value)))
  
  colnames(stats_table) <- c("Q", "comp", "p")
  
  stats_table$FDR <- p.adjust(stats_table$p, method = "fdr")
  return(stats_table)
}  

wilcox_test_year_Q12Q13 <- function(Q_table, Q){
  Y2023_Q <- subset(Q_table, Q_table$year == 2023)
  Y2024_Q <- subset(Q_table, Q_table$year == 2024)
 
  Y2023_Q_AP <- subset(Y2023_Q, Y2023_Q$session == "After_prac")
  Y2024_Q_AP <- subset(Y2024_Q, Y2024_Q$session == "After_prac")
  Y2023_Q_AP_v = eval(parse(text=paste('Y2023_Q_AP','$',Q,sep='')))
  Y2024_Q_AP_v = eval(parse(text=paste('Y2024_Q_AP','$',Q,sep='')))
  WT_2023_2024_AP   <- wilcox.test(Y2023_Q_AP_v,   Y2024_Q_AP_v)
  
  stats_table <- as.data.frame(cbind(
    c(Q),
    c( "AP"),
    c(WT_2023_2024_AP$p.value)))
  
  colnames(stats_table) <- c("Q", "comp", "p")
  
  stats_table$FDR <- p.adjust(stats_table$p, method = "fdr")
  return(stats_table)
}  

year_stats <- rbind(
wilcox_test_year(dat_all_BZ, "Q1"),
wilcox_test_year(dat_all_BZ, "Q2"),  
wilcox_test_year(dat_all_BZ, "Q3"),
wilcox_test_year(dat_all_BZ, "Q4"),  
wilcox_test_year(dat_all_BZ, "Q5"),  
wilcox_test_year(dat_all_BZ, "Q6"),  
wilcox_test_year(dat_all_BZ, "Q7"),  
wilcox_test_year(dat_all_BZ, "Q8"),  
wilcox_test_year(dat_all_BZ, "Q9"),  
wilcox_test_year(dat_all_BZ, "Q10"),  
wilcox_test_year(dat_all_BZ, "Q11"),  
wilcox_test_year_Q12Q13(dat_all_BZ, "Q12"),
wilcox_test_year_Q12Q13(dat_all_BZ, "Q13"))

year_stats$FDRall <- p.adjust(year_stats$p, method = "fdr")
write.csv(year_stats, "all_20232024_year_stats.csv")


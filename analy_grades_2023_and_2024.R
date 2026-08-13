library(ggplot2)
library(stringr)
library(cowplot)
library(rstatix)
setwd("output_20232024")

### NO_OSstudent == is for OS and ES students (all non-biozoo!)

dat_2023 <- read.csv("../data/grades/All_grades_and_attendence_2020_to_2023.csv")
dat_2024 <- read.csv("../data/grades/All_grades_and_attendence_2024.csv")

### join
head(dat_2023) ### MCQC = insect one
head(dat_2024) ### MCQD = insect one

dat1 <- as.data.frame(cbind(
c(dat_2023$year, dat_2024$year),
c(dat_2023$MCQ_A, dat_2024$MCQ_A),
c(dat_2023$MCQ_B, dat_2024$MCQ_B),
c(dat_2023$MCQ_C, dat_2024$MCQ_D),
c(dat_2023$Exam, dat_2024$Exam),
c(dat_2023$Attended_prac, dat_2024$Attended_prac)))

colnames(dat1) = c("year", "MCQ_A", "MCQ_B", "MCQ_C", "Exam", "Attended_prac")
dat1$MCQ_A <- as.numeric(dat1$MCQ_A) 
dat1$MCQ_B <- as.numeric(dat1$MCQ_B) 
dat1$MCQ_C <- as.numeric(dat1$MCQ_C) 
dat1$Exam <- as.numeric(dat1$Exam) 
str(dat1)

levels(as.factor(dat1$Attended_prac))
dat1 <- subset(dat1, dat1$Attended_prac != "Non_registered") ## drop unregistered student.
dat1 <- subset(dat1, dat1$Attended_prac != "NO_OSstudent")   ## drop OS/ES student (as they don't do the prac)
levels(as.factor(dat1$Attended_prac))
head(dat1) 

####

head(dat1)
dat1$exam_MCQC_delta <- dat1$MCQ_C / dat1$Exam  
dat1$MCQA_MCQC_delta <- dat1$MCQ_C / ((dat1$MCQ_A + dat1$MCQ_B) / 2)
table(dat1$Attended_prac, useNA = "ifany")
table(dat1$MCQ_C, useNA = "ifany")
dat1 <- dat1[!is.na(dat1$Attended_prac),]
dat1$MCQ_A_fail <- ifelse(dat1$MCQ_A < 40, "Fail", "Pass")
dat1$MCQ_B_fail <- ifelse(dat1$MCQ_B < 40, "Fail", "Pass")
dat1$MCQ_C_fail <- ifelse(dat1$MCQ_C < 40, "Fail", "Pass")
dat1$Exam_fail  <- ifelse(dat1$Exam < 40, "Fail", "Pass")

dat1$MCQ_A_Attempt <- ifelse(dat1$MCQ_A  == 0, "No attempt", "Attempted")
dat1$MCQ_B_Attempt <- ifelse(dat1$MCQ_B == 0, "No attempt", "Attempted")
dat1$MCQ_C_Attempt <- ifelse(dat1$MCQ_C == 0, "No attempt", "Attempted")
dat1$Exam_Attempt  <- ifelse(dat1$Exam == 0 , "No attempt", "Attempted")

dat1$Attended_prac_2 <- ifelse(dat1$Attended_prac == "NO_OSstudent", "NO", dat1$Attended_prac)

dat1_202220232024 <- subset(dat1, dat1$year %in% c("2022", "2023", "2024"))
dat1_202220232024$year <- as.factor(dat1_202220232024$year)
levels(as.factor(dat1$year))
levels(as.factor(dat1_202220232024$year))



dat2 <- as.data.frame(cbind(
  c(dat1$year, dat1$year, dat1$year, dat1$year),
  c(dat1$MCQ_A, dat1$MCQ_B, dat1$MCQ_C, dat1$Exam),
  c(rep("MCQ_A", length(dat1[,1])), rep("MCQ_B", length(dat1[,1])), rep("MCQ_C", length(dat1[,1])), rep("Exam", length(dat1[,1]))),
  c(dat1$Attended_prac, dat1$Attended_prac, dat1$Attended_prac, dat1$Attended_prac),
  c(dat1$Attended_prac_2, dat1$Attended_prac_2, dat1$Attended_prac_2, dat1$Attended_prac_2)
))

colnames(dat2) <- c("year", "score", "test", "Attended_prac",  "Attended_prac_2")
table(dat2$Attended_prac)
table(dat2$Attended_prac_2)

head(dat2)
dat2 <- na.omit(dat2)
dat2$score <- as.numeric(dat2$score)
dat2$fail <- ifelse(dat2$score < 40, "Fail", "Pass")
dat2$attempted <- ifelse(dat2$score == 0 , "No attempt", "Attempted")
str(dat2 )
table(dat2$fail)
table(dat2$attempted)

dat2_2024 <- subset(dat2, dat2$year == "2024")
dat2_2023 <- subset(dat2, dat2$year == "2023")
dat2_2022 <- subset(dat2, dat2$year == "2022")
dat2_2021 <- subset(dat2, dat2$year == "2021")
dat2_2020 <- subset(dat2, dat2$year == "2020")
dat2_20222023 <- rbind(dat2_2023, dat2_2022)
dat2_202220232024 <- rbind(dat2_2024, dat2_2023, dat2_2022)


### check grades look right
ggplot(dat2 , aes(as.factor(year), score)) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(test)), position = position_dodge2(preserve = "single")) 

### grades

P1_MCQC <- ggplot(dat1 , aes(as.factor(year), MCQ_C)) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(Attended_prac_2)), position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("NO" = "#56B4E9" , "YES" =  "#E69F00" , "No_prac"  = "grey")) + 
  theme(legend.position = "none") + xlab("Year") + ylab("Score on MCQ C") + ylim(c(0, 105))

P2_MCQC <-ggplot(dat1 , aes(as.factor(year), exam_MCQC_delta)) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(Attended_prac_2)), position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("NO" = "#56B4E9" , "YES" =  "#E69F00" , "No_prac"  = "grey")) + 
  theme(legend.position = "none") + xlab("Year") + ylab("Relative score on MCQ C")

P3_MCQC <-ggplot(dat1 , aes(as.factor(year), MCQA_MCQC_delta)) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(Attended_prac_2)), position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("NO" = "#56B4E9" , "YES" =  "#E69F00" , "No_prac"  = "grey")) + 
  theme(legend.position = "none") + xlab("Year") + ylab("Relative score on MCQ C")

pdf("grades_Attended_prac_2.pdf", width = 7, height = 10)
plot_grid(P1_MCQC, P2_MCQC, P3_MCQC, ncol = 1)
dev.off()

pdf("grades_legend_Attended_prac_2.pdf", width = 7, height = 3.3)
ggplot(dat1 , aes(as.factor(year), MCQ_C)) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(Attended_prac_2)), position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("NO" = "#56B4E9" , "YES" =  "#E69F00" , "No_prac"  = "grey")) 
dev.off()

# without 2020 and 2021


P1_MCQCa <- ggplot(dat1_202220232024 , aes(as.factor(year), MCQ_C)) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(Attended_prac_2)), position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("NO" = "#56B4E9" , "YES" =  "#E69F00" , "No_prac"  = "grey")) + 
  theme(legend.position = "none") + xlab("Year") + ylab("Score on MCQ C") + ylim(c(0, 105))

P2_MCQCa <- ggplot(dat1_202220232024 , aes(as.factor(year), exam_MCQC_delta)) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(Attended_prac_2)), position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("NO" = "#56B4E9" , "YES" =  "#E69F00" , "No_prac"  = "grey")) + 
  theme(legend.position = "none") + xlab("Year") + ylab("Relative score on MCQ C")

P3_MCQCa <-ggplot(dat1_202220232024 , aes(as.factor(year), MCQA_MCQC_delta)) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(Attended_prac_2)), position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("NO" = "#56B4E9" , "YES" =  "#E69F00" , "No_prac"  = "grey")) + 
  theme(legend.position = "none") + xlab("Year") + ylab("Relative score on MCQ C")

pdf("grades_Attended_prac_2a.pdf", width = 7, height = 10)
plot_grid(P1_MCQCa, P2_MCQCa, P3_MCQCa, ncol = 1)
dev.off()

pdf("grades_legend_Attended_prac_2a.pdf", width = 7, height = 3.3)
ggplot(dat1_202220232024 , aes(as.factor(year), MCQ_C)) + 
  theme_bw() +
  geom_boxplot(aes(fill = factor(Attended_prac_2)), position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values=c("NO" = "#56B4E9" , "YES" =  "#E69F00" , "No_prac"  = "grey")) 
dev.off()





#### wilcox

dat1_2024 <- subset(dat1, dat1$year == 2024)
dat1_2024_att <- subset(dat1_2024, dat1_2024$Attended_prac_2 == "YES")
dat1_2024_abs <- subset(dat1_2024, dat1_2024$Attended_prac_2 == "NO")
dat1_2023 <- subset(dat1, dat1$year == 2023)
dat1_2023_att <- subset(dat1_2023, dat1_2023$Attended_prac_2 == "YES")
dat1_2023_abs <- subset(dat1_2023, dat1_2023$Attended_prac_2 == "NO")
dat1_2022 <- subset(dat1, dat1$year == 2022)
dat1_2022_att <- subset(dat1_2022, dat1_2022$Attended_prac_2 == "YES")
dat1_2022_abs <- subset(dat1_2022, dat1_2022$Attended_prac_2 == "NO")
dat1_2021 <- subset(dat1, dat1$year == 2021)
dat1_2020 <- subset(dat1, dat1$year == 2020)

MCQC_wilcox <- as.data.frame(cbind(
c("2024", "2023", "2022", "2024", "2023", "2022", "2024", "2023", "2022"),
c("MCQC", "MCQC", "MCQC", "rMCQ_exam",  "rMCQ_exam", "rMCQ_exam", "rMCQ_MCQAB", "rMCQ_MCQAB", "rMCQ_MCQAB"),
c(  wilcox.test(dat1_2024_att$MCQ_C, dat1_2024_abs$MCQ_C)$p.value,
    wilcox.test(dat1_2023_att$MCQ_C, dat1_2023_abs$MCQ_C)$p.value,
    wilcox.test(dat1_2022_att$MCQ_C, dat1_2022_abs$MCQ_C)$p.value,
    wilcox.test(dat1_2024_att$exam_MCQC_delta, dat1_2024_abs$exam_MCQC_delta)$p.value,
    wilcox.test(dat1_2023_att$exam_MCQC_delta, dat1_2023_abs$exam_MCQC_delta)$p.value,
    wilcox.test(dat1_2022_att$exam_MCQC_delta, dat1_2022_abs$exam_MCQC_delta)$p.value,
    wilcox.test(dat1_2024_att$MCQA_MCQC_delta, dat1_2024_abs$MCQA_MCQC_delta)$p.value,
    wilcox.test(dat1_2023_att$MCQA_MCQC_delta, dat1_2023_abs$MCQA_MCQC_delta)$p.value,
    wilcox.test(dat1_2022_att$MCQA_MCQC_delta, dat1_2022_abs$MCQA_MCQC_delta)$p.value)))

MCQC_wilcox$V4 <-  p.adjust(MCQC_wilcox$V3)
colnames(MCQC_wilcox ) <- c("Year", "test", "wilcox_p", "wilcox_FDR")
write.csv(MCQC_wilcox, "MCQC_wilcox_Attended_prac_2.csv")



#### FAILS LR

colnames(dat1_202220232024)


### make a factor for previous experience (if pass A and B == 2, A or B = 1, neither = 0 )
dat1_202220232024$MCQ_A_fail_01s <- as.numeric(ifelse(dat1_202220232024$MCQ_A_fail == "Pass", 1,
                                                      ifelse(dat1_202220232024$MCQ_A_fail == "Fail", 0, dat1_202220232024$MCQ_A_fail)))

dat1_202220232024$MCQ_B_fail_01s <- as.numeric(ifelse(dat1_202220232024$MCQ_B_fail == "Pass", 1,
                                                      ifelse(dat1_202220232024$MCQ_B_fail == "Fail", 0, dat1_202220232024$MCQ_B_fail)))

dat1_202220232024$MCQ_AB_fail_factor <- as.factor(dat1_202220232024$MCQ_A_fail_01s + dat1_202220232024$MCQ_B_fail_01s)


### make 0 1 for log reg
dat1_202220232024$MCQ_C_fail_01s <- as.numeric(ifelse(dat1_202220232024$MCQ_C_fail == "Pass", 1,
                                           ifelse(dat1_202220232024$MCQ_C_fail == "Fail", 0, dat1_202220232024$MCQ_C_fail)))


dat1_202220232024$Exam_fail_01s <- as.numeric(ifelse(dat1_202220232024$Exam_fail == "Pass", 1,
                                                     ifelse(dat1_202220232024$Exam_fail == "Fail", 0, dat1_202220232024$Exam_fail)))

### logR


LR_MCQ_C <- glm(MCQ_C_fail_01s ~ Attended_prac_2 * year + MCQ_AB_fail_factor , family = binomial (link=logit), data = dat1_202220232024)
drop1(LR_MCQ_C, ~., test="Chisq")

writeLines(capture.output(drop1(LR_MCQ_C, ~., test="Chisq")), "LR_MCQ_C_out.txt")

# LR_Exam  <- glm(Exam_fail_01s ~ Attended_prac_2 * year + MCQ_AB_fail_factor , family = binomial (link=logit), data = dat1_202220232024)
# drop1(LR_Exam, ~., test="Chisq")
# writeLines(capture.output(drop1(LR_Exam, ~., test="Chisq")), "LR_Exam_out.txt")

#### FAILS

FT_fail_df <- as.data.frame(cbind(
c("MCQ_A", "MCQ_A", "MCQ_A", "MCQ_B", "MCQ_B", "MCQ_B", "MCQ_C", "MCQ_C","MCQ_C",  "Exam", "Exam", "Exam"),
c("2024", "2023", "2022", "2024", "2023", "2022", "2024", "2023", "2022", "2024", "2023", "2022"),

c(fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2024_att$MCQ_A_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2024_att$MCQ_A_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2024_abs$MCQ_A_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2024_abs$MCQ_A_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

c(fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2023_att$MCQ_A_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2023_att$MCQ_A_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2023_abs$MCQ_A_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2023_abs$MCQ_A_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2022_att$MCQ_A_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2022_att$MCQ_A_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2022_abs$MCQ_A_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2022_abs$MCQ_A_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2024_att$MCQ_B_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2024_att$MCQ_B_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2024_abs$MCQ_B_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2024_abs$MCQ_B_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2023_att$MCQ_B_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2023_att$MCQ_B_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2023_abs$MCQ_B_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2023_abs$MCQ_B_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2022_att$MCQ_B_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2022_att$MCQ_B_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2022_abs$MCQ_B_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2022_abs$MCQ_B_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2024_att$MCQ_C_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2024_att$MCQ_C_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2024_abs$MCQ_C_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2024_abs$MCQ_C_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2023_att$MCQ_C_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2023_att$MCQ_C_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2023_abs$MCQ_C_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2023_abs$MCQ_C_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2022_att$MCQ_C_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2022_att$MCQ_C_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2022_abs$MCQ_C_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2022_abs$MCQ_C_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2024_att$Exam_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2024_att$Exam_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2024_abs$Exam_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2024_abs$Exam_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2023_att$Exam_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2023_att$Exam_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2023_abs$Exam_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2023_abs$Exam_fail == "Pass", 1, 0), na.rm = T)
    ))))$p,

fisher.test(
  as.data.frame(rbind(
    c(sum(ifelse(dat1_2022_att$Exam_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2022_att$Exam_fail == "Pass", 1, 0), na.rm = T)),
    c(sum(ifelse(dat1_2022_abs$Exam_fail == "Fail", 1, 0), na.rm = T), sum(ifelse(dat1_2022_abs$Exam_fail == "Pass", 1, 0), na.rm = T)
    ))))$p))))

colnames(FT_fail_df) <- c("Assessment", "Year", "p")
FT_fail_df$FDR <- p.adjust(FT_fail_df$p, method = "fdr")


Fail_pass_N_df <- as.data.frame(rbind(
  
c(sum(ifelse(dat1_2024_att$MCQ_A_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_att$MCQ_A_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_abs$MCQ_A_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_abs$MCQ_A_fail == "Pass", 1, 0), na.rm = T)),
c(sum(ifelse(dat1_2023_att$MCQ_A_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_att$MCQ_A_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_abs$MCQ_A_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_abs$MCQ_A_fail == "Pass", 1, 0), na.rm = T)),
c(sum(ifelse(dat1_2022_att$MCQ_A_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_att$MCQ_A_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_abs$MCQ_A_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_abs$MCQ_A_fail == "Pass", 1, 0), na.rm = T)),


c(sum(ifelse(dat1_2024_att$MCQ_B_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_att$MCQ_B_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_abs$MCQ_B_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_abs$MCQ_B_fail == "Pass", 1, 0), na.rm = T)),
c(sum(ifelse(dat1_2023_att$MCQ_B_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_att$MCQ_B_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_abs$MCQ_B_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_abs$MCQ_B_fail == "Pass", 1, 0), na.rm = T)),
c(sum(ifelse(dat1_2022_att$MCQ_B_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_att$MCQ_B_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_abs$MCQ_B_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_abs$MCQ_B_fail == "Pass", 1, 0), na.rm = T)),

c(sum(ifelse(dat1_2024_att$MCQ_C_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_att$MCQ_C_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_abs$MCQ_C_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_abs$MCQ_C_fail == "Pass", 1, 0), na.rm = T)),
c(sum(ifelse(dat1_2023_att$MCQ_C_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_att$MCQ_C_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_abs$MCQ_C_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_abs$MCQ_C_fail == "Pass", 1, 0), na.rm = T)),
c(sum(ifelse(dat1_2022_att$MCQ_C_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_att$MCQ_C_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_abs$MCQ_C_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_abs$MCQ_C_fail == "Pass", 1, 0), na.rm = T)),

c(sum(ifelse(dat1_2024_att$Exam_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_att$Exam_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_abs$Exam_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2024_abs$Exam_fail == "Pass", 1, 0), na.rm = T)),
c(sum(ifelse(dat1_2023_att$Exam_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_att$Exam_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_abs$Exam_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2023_abs$Exam_fail == "Pass", 1, 0), na.rm = T)),
c(sum(ifelse(dat1_2022_att$Exam_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_att$Exam_fail == "Pass", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_abs$Exam_fail == "Fail", 1, 0), na.rm = T),
  sum(ifelse(dat1_2022_abs$Exam_fail == "Pass", 1, 0), na.rm = T))))


colnames(Fail_pass_N_df ) <- c("Fail_att", "Pass_att", "Fail_abs", "Pass_abs")
Fail_pass_N_df$att_fail_perc <- Fail_pass_N_df$Fail_att / (Fail_pass_N_df$Pass_att + Fail_pass_N_df$Fail_att) * 100
Fail_pass_N_df$abs_fail_perc <- Fail_pass_N_df$Fail_abs / (Fail_pass_N_df$Pass_abs + Fail_pass_N_df$Fail_abs) * 100
Fail_pass_N_df

Fail_pass_N_df <- as.data.frame(cbind(Fail_pass_N_df, FT_fail_df))

write.csv(Fail_pass_N_df , "FT_fail_attabs.csv")


Fail_pass_N_df_l <- as.data.frame(cbind(
c(Fail_pass_N_df$att_fail_perc, Fail_pass_N_df$abs_fail_perc),
c(rep("att", length(Fail_pass_N_df[,1])), rep("abs", length(Fail_pass_N_df[,1]))),
c(Fail_pass_N_df$Assessment, Fail_pass_N_df$Assessment),
c(Fail_pass_N_df$Year, Fail_pass_N_df$Year)))

colnames(Fail_pass_N_df_l) <- c("fail_per", "attendence", "assessment", "year")


Fail_pass_N_df_l_20212020 <- as.data.frame(cbind(c(
sum(ifelse(dat1_2021$MCQ_A_fail == "Fail", 1, 0), na.rm = T) / (sum(ifelse(dat1_2021$MCQ_A_fail == "Fail", 1, 0), na.rm = T) + sum(ifelse(dat1_2021$MCQ_A_fail == "Pass", 1, 0), na.rm = T)) * 100,
sum(ifelse(dat1_2021$MCQ_B_fail == "Fail", 1, 0), na.rm = T) / (sum(ifelse(dat1_2021$MCQ_B_fail == "Fail", 1, 0), na.rm = T) + sum(ifelse(dat1_2021$MCQ_B_fail == "Pass", 1, 0), na.rm = T)) * 100,
sum(ifelse(dat1_2021$MCQ_C_fail == "Fail", 1, 0), na.rm = T) / (sum(ifelse(dat1_2021$MCQ_C_fail == "Fail", 1, 0), na.rm = T) + sum(ifelse(dat1_2021$MCQ_C_fail == "Pass", 1, 0), na.rm = T)) * 100,
sum(ifelse(dat1_2021$Exam_fail  == "Fail", 1, 0), na.rm = T) / (sum(ifelse(dat1_2021$Exam_fail  == "Fail", 1, 0), na.rm = T) + sum(ifelse(dat1_2021$Exam_fail == "Pass", 1, 0), na.rm = T)) * 100,
sum(ifelse(dat1_2020$MCQ_A_fail == "Fail", 1, 0), na.rm = T) / (sum(ifelse(dat1_2020$MCQ_A_fail == "Fail", 1, 0), na.rm = T) + sum(ifelse(dat1_2020$MCQ_A_fail == "Pass", 1, 0), na.rm = T)) * 100,
sum(ifelse(dat1_2020$MCQ_B_fail == "Fail", 1, 0), na.rm = T) / (sum(ifelse(dat1_2020$MCQ_B_fail == "Fail", 1, 0), na.rm = T) + sum(ifelse(dat1_2020$MCQ_B_fail == "Pass", 1, 0), na.rm = T)) * 100,
sum(ifelse(dat1_2020$MCQ_C_fail == "Fail", 1, 0), na.rm = T) / (sum(ifelse(dat1_2020$MCQ_C_fail == "Fail", 1, 0), na.rm = T) + sum(ifelse(dat1_2020$MCQ_C_fail == "Pass", 1, 0), na.rm = T)) * 100,
sum(ifelse(dat1_2020$Exam_fail  == "Fail", 1, 0), na.rm = T) / (sum(ifelse(dat1_2020$Exam_fail  == "Fail", 1, 0), na.rm = T) + sum(ifelse(dat1_2020$Exam_fail == "Pass", 1, 0), na.rm = T)) * 100),
rep("No_prac", 8),
c("MCQ_A", "MCQ_B", "MCQ_C", "Exam", "MCQ_A", "MCQ_B", "MCQ_C", "Exam"),
c("2021","2021","2021","2021","2020", "2020","2020","2020")))

colnames(Fail_pass_N_df_l_20212020) <-  c("fail_per", "attendence", "assessment", "year")

Fail_pass_N_df_l <- rbind(Fail_pass_N_df_l, Fail_pass_N_df_l_20212020)

Fail_pass_N_df_l$fail_per <- as.numeric(Fail_pass_N_df_l$fail_per)
Fail_pass_N_df_l$assessment_year <- paste(Fail_pass_N_df_l$assessment, Fail_pass_N_df_l$year, sep = "_")
str(Fail_pass_N_df_l)
Fail_pass_N_df_l$assessment_year_o <- ordered(Fail_pass_N_df_l$assessment_year, levels=c("MCQ_A_2020", "MCQ_B_2020", "MCQ_C_2020", "Exam_2020",
                                                                                         "MCQ_A_2021", "MCQ_B_2021", "MCQ_C_2021", "Exam_2021",
                                                                                         "MCQ_A_2022", "MCQ_B_2022", "MCQ_C_2022", "Exam_2022",
                                                                                         "MCQ_A_2023", "MCQ_B_2023", "MCQ_C_2023", "Exam_2023",
                                                                                         "MCQ_A_2024", "MCQ_B_2024", "MCQ_C_2024", "Exam_2024") )

# ggplot(Fail_pass_N_df_l, aes(x = factor(assessment_year_o), y = fail_per, fill = attendence))+ 
#   geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
#   theme_bw() 


ggplot(subset(Fail_pass_N_df_l, Fail_pass_N_df_l$assessment == "Exam") , aes(x = factor(assessment_year_o), y = fail_per, fill = attendence))+ 
  geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
  theme_bw() + ylim(0, max(Fail_pass_N_df_l$fail_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey"))


pdf("fails.pdf", width = 8, height = 6)
plot_grid(
ggplot(subset(Fail_pass_N_df_l, Fail_pass_N_df_l$assessment == "MCQ_A") , aes(x = factor(assessment_year_o), y = fail_per, fill = attendence))+ 
  geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
  theme_bw() + ylim(0, max(Fail_pass_N_df_l$fail_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey")),

ggplot(subset(Fail_pass_N_df_l, Fail_pass_N_df_l$assessment == "MCQ_B") , aes(x = factor(assessment_year_o), y = fail_per, fill = attendence))+ 
  geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
  theme_bw() + ylim(0, max(Fail_pass_N_df_l$fail_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey")),   

ggplot(subset(Fail_pass_N_df_l, Fail_pass_N_df_l$assessment == "MCQ_C") , aes(x = factor(assessment_year_o), y = fail_per, fill = attendence))+ 
  geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
  theme_bw() + ylim(0, max(Fail_pass_N_df_l$fail_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey")) ,  

ggplot(subset(Fail_pass_N_df_l, Fail_pass_N_df_l$assessment == "Exam") , aes(x = factor(assessment_year_o), y = fail_per, fill = attendence))+ 
  geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
  theme_bw() + ylim(0, max(Fail_pass_N_df_l$fail_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey")),
ncol = 2)
dev.off()


#### without 2020. 2021

Fail_pass_N_df_l_20220232024 <- subset(Fail_pass_N_df_l, Fail_pass_N_df_l$year %in% c("2022", "2023", "2024"))

pdf("fails_20220232024.pdf", width = 8, height = 6)
plot_grid(
  ggplot(subset(Fail_pass_N_df_l_20220232024, Fail_pass_N_df_l_20220232024$assessment == "MCQ_A") , aes(x = factor(assessment_year_o), y = fail_per, fill = attendence))+ 
    geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
    theme_bw() + ylim(0, max(Fail_pass_N_df_l$fail_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey")),
  
  ggplot(subset(Fail_pass_N_df_l_20220232024, Fail_pass_N_df_l_20220232024$assessment == "MCQ_B") , aes(x = factor(assessment_year_o), y = fail_per, fill = attendence))+ 
    geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
    theme_bw() + ylim(0, max(Fail_pass_N_df_l$fail_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey")),   
  
  ggplot(subset(Fail_pass_N_df_l_20220232024, Fail_pass_N_df_l_20220232024$assessment == "MCQ_C") , aes(x = factor(assessment_year_o), y = fail_per, fill = attendence))+ 
    geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
    theme_bw() + ylim(0, max(Fail_pass_N_df_l$fail_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey")) ,  
  
  ggplot(subset(Fail_pass_N_df_l_20220232024, Fail_pass_N_df_l_20220232024$assessment == "Exam") , aes(x = factor(assessment_year_o), y = fail_per, fill = attendence))+ 
    geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
    theme_bw() + ylim(0, max(Fail_pass_N_df_l$fail_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey")),
  ncol = 2)
dev.off()

####### more likely to attempt MCQC?


colnames(dat1_202220232024)

### make a factor for previous experience (if attempt A and B == 2, A or B = 1, neither = 0 )
dat1_202220232024$MCQ_A_Attempt_01s <- as.numeric(ifelse(dat1_202220232024$MCQ_A_Attempt == "Attempted", 1,
                                                         ifelse(dat1_202220232024$MCQ_A_Attempt == "No attempt", 0, dat1_202220232024$MCQ_A_Attempt)))

dat1_202220232024$MCQ_B_Attempt_01s <- as.numeric(ifelse(dat1_202220232024$MCQ_B_Attempt == "Attempted", 1,
                                                         ifelse(dat1_202220232024$MCQ_B_Attempt == "No attempt", 0, dat1_202220232024$MCQ_B_Attempt)))

dat1_202220232024$MCQ_AB_Attempt_factor <- as.factor(dat1_202220232024$MCQ_A_Attempt_01s + dat1_202220232024$MCQ_B_Attempt_01s)


### make 0 1 for log reg
dat1_202220232024$MCQ_C_Attempt_01s <- as.numeric(ifelse(dat1_202220232024$MCQ_C_Attempt == "Attempted", 1,
                                                         ifelse(dat1_202220232024$MCQ_C_Attempt == "No attempt", 0, dat1_202220232024$MCQ_C_Attempt)))


dat1_202220232024$Exam_Attempt_01s <- as.numeric(ifelse(dat1_202220232024$Exam_Attempt == "Attempted", 1,
                                                        ifelse(dat1_202220232024$Exam_Attempt == "No attempt", 0, dat1_202220232024$Exam_Attempt)))



str(dat1_202220232024)


### logR
#LR_MCQ_C_attempt <- glm(MCQ_C_Attempt_01s ~ Attended_prac_2 * year + MCQ_AB_Attempt_factor , family = binomial (link=logit), data = dat1_202220232024) ### can't fit interaction (glm.fit: fitted probabilities numerically 0 or 1 occurred) - but giives same result for attended prac without it
LR_MCQ_C_attempt <- glm(MCQ_C_Attempt_01s ~ Attended_prac_2 + year + MCQ_AB_Attempt_factor , family = binomial (link=logit), data = dat1_202220232024) ### can't fit interaction (glm.fit: fitted probabilities numerically 0 or 1 occurred) - but giives same result for attended prac without it
drop1(LR_MCQ_C_attempt, ~., test="Chisq")

#### fails excluding students that did not attempt MCQC

dat1_202220232024_attemptedC <- subset(dat1_202220232024, dat1_202220232024$MCQ_C_Attempt == "Attempted")
LR_MCQ_C_attemptedC <- glm(MCQ_C_fail_01s ~ Attended_prac_2 * year + MCQ_AB_fail_factor , family = binomial (link=logit), data = dat1_202220232024_attemptedC)
drop1(LR_MCQ_C_attemptedC, ~., test="Chisq")


#### fails excluding students that did not attempt the previous ones and MCQ C

dat1_202220232024$MCQABC_attempted <- paste(dat1_202220232024$MCQ_A_Attempt, dat1_202220232024$MCQ_B_Attempt, dat1_202220232024$MCQ_C_Attempt, sep = "_") 
dat1_202220232024_attempted <- subset(dat1_202220232024, dat1_202220232024$MCQABC_attempted == "Attempted_Attempted_Attempted")
LR_MCQ_C_attemptedABC <- glm(MCQ_C_fail_01s ~ Attended_prac_2 * year + MCQ_AB_fail_factor , family = binomial (link=logit), data = dat1_202220232024_attempted)
drop1(LR_MCQ_C_attemptedABC , ~., test="Chisq")

writeLines(capture.output(drop1(LR_MCQ_C_attempt, ~., test="Chisq")), "LR_MCQ_C_attempt_out.txt")
writeLines(capture.output(drop1(LR_MCQ_C_attemptedC, ~., test="Chisq")), "LR_MCQ_C_attemptedMCQC_out.txt")
writeLines(capture.output(drop1(LR_MCQ_C_attemptedABC , ~., test="Chisq")), "LR_MCQ_C_attemptedMCQABC_out.txt")

#### AttemptS Fisher tests


FT_Attempt_df <- as.data.frame(cbind(
  c("MCQ_C", "MCQ_C", "MCQ_C"),
  c("2024", "2023", "2022"),
  c(fisher.test(
        as.data.frame(rbind(
          c(sum(ifelse(dat1_2024_att$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T), sum(ifelse(dat1_2024_att$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T)),
          c(sum(ifelse(dat1_2024_abs$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T), sum(ifelse(dat1_2024_abs$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T)
          ))))$p,
      
      fisher.test(
        as.data.frame(rbind(
          c(sum(ifelse(dat1_2023_att$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T), sum(ifelse(dat1_2023_att$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T)),
          c(sum(ifelse(dat1_2023_abs$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T), sum(ifelse(dat1_2023_abs$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T)
          ))))$p,
      
      fisher.test(
        as.data.frame(rbind(
          c(sum(ifelse(dat1_2022_att$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T), sum(ifelse(dat1_2022_att$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T)),
          c(sum(ifelse(dat1_2022_abs$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T), sum(ifelse(dat1_2022_abs$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T)
          ))))$p)))

colnames(FT_Attempt_df) <- c("Assessment", "Year", "p")
FT_Attempt_df$FDR <- p.adjust(FT_Attempt_df$p, method = "fdr")


Attempt_pass_N_df <- as.data.frame(rbind(
  
  c(sum(ifelse(dat1_2024_att$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T),
    sum(ifelse(dat1_2024_att$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T),
    sum(ifelse(dat1_2024_abs$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T),
    sum(ifelse(dat1_2024_abs$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T)),    
  c(sum(ifelse(dat1_2023_att$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T),
    sum(ifelse(dat1_2023_att$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T),
    sum(ifelse(dat1_2023_abs$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T),
    sum(ifelse(dat1_2023_abs$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T)),
  c(sum(ifelse(dat1_2022_att$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T),
    sum(ifelse(dat1_2022_att$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T),
    sum(ifelse(dat1_2022_abs$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T),
    sum(ifelse(dat1_2022_abs$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T))))

colnames(Attempt_pass_N_df ) <- c("No_attempt_att", "Yes_attempt_att", "No_attempt_abs", "Yes_attempt_abs")
Attempt_pass_N_df$att_No_attempt_perc <- Attempt_pass_N_df$No_attempt_att / (Attempt_pass_N_df$Yes_attempt_att + Attempt_pass_N_df$No_attempt_att) * 100
Attempt_pass_N_df$abs_No_attempt_perc <- Attempt_pass_N_df$No_attempt_abs / (Attempt_pass_N_df$Yes_attempt_abs + Attempt_pass_N_df$No_attempt_abs) * 100
Attempt_pass_N_df

Attempt_pass_N_df <- as.data.frame(cbind(Attempt_pass_N_df, FT_Attempt_df))

write.csv(Attempt_pass_N_df , "FT_Attempt_attabs.csv")


Attempt_pass_N_df_l <- as.data.frame(cbind(
  c(Attempt_pass_N_df$att_No_attempt_perc, Attempt_pass_N_df$abs_No_attempt_perc),
  c(rep("att", length(Attempt_pass_N_df[,1])), rep("abs", length(Attempt_pass_N_df[,1]))),
  c(Attempt_pass_N_df$Assessment, Attempt_pass_N_df$Assessment),
  c(Attempt_pass_N_df$Year, Attempt_pass_N_df$Year)))

colnames(Attempt_pass_N_df_l) <- c("No_attempt_per", "attendence", "assessment", "year")


Attempt_pass_N_df_l_20212020 <- as.data.frame(cbind(c(
  sum(ifelse(dat1_2021$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T) / (sum(ifelse(dat1_2021$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T) + sum(ifelse(dat1_2021$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T)) * 100,
  sum(ifelse(dat1_2020$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T) / (sum(ifelse(dat1_2020$MCQ_C_Attempt == "No attempt", 1, 0), na.rm = T) + sum(ifelse(dat1_2020$MCQ_C_Attempt == "Attempted", 1, 0), na.rm = T)) * 100),
  rep("No_prac", 2),
  c("MCQ_C", "MCQ_C"),
  c("2021","2020")))

colnames(Attempt_pass_N_df_l_20212020) <-  c("No_attempt_per", "attendence", "assessment", "year")

Attempt_pass_N_df_l <- rbind(Attempt_pass_N_df_l, Attempt_pass_N_df_l_20212020)

Attempt_pass_N_df_l$No_attempt_per <- as.numeric(Attempt_pass_N_df_l$No_attempt_per)
Attempt_pass_N_df_l$assessment_year <- paste(Attempt_pass_N_df_l$assessment, Attempt_pass_N_df_l$year, sep = "_")
str(Attempt_pass_N_df_l)
Attempt_pass_N_df_l$assessment_year_o <- ordered(Attempt_pass_N_df_l$assessment_year, levels=c("MCQ_C_2020", "MCQ_C_2021", "MCQ_C_2022", "MCQ_C_2023", "MCQ_C_2024") )

# ggplot(Attempt_pass_N_df_l, aes(x = factor(assessment_year_o), y = Attempt_per, fill = attendence))+ 
#   geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
#   theme_bw() 


pdf("Attempts.pdf", width = 4, height = 3)
ggplot(Attempt_pass_N_df_l , aes(x = factor(assessment_year_o), y = No_attempt_per, fill = attendence))+ 
  geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
  theme_bw() + ylim(0, max(Attempt_pass_N_df_l$No_attempt_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey"))
dev.off()

#### without 2020. 2021

Attempt_pass_N_df_l_20220232024 <- subset(Attempt_pass_N_df_l, Attempt_pass_N_df_l$year %in% c("2022", "2023", "2024"))

pdf("Attempts_20220232024.pdf", width = 4, height = 3)
ggplot(Attempt_pass_N_df_l_20220232024 , aes(x = factor(assessment_year_o), y = No_attempt_per, fill = attendence))+ 
  geom_bar(position=position_dodge2(preserve = "single"),stat="identity", lwd = 1) + 
  theme_bw() + ylim(0, max(Attempt_pass_N_df_l$No_attempt_per * 1.02)) + scale_fill_manual(values=c("abs" = "#56B4E9" , "att" =  "#E69F00" , "No_prac"  = "grey"))
dev.off()




##################################################################################################
################# effect sizes with bootstapped CIs

#### overall effect size 

use_seed = 42
N_boot = 5000
set.seed(use_seed )

MCQ_C_fail_01_overall_effectsize <- as.data.frame(cohens_d(
  data = dat1_202220232024, 
  formula = MCQ_C_fail_01s ~ Attended_prac_2, 
  paired = FALSE,
  ci = TRUE, 
  nboot = N_boot, 
  ci.type = "perc"
))
set.seed(use_seed )
MCQ_C_attempted_overall_effectsize <- as.data.frame(cohens_d(
  data = dat1_202220232024, 
  formula = MCQ_C_Attempt_01s ~ Attended_prac_2, 
  paired = FALSE,
  ci = TRUE, 
  nboot = N_boot, 
  ci.type = "perc"
))
set.seed(use_seed )
MCQ_C_fail_01_overall_effectsize_attemptedABC <- as.data.frame(cohens_d(
  data = dat1_202220232024_attempted, 
  formula = MCQ_C_fail_01s ~ Attended_prac_2, 
  paired = FALSE,
  ci = TRUE, 
  nboot = N_boot, 
  ci.type = "perc"
))
set.seed(use_seed )
MCQ_C_fail_01_overall_effectsize_attemptedC <- as.data.frame(cohens_d(
  data = dat1_202220232024_attemptedC, 
  formula = MCQ_C_fail_01s ~ Attended_prac_2, 
  paired = FALSE,
  ci = TRUE, 
  nboot = N_boot, 
  ci.type = "perc"
))

MCQC_effectsizes <- as.data.frame(rbind(
  MCQ_C_fail_01_overall_effectsize,
  MCQ_C_attempted_overall_effectsize,
  MCQ_C_fail_01_overall_effectsize_attemptedABC,
  MCQ_C_fail_01_overall_effectsize_attemptedC
))

MCQC_effectsizes$comp <- c(  "MCQ_C_fail_01_overall_effectsize",
                             "MCQ_C_attempted_overall_effectsize",
                             "MCQ_C_fail_01_overall_effectsize_attemptedABC",
                             "MCQ_C_fail_01_overall_effectsize_attemptedC")


write.csv(MCQC_effectsizes, paste("MCQC_effectsizes", N_boot, "seed", use_seed, ".csv", sep = ""))


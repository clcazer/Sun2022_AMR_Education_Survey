require(checkpoint)
checkpoint("2022-11-10")

require(tidyverse)
require(plotrix)
require(matrixStats)
require(scales)
require(gridExtra)

####load and evaluate data####

before <- read.csv("data/AMR Survey_CL 2021_before clinics_deidentified.csv", na.strings=c("", "NA"), stringsAsFactors = F)
View(before)
head(before)
names(before)
dim(before)

after <- read.csv("data/AMR Survey_CL 2021_after clinics_deidentified.csv", na.strings=c("", "NA"), stringsAsFactors = F)
View(after)
head(after)
names(after)
dim(after)

#questions
Qs <- read.csv("data/AMR Survey Questions.csv")
Qs #all questions

#question sections
confAM_Q_index <- match("Q5_1", colnames(Qs)):match("Q5_7", colnames(Qs)) #AM confidence questions
AM_Q_index <- match("Q6_1", colnames(Qs)):match("Q15", colnames(Qs)) #AM knowledge questions
confAMR_Q_index <- match("Q17_1", colnames(Qs)):match("Q17_6", colnames(Qs)) #AMR confidence questions (note there is no Q16)
AMR_Q_index <- match("Q18", colnames(Qs)):match("Q23_9", colnames(Qs)) #AMR knowledge questions
confAMS_Q_index <- match("Q25_1", colnames(Qs)):match("Q25_10", colnames(Qs)) #AMS confidence questions (note there is no Q24 or 26)
guideline_Q_index <- match("Q27", colnames(Qs)):match("Q29_20_TEXT", colnames(Qs)) #AM guideline questions
perception_Q_index <- match("Q30_1", colnames(Qs)):match("Q30_6", colnames(Qs)) #perception of AMR contributions, exclude the 'other" text
AMS_Q_index <- match("Q31_1", colnames(Qs)):match("Q32", colnames(Qs)) #AMS knowledge questions
clinical_Q_index <- match("Q34", colnames(Qs)):match("Q42", colnames(Qs)) #clinical scenario questions (note there is no Q 33)
confQs <- names(Qs[c(confAM_Q_index, confAMR_Q_index, confAMS_Q_index)])

#number of surveys
#completion indicated in the "finished" column with T/F

sink("Results/results.txt")

cat("\n number of surveys - before \n")
nrow(before)
cat("\n number of surveys - before, complete \n")
sum(before$Finished==TRUE)
cat("\n number of surveys - after \n")
nrow(after)
cat("\n number of surveys - after, complete \n")
sum(after$Finished==TRUE)

sink()


####Confidence####
#note that there are no students who partially completed a confidence section, they either answered all questions or none.
rowNAsums <- function(x) sum(is.na(x))
apply(before[,confAM_Q_index], MARGIN=1, FUN=rowNAsums) == 0|length(confAM_Q_index)
apply(before[,confAMR_Q_index], MARGIN=1, FUN=rowNAsums) == 0|length(confAMR_Q_index)
apply(before[,confAMS_Q_index], MARGIN=1, FUN=rowNAsums) == 0|length(confAMS_Q_index)

#number of complete responses by section
sink("Results/results.txt", append=T)
cat("\n number of complete confidence responses - before AM, AMR, AMS, total \n")
sum(apply(before[,confAM_Q_index], MARGIN=1, FUN=rowNAsums) == 0)
sum(apply(before[,confAMR_Q_index], MARGIN=1, FUN=rowNAsums) == 0)
sum(apply(before[,confAMS_Q_index], MARGIN=1, FUN=rowNAsums) == 0)
sum(apply(before[,confQs], MARGIN=1, FUN=rowNAsums) == 0)

cat("\n number of complete confidence responses - after AM, AMR, AMS, total \n")
sum(apply(after[,confAM_Q_index], MARGIN=1, FUN=rowNAsums) == 0)
sum(apply(after[,confAMR_Q_index], MARGIN=1, FUN=rowNAsums) == 0)
sum(apply(after[,confAMS_Q_index], MARGIN=1, FUN=rowNAsums) == 0)
sum(apply(after[,confQs], MARGIN=1, FUN=rowNAsums) == 0)

sink()

#order confidence factors
before[,c(confAM_Q_index, confAMR_Q_index, confAMS_Q_index)] <- lapply(before[,c(confAM_Q_index, confAMR_Q_index, confAMS_Q_index)] , function(x) factor(x, ordered = TRUE, levels = c("Not at all confident","Slightly confident", "Somewhat confident","Very confident")))
after[,c(confAM_Q_index, confAMR_Q_index, confAMS_Q_index)] <- lapply(after[,c(confAM_Q_index, confAMR_Q_index, confAMS_Q_index)] , function(x) factor(x, ordered = TRUE, levels = c("Not at all confident","Slightly confident", "Somewhat confident","Very confident")))

#code confidence levels as 0 to 3
before <- before %>% mutate(across(names(before)[c(confAM_Q_index, confAMR_Q_index, confAMS_Q_index)], ~recode(., "Not at all confident"=0, "Slightly confident"=1, "Somewhat confident"=2, "Very confident"=3)))
after <- after %>% mutate(across(names(after)[c(confAM_Q_index, confAMR_Q_index, confAMS_Q_index)], ~recode(., "Not at all confident"=0, "Slightly confident"=1, "Somewhat confident"=2, "Very confident"=3)))

#conf scores (median of each conf Q) overall and by section
before$Total_C <- rowMedians(as.matrix(before[,confQs]), na.rm=T)
after$Total_C <- rowMedians(as.matrix(after[,confQs]), na.rm=T)

before$AM_C <- rowMedians(as.matrix(before[,confAM_Q_index]), na.rm=T)
before$AMR_C <- rowMedians(as.matrix(before[,confAMR_Q_index]), na.rm=T)
before$AMS_C <- rowMedians(as.matrix(before[,confAMS_Q_index]), na.rm=T)

after$AM_C <- rowMedians(as.matrix(after[,confAM_Q_index]), na.rm=T)
after$AMR_C <- rowMedians(as.matrix(after[,confAMR_Q_index]), na.rm=T)
after$AMS_C <- rowMedians(as.matrix(after[,confAMS_Q_index]), na.rm=T)

#average confidence scores
sink("Results/results.txt", append=T)
cat("\n average confidence scores _ before \n")
sapply(before[,c("AM_C", "AMR_C", "AMS_C", "Total_C")], function(x) round(mean(x, na.rm=T),2)) #need na.rm here to get mean because not every student did each section
cat("\n average confidence scores _ after \n")
sapply(after[,c("AM_C", "AMR_C", "AMS_C", "Total_C")], function(x) round(mean(x, na.rm=T),2))


#median conf scores
cat("\n median confidence scores _ before \n")
sapply(before[,c("AM_C", "AMR_C", "AMS_C", "Total_C")], function (x) median(x, na.rm=T))
cat("\n median confidence scores _ after \n")
sapply(after[,c("AM_C", "AMR_C", "AMS_C", "Total_C")], function (x) median(x, na.rm=T))


#mode conf scores
cat("\n mode confidence scores _ before \n")
sapply(before[,c("AM_C", "AMR_C", "AMS_C", "Total_C")], function (x) table(x))
cat("\n mode confidence scores _ after \n")
sapply(after[,c("AM_C", "AMR_C", "AMS_C", "Total_C")], function (x) table(x))

cat("\n Comparing all students confidence scores (medians) before and after \n")
#compare total confidence, and by section
wilcox.test(before$Total_C, after$Total_C,paired=F,exact =T)
wilcox.test(before$AM_C, after$AM_C,paired=F,exact =T)
wilcox.test(before$AMR_C, after$AMR_C,paired=F,exact =T)
wilcox.test(before$AMS_C, after$AMS_C,paired=F,exact =T)
sink()

#specific questions with lowest and highest confidence levels across all students
#examine what percent of students were somewhat or very confident (score >=2) for each question (column)
sink("Results/results.txt", append=T)

cat("\n percent of students somewhat or very confident in each question  \n")
before_confQ_scores <- sapply(before[,confQs], function(x) round(mean(x>=2, na.rm=T),2))
after_confQ_scores <- sapply(after[,confQs], function(x) round(mean(x>=2, na.rm=T),2))
confQ_scores <- cbind(before_confQ_scores, after_confQ_scores)
colnames(confQ_scores) <- c("before", "after")
rownames(confQ_scores) <- str_extract(Qs[confQs], '-(.*)')
confQ_scores

sink()

####Knowledge####

#scoring correct answers, see Qualtrics scoring pdf
#AM Knowledge correct answers (Q6_1 to Q15)
AM_correct <- c("TRUE", "TRUE", "FALSE", #Q6
                "Inhibition of protein synthesis", "Inhibition of cell wall synthesis", "Inhibition of nucleic acid synthesis", #Q7
                "Inhibition of protein synthesis", "Inhibition of protein synthesis", "Inhibition of nucleic acid synthesis", #Q7
                "Inhibition of protein synthesis", "Inhibition of folic acid synthesis", "Inhibition of protein synthesis", #Q7
                "Bactericidal", "Bactericidal", "Bactericidal", "Bacteriostatic", "Bactericidal", "Bacteriostatic", "Bactericidal", "Bacteriostatic", #Q8
                "Amikacin", #Q9
                "Marbofloxacin", #Q10
                "Cephalexin", #Q11
                "Clindamycin", #Q12
                "Marbofloxacin, Ampicillin, Metronidazole", #Q13
                "IM florfenicol in a dairy calf with respiratory disease", #Q14
                "Oral amoxicillin-clavulanic acid in a dog with bacterial pyodermatitis") #Q15

#compare results to correct answers, make 1 for correct and 0 for incorrect
for (i in AM_Q_index){ #for each AM question
  after[,i] <- as.numeric(after[,i]==AM_correct[i-AM_Q_index[1]+1]) #AM_correct starts at index 1, i starts at AM_Q_index[1]
  before[,i] <- as.numeric(before[,i]==AM_correct[i-AM_Q_index[1]+1])
}

#AMR knowledge correct answers (Q18 - Q23)
AMR_correct <- c("Mutation of topoisomerase", #Q18; no Q19
                 "Intrinsic resistance, decreased permeability", #Q20
                 "Acquired resistance, target modification", #Q21
                 "Intrinsic resistance, drug degradation", #Q22
                 "TRUE", "TRUE", "FALSE", "TRUE", "TRUE", "TRUE", "FALSE", "TRUE", "FALSE") #Q23
  
#compare results to correct answers, make 1 for correct and 0 for incorrect
for (i in AMR_Q_index){ #for each AMR question
  after[,i] <- as.numeric(after[,i]==AMR_correct[i-AMR_Q_index[1]+1]) #AMR_correct starts at index 1, i starts at AMR_Q_index[1]
  before[,i] <- as.numeric(before[,i]==AMR_correct[i-AMR_Q_index[1]+1])
}

#AMS knowledge correct answers (Q31-32)
AMS_correct <- c("FALSE", "TRUE", "FALSE", "TRUE", "FALSE", "TRUE", "FALSE", "FALSE", "TRUE", "TRUE", #Q31
                 "Tylosin mixed in feed and fed to beef cattle") #Q32

#compare results to correct answers, make 1 for correct and 0 for incorrect
for (i in AMS_Q_index){ #for each AMS question
  after[,i] <- as.numeric(after[,i]==AMS_correct[i-AMS_Q_index[1]+1]) #AMS_correct starts at index 1, i starts at AMS_Q_index[1]
  before[,i] <- as.numeric(before[,i]==AMS_correct[i-AMS_Q_index[1]+1])
}

#clinical knowledge correct answers (Q34-Q42)
Clinical_correct <- c("Antimicrobials are not required at this time", #Q34
                      "10-day course of doxycycline", #Q35
                      "Topical chlorhexidine three times a week for two weeks", #Q36
                      "3-day course of amoxicillin", #Q37
                      "Stop the amoxicillin-clavulanic acid and start a 5-day course of trimethoprim-sulfamethoxazole", #Q38
                      "3-day course of extra-label ceftiofur hydrochloride, 14-day withdrawal period", #Q39
                      "Systemic antimicrobials are not required at this time", #Q40
                      "Culture all mastitis cows but wait until culture results are received to start treatment. Cattle with gram negative infections or no growth are not treated.", #Q41
                      "3-day course of IM penicillin, 6 day withhold") #Q42

#compare results to correct answers, make 1 for correct and 0 for incorrect
for (i in clinical_Q_index){ #for each AMS question
  after[,i] <- as.numeric(after[,i]==Clinical_correct[i-clinical_Q_index[1]+1]) #Clinical_correct starts at index 1, i starts at clinical_Q_index[1]
  before[,i] <- as.numeric(before[,i]==Clinical_correct[i-clinical_Q_index[1]+1])
}


#comparing to score column
score <- rowSums(before[,c(AM_Q_index, AMR_Q_index, AMS_Q_index, clinical_Q_index)], na.rm=T)
score
before$SC0
all.equal(score, as.numeric(before$SC0)) #Sc0 column in before has NA's where score would give 0

score <- rowSums(after[,c(AM_Q_index, AMR_Q_index, AMS_Q_index, clinical_Q_index)], na.rm=T)
score
after$SC0
all.equal(score, as.numeric(after$SC0)) #SC0 column in after is the same as score
rm(score)

#percent correct
#note that SC0 will have NA if no answers were given. rowMeans with na.rm=T will give NA if all answers are missing, otherwise will give % correct out of number answered
before$perc <- rowMeans(before[,c(AM_Q_index, AMR_Q_index, AMS_Q_index, clinical_Q_index)], na.rm=T)
after$perc <- rowMeans(after[,c(AM_Q_index, AMR_Q_index, AMS_Q_index, clinical_Q_index)], na.rm=T)

#complete rows only - completion indicated in the "finished" column with T/F
before_complete <- before[before$Finished==TRUE,]
after_complete <- after[after$Finished==TRUE,]

#summary of scores for complete and all responses
sink("Results/results.txt", append=T)

cat("\n summary of complete scores - before; N is last \n")
summary(before_complete$perc) %>% round(2)
length(before_complete$perc)

cat("\n summary of complete scores - after; N is last \n")
summary(after_complete$perc) %>% round(2)
length(after_complete$perc)

cat("\n summary of all scores - before; N is last \n")
summary(before$perc) %>% round(2)
length(before$perc)

cat("\n summary of all scores - after; N is last \n")
summary(after$perc) %>% round(2)
length(after$perc)

sink()

#complete rows for each section
#note that the scored questions do not include any "TEXT" questions that may be left blank in completed sections
names(Qs)[AM_Q_index]
names(Qs)[AMR_Q_index]
names(Qs)[AMS_Q_index]
names(Qs)[clinical_Q_index]

AM_n_missing_before <- rowSums(is.na(before[,AM_Q_index]))
AM_n_missing_before
AM_complete_row_index_before <- AM_n_missing_before==0

AMR_n_missing_before <- rowSums(is.na(before[,AMR_Q_index]))
AMR_n_missing_before
AMR_complete_row_index_before <- AMR_n_missing_before==0

AMS_n_missing_before <- rowSums(is.na(before[,AMS_Q_index]))
AMS_n_missing_before
AMS_complete_row_index_before <- AMS_n_missing_before==0

Clinical_n_missing_before <- rowSums(is.na(before[,clinical_Q_index]))
Clinical_n_missing_before
Clinical_complete_row_index_before <- Clinical_n_missing_before==0

AM_n_missing_after <- rowSums(is.na(after[,AM_Q_index]))
AM_n_missing_after
AM_complete_row_index_after <- AM_n_missing_after==0

AMR_n_missing_after <- rowSums(is.na(after[,AMR_Q_index]))
AMR_n_missing_after
AMR_complete_row_index_after <- AMR_n_missing_after==0

AMS_n_missing_after <- rowSums(is.na(after[,AMS_Q_index]))
AMS_n_missing_after
AMS_complete_row_index_after <- AMS_n_missing_after==0

Clinical_n_missing_after <- rowSums(is.na(after[,clinical_Q_index]))
Clinical_n_missing_after
Clinical_complete_row_index_after <- Clinical_n_missing_after==0
#although some earlier sections (AM, AMR) have more included responses if completeness is evaluated by section rather than the whole survey


#scores for each section
before$AM_score <- rowSums(before[, AM_Q_index], na.rm=T) ##note that rowSums with na.rm=T gives total correct. If none were answered, it gives 0.
before$AM_perc <- rowMeans(before[, AM_Q_index], na.rm=T) ##note that rowMeans with na.rm=T gives the total correct/total answered. If none were answered, it gives NA
after$AM_score <- rowSums(after[, AM_Q_index], na.rm=T)
after$AM_perc <- rowMeans(after[, AM_Q_index], na.rm=T)

before$AMR_score <- rowSums(before[, AMR_Q_index], na.rm=T)
before$AMR_perc <- rowMeans(before[, AMR_Q_index], na.rm=T) 
after$AMR_score <- rowSums(after[, AMR_Q_index], na.rm=T)
after$AMR_perc <- rowMeans(after[, AMR_Q_index], na.rm=T)

before$AMS_score <- rowSums(before[, AMS_Q_index], na.rm=T)
before$AMS_perc <- rowMeans(before[, AMS_Q_index], na.rm=T)
after$AMS_score <- rowSums(after[, AMS_Q_index], na.rm=T)
after$AMS_perc <- rowMeans(after[, AMS_Q_index], na.rm=T)

before$Clinical_score <- rowSums(before[, clinical_Q_index], na.rm=T)
before$Clinical_perc <- rowMeans(before[, clinical_Q_index], na.rm=T)
after$Clinical_score <- rowSums(after[, clinical_Q_index], na.rm=T)
after$Clinical_perc <- rowMeans(after[, clinical_Q_index], na.rm=T)

#summary of section scores
sink("Results/results.txt", append=TRUE)

cat("\n summary of AM scores, complete - before; N is last \n")
summary(before$AM_perc[AM_complete_row_index_before]) %>% round(2)
length(before$AM_perc[AM_complete_row_index_before])

cat("\n summary of AM scores, complete - after; N is last \n")
summary(after$AM_perc[AM_complete_row_index_after]) %>% round(2)
length(after$AM_perc[AM_complete_row_index_after])

cat("\n summary of AMR scores, complete - before; N is last \n")
summary(before$AMR_perc[AMR_complete_row_index_before]) %>% round(2)
length(before$AMR_perc[AMR_complete_row_index_before])

cat("\n summary of AMR scores, complete - after; N is last \n")
summary(after$AMR_perc[AMR_complete_row_index_after]) %>% round(2)
length(after$AMR_perc[AMR_complete_row_index_after])

cat("\n summary of AMS scores, complete - before; N is last \n")
summary(before$AMS_perc[AMS_complete_row_index_before]) %>% round(2)
length(before$AMS_perc[AMS_complete_row_index_before])

cat("\n summary of AMS scores, complete - after; N is last \n")
summary(after$AMS_perc[AMS_complete_row_index_after]) %>% round(2)
length(after$AMS_perc[AMS_complete_row_index_after])

cat("\n summary of Clinical scores, complete - before; N is last \n")
summary(before$Clinical_perc[Clinical_complete_row_index_before]) %>% round(2)
length(before$Clinical_perc[Clinical_complete_row_index_before])

cat("\n summary of Clinical scores, complete - after; N is last \n")
summary(after$Clinical_perc[Clinical_complete_row_index_after]) %>% round(2)
length(after$Clinical_perc[Clinical_complete_row_index_after])

cat("\n summary of AM scores, all - before; N is last \n")
summary(before$AM_perc) %>% round(2)
length(before$AM_perc)

cat("\n summary of AM scores, all - after; N is last \n")
summary(after$AM_perc) %>% round(2)
length(after$AM_perc)

cat("\n summary of AMR scores, all - before; N is last \n")
summary(before$AMR_perc) %>% round(2)
length(before$AMR_perc)

cat("\n summary of AMR scores, all - after; N is last \n")
summary(after$AMR_perc) %>% round(2)
length(after$AMR_perc)

cat("\n summary of AMS scores, all - before; N is last \n")
summary(before$AMS_perc) %>% round(2)
length(before$AMS_perc)

cat("\n summary of AMS scores, all - after; N is last \n")
summary(after$AMS_perc) %>% round(2)
length(after$AMS_perc)

cat("\n summary of Clinical scores, all - before; N is last \n")
summary(before$Clinical_perc) %>% round(2)
length(before$Clinical_perc)

cat("\n summary of Clinical scores, all - after; N is last \n")
summary(after$Clinical_perc) %>% round(2)
length(after$Clinical_perc)

cat("\n compare total knowledge before and after, and by section \n")
#compare total knowledge, and by section
wilcox.test(before$perc, after$perc,paired=F,exact =T)
wilcox.test(before$AM_perc, after$AM_perc,paired=F,exact =T)
wilcox.test(before$AMR_perc, after$AMR_perc,paired=F,exact =T)
wilcox.test(before$AMS_perc, after$AMS_perc,paired=F,exact =T)
wilcox.test(before$Clinical_perc, after$Clinical_perc,paired=F,exact =T)
wilcox.test(before_complete$perc, after_complete$perc,paired=F,exact =T)
wilcox.test(before$AM_perc[AM_complete_row_index_before], after$AM_perc[AM_complete_row_index_after],paired=F,exact =T)
wilcox.test(before$AMR_perc[AMR_complete_row_index_before], after$AMR_perc[AMR_complete_row_index_after],paired=F,exact =T)
wilcox.test(before$AMS_perc[AMS_complete_row_index_before], after$AMS_perc[AMS_complete_row_index_after],paired=F,exact =T)
wilcox.test(before$Clinical_perc[Clinical_complete_row_index_before], after$Clinical_perc[Clinical_complete_row_index_after],paired=F,exact =T)
sink()

#alternative method for all responses summary (won't work for complete unless looking at overall complete and not by section)
sapply(before[,c("AM_perc", "AMR_perc", "AMS_perc", "Clinical_perc")], function(x) list(min(x, na.rm=T), max(x, na.rm=T), mean(x, na.rm=T), median(x, na.rm=T)))
sapply(after[after$Finished==TRUE,c("AM_perc", "AMR_perc", "AMS_perc", "Clinical_perc")], function(x) list(mean(x, na.rm=T), std.error(x)))


#### AMR guideline ####
Qs$Q27 #yes/no aware
Qs$Q28 #question 28 - guidelines aware of
Qs$Q29 #question 29 - guidelines read

sink("Results/results.txt", append=TRUE)
#split strings at commas to extract each guideline
## before aware
cat("\n before - % aware of at least one guideline \n")
before.aware.n <- sum(before$Q27=="Yes", na.rm=T)
mean(before$Q27=="Yes", na.rm=T) %>% round(2)

guidelines_before_aware <- unlist(strsplit(before$Q28, ","))
before.aware <- as.data.frame(table (guidelines_before_aware))
colnames(before.aware) <- c("guidelines", "before.aware.Freq")

cat("\n before - average # guidelines aware of \n")
(sum(guidelines_before_aware!="Not sure", na.rm=T)/sum(!is.na(before$Q28))) %>% round(1) #guidelines mentioned per person who replied

## before read
guidelines_before_read <- unlist(strsplit(before$Q29, ","))

before.read <- as.data.frame(table (guidelines_before_read))
colnames(before.read) <- c("guidelines", "before.read.Freq")

cat("\n before - average # guidelines read \n")
(sum(guidelines_before_read!="Not sure" & guidelines_before_read!="None of these", na.rm=T)/sum(!is.na(before$Q29))) %>% round(1) #guidelines mentioned per person who replied

## after aware
cat("\n after - % aware of at least one guideline \n")
after.aware.n <- sum(after$Q27=="Yes", na.rm=T) %>% round(2)
mean(after$Q27=="Yes", na.rm=T) %>% round(2)

guidelines_after_aware <- unlist(strsplit(after$Q28, ","))
after.aware <- as.data.frame(table(guidelines_after_aware))
colnames(after.aware) <- c("guidelines", "after.aware.Freq")

cat("\n after - average # guidelines aware of \n")
(sum(guidelines_after_aware!="Not sure", na.rm=T)/sum(!is.na(after$Q28))) %>% round(1) #guidelines mentioned per person who replied

## after read
guidelines_after_read <- unlist(strsplit(after$Q29, ","))
after.read <- as.data.frame(table (guidelines_after_read))
colnames(after.read) <- c("guidelines", "after.read.Freq")
cat("\n after - average # guidelines read \n")
(sum(guidelines_after_read!="Not sure" & guidelines_after_read!="None of these", na.rm=T)/sum(!is.na(after$Q29))) %>% round(1) #guidelines mentioned per person who replied

cat("\n guideline table with percentages (out of n students aware of at least one guideline) \n")
guideline.table <- full_join(before.aware, after.aware, by="guidelines") %>% full_join(before.read, by="guidelines") %>% full_join(after.read, by="guidelines")
guideline.table$before.aware.perc <- round(guideline.table$before.aware.Freq/before.aware.n, 2)
guideline.table$after.aware.perc <- round(guideline.table$after.aware.Freq/after.aware.n, 2)
guideline.table$before.read.perc <- round(guideline.table$before.read.Freq/before.aware.n, 2)
guideline.table$after.read.perc <- round(guideline.table$after.read.Freq/after.aware.n, 2)
guideline.table
sink()

####paired data####
#join
paired <- inner_join(before, after, by="Q2", suffix=c("_before", "_after"))
#note: there are two ID==22 in the before group and only one in after, they cannot be distinguished. exclude from paired analysis
#drop 22
paired <- paired %>% filter(Q2!="22")
#include only those that completed both before and after
paired <- paired %>% filter(Finished_before=="TRUE" & Finished_after=="TRUE")

#descriptive of paired students
sink("Results/results.txt", append=T)

cat("\n number of students completing both surveys \n")
nrow(paired)

#compare total confidence, and by section
cat("\n Comparing paired students confidence scores (medians) before and after \n")
wilcox.test(paired$Total_C_before,paired$Total_C_after,paired=T,exact =T)
wilcox.test(paired$AM_C_before, paired$AM_C_after,paired=T,exact =T)
wilcox.test(paired$AMR_C_before, paired$AMR_C_after,paired=T,exact =T)
wilcox.test(paired$AMS_C_before, paired$AMS_C_after,paired=T,exact =T)

cat("\n compare paired total knowledge before and after, and by section \n")
#compare total knowledge, and by section
wilcox.test(paired$perc_before, paired$perc_after, paired=T,exact =T)
wilcox.test(paired$AM_perc_before, paired$AM_perc_after,paired=F,exact =T)
wilcox.test(paired$AMR_perc_before, paired$AMR_perc_after,paired=F,exact =T)
wilcox.test(paired$AMS_perc_before, paired$AMS_perc_after,paired=F,exact =T)
wilcox.test(paired$Clinical_perc_before, paired$Clinical_perc_after,paired=F,exact =T)
sink()



####AMR_Perception####
Qs[perception_Q_index]
before[,perception_Q_index] <- lapply(before[,perception_Q_index], as.numeric)
after[,perception_Q_index] <- lapply(after[,perception_Q_index], as.numeric)

#create dataframe for plotting
perception <- rbind(cbind(before[,perception_Q_index], survey=rep("pre-clinical", nrow(before))),
                    cbind(after[,perception_Q_index], survey=rep("post-clinical", nrow(after))))

perception <- pivot_longer(perception, !survey, names_to="Q", values_to="value")
perception$Q <- case_when(
  perception$Q=="Q30_1" ~ "Companion animal vets",
  perception$Q=="Q30_2" ~ "Food animal vets",
  perception$Q=="Q30_4" ~ "Human healthcare providers",
  perception$Q=="Q30_6" ~ "Other"
)

#set factor order for bar order
perception$survey <- as.factor(perception$survey)
perception$survey <- relevel(perception$survey, "pre-clinical")
perception$Q <- as.factor(perception$Q)
levels(perception$Q) <- c("Companion animal\nvets", "Food animal\nvets", "Human healthcare\nproviders", "Other")

ggplot(perception, aes(fill=survey, y=value/100, x=Q))+ #need to divide by 100 and then use scale=percent to get right format
  stat_summary(fun="mean", geom="bar", position="dodge")+
  stat_summary(fun.data=mean_se, geom="errorbar", position="dodge")+ #error bar is +/- 1 SE
  scale_fill_grey(start=0.4, end=0.8)+
  scale_y_continuous(labels=percent)+
  ylab("Average Percent of\nResponsibility for AMR")+
  xlab("")+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(), legend.position = "bottom", legend.title = element_blank() )
ggsave("Figs/Fig 2_AMR attribution.png", dpi=300, width=6, height=4, units="in")

#attribution to vets vs humans
sink("Results/results.txt", append=T)

cat("\n number of students attributing more to SA vets over FA vets - before \n")
round(mean(before$Q30_1>=before$Q30_2, na.rm=T),2)
cat("\n number of students attributing more to SA vets over FA vets - after \n")
round(mean(after$Q30_1>=after$Q30_2, na.rm=T),2)

cat("\n number of students attributing more to vets over human healthcare - before \n")
round(mean((before$Q30_1+before$Q30_2)>=before$Q30_4, na.rm=T),2)
cat("\n number of students attributing more to vets over human healthcare - after \n")
round(mean((after$Q30_1+after$Q30_2)>=after$Q30_4, na.rm=T),2)

sink()

####Compare Conf and Knowledge####
conf.know.before <- before[,c("Total_C", "AM_C", "AMR_C", "AMS_C", "perc", "AM_perc", "AMR_perc", "AMS_perc", "Clinical_perc")]
conf.know.after <- after[,c("Total_C", "AM_C", "AMR_C", "AMS_C", "perc", "AM_perc", "AMR_perc", "AMS_perc", "Clinical_perc")]
conf.know.before$survey <- "pre-clinical"
conf.know.after$survey <- "post-clinical"

conf.know <- rbind(conf.know.before, conf.know.after)
conf.know$survey <- as.factor(conf.know$survey)
conf.know$survey <- relevel(conf.know$survey, "pre-clinical")


overall.conf.know <- ggplot(conf.know, aes(x=Total_C, y=perc))+
         geom_point(shape=16)+
  facet_grid(survey~.)+
  xlab("Overall Confidence Score")+
  ylab("Overall Knowledge Score")+
  theme_bw()

AM.conf.know <- ggplot(conf.know, aes(x=AM_C, y=AM_perc))+
  geom_point(shape=16)+
  facet_grid(survey~.)+
  xlab("PK & PD Confidence Score")+
  ylab("PK & PD Knowledge Score")+
  theme_bw()

AMR.conf.know <- ggplot(conf.know, aes(x=AMR_C, y=AMR_perc))+
  geom_point(shape=16)+
  facet_grid(survey~.)+
  xlab("AMR Confidence Score")+
  ylab("AMR Knowledge Score")+
  theme_bw()

AMS.conf.know <- ggplot(conf.know, aes(x=AMS_C, y=AMS_perc))+
  geom_point(shape=16)+
  facet_grid(survey~.)+
  xlab("AMS Confidence Score")+
  ylab("AMS Knowledge Score")+
  theme_bw()

Clinical.conf.know <- ggplot(conf.know, aes(x=AMS_C, y=Clinical_perc))+
  geom_point(shape=16)+
  facet_grid(survey~.)+
  xlab("AMS Confidence Score")+
  ylab("Clinical Knowledge Score")+
  theme_bw()

section.conf.know <- grid.arrange(AM.conf.know, AMR.conf.know, AMS.conf.know, Clinical.conf.know, nrow=2)
ggsave("Figs/Sup Fig 1_section conf know.png", plot=section.conf.know, dpi=300, height=6, width=8, units="in")
ggsave("Figs/Fig 1_overall conf know.png", plot=overall.conf.know, dpi=300, height=3, width=4, units="in")

#correlation tests
sink("Results/results.txt", append=T)
cat("\n correlation between confidence and knowledge \n")
cor.test(conf.know.after$Total_C, conf.know.after$perc, method=c("kendall"), use="complete")
cor.test(conf.know.after$AM_C, conf.know.after$AM_perc, method=c("kendall"), use="complete")
cor.test(conf.know.after$AMR_C, conf.know.after$AMR_perc, method=c("kendall"), use="complete")
cor.test(conf.know.after$AMS_C, conf.know.after$AMS_perc, method=c("kendall"), use="complete")
cor.test(conf.know.after$AMS_C, conf.know.after$Clinical_perc, method=c("kendall"), use="complete")

cor.test(conf.know.before$Total_C, conf.know.before$perc, method=c("kendall"), use="complete")
cor.test(conf.know.before$AM_C, conf.know.before$AM_perc, method=c("kendall"), use="complete")
cor.test(conf.know.before$AMR_C, conf.know.before$AMR_perc, method=c("kendall"), use="complete")
cor.test(conf.know.before$AMS_C, conf.know.before$AMS_perc, method=c("kendall"), use="complete")
cor.test(conf.know.before$AMS_C, conf.know.before$Clinical_perc, method=c("kendall"), use="complete")
sink()
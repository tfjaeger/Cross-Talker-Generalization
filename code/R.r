library(lme4)
library(readxl)
data <- read_excel("./data/test.xlsx")
model <- glmer(cbind(IsCorrect, PropKeywordsCorrect) ~ 1 + (1 | Filename) + (1 | SentenceID), data = data, family = binomial)
summary(model)



library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/data/test.xlsx")
model <- glmer(cbind(NumKeywordsCorrect, NumKeywords-NumKeywordsCorrect) ~ 1 + PropKeywordsCorrect + (1 | WorkerID) + (1 | SentenceID), data = data, family = binomial)
summary(model)

getwd()

library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/Sep23/similarity_measure.xlsx")
model <- glmer(cbind(Num_diphone_intersect, Num_diphone-Num_diphone_intersect) ~ 1 + Similarity_mean + (1 | WorkerID) + (1 | SentenceID), data = data, family = binomial)
summary(model)


library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/data/test.xlsx")
model_human_interaction <- glmer(cbind(NumKeywordsCorrect, NumKeywords - NumKeywordsCorrect) ~ 
                                 1 + PropKeywordsCorrect * Cond.treat + (1 | WorkerID) + (1 | SentenceID)+ (1 | Keywords), 
                                 data = data,
                                 family = binomial)
summary(model_human_interaction)


data <- read_excel("./cross-talker-ASR/Sep23/similarity_measure.xlsx")
#data$Condition <- relevel(data$Condition, ref = "multi")
model_simulation_interaction <- glmer(cbind(Num_diphone_intersect, Num_diphone - Num_diphone_intersect) ~ 
                                      1 + Similarity_mean * Condition + (1 | WorkerID) + (1 | SentenceID) , 
                                      data = data,
                                      family = binomial)

summary(model_simulation_interaction)


library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/Oct/similarities1.xlsx")
#data$Condition <- relevel(data$Condition, ref = "multi")
model_simulation_interaction <- glmer(cbind(NumKeywordsCorrect, Num_keyword - NumKeywordsCorrect) ~ 
                                      1 + Similarity_mean * Condition + (1 | WorkerID) + (1 | SentenceID), 
                                      data = data,
                                      family = binomial)
summary(model_simulation_interaction)

library(lme4)
library(readxl)
data1 <- read_excel("./cross-talker-ASR/Oct/similarities2.xlsx")
model_simulation_interaction_ <- glmer(cbind(NumKeywordsCorrect, Num_diphone - NumKeywordsCorrect) ~ 
                                      1 +  Similarity_max  + (1 | WorkerID) + (1 | SentenceID) + (1 | keyword_label), 
                                      data = data1,
                                      family = binomial)
summary(model_simulation_interaction_)





#Oct7
library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/Oct7/similarities.xlsx")
model_simulation_interaction <- glmer(cbind(NumKeywordsCorrect, Num_keyword - NumKeywordsCorrect) ~ 
                                      1 + Similarity_mean * Condition + (1 | WorkerID) + (1 | SentenceID), 
                                      data = data,
                                      family = binomial)
summary(model_simulation_interaction)




#Human
library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/data/test.xlsx")
model <- glmer(cbind(NumKeywordsCorrect, NumKeywords-NumKeywordsCorrect) ~ 1 + IsCorrect + (1 | WorkerID) + (1 | SentenceID), data = data, family = binomial)
summary(model)

##########################################################################
library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/Oct7/similarities1.xlsx")
model <-glmer(IsCorrect ~ sim_mean_max + (1|WorkerID) + (1|SentenceID), data = data, family=binomial)
summary(model)


library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/Oct7/similarities1.xlsx")
data$Condition2 <- factor(data$Condition2)
contrasts(data$Condition2) = contr.sum(4)
model <-glmer(IsCorrect ~ sim_mean_max* Condition2 + (1|WorkerID) + (1|SentenceID), data = data, family=binomial)
summary(model)

library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/Oct7/similarities1.xlsx")
model <-glmer(IsCorrect ~ sim_mean_mean + (1|WorkerID) + (1|SentenceID), data = data, family=binomial)
summary(model)

library(lme4)
library(readxl)
data <- read_excel("./cross-talker-ASR/Oct7/similarities1.xlsx")
model <-glmer(IsCorrect ~  sim_mean_max + sim_mean_std + (1|WorkerID) + (1|SentenceID), data = data, family=binomial)
summary(model)

filtered_df <- subset(data, Condition2 == "Talker-specific")
model <-glmer(IsCorrect ~  sim_mean_max  + (1|WorkerID) + (1|SentenceID), data = filtered_df, family=binomial)
summary(model)


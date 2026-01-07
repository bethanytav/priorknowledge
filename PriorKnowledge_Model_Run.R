library(dplyr)
library(lme4)
library(lmerTest)
library(sjPlot)

df_long <- read.csv("/Downloads/PriorKnowledge_Data.csv")

## outcomes:
## 1 - TOT
## 2 - Motiv_4_Task
## 3 - Motiv_4_Future
## 4 - Effort_Perceived

####################################################################
# 1 - Model for Effort - Actual ($TOT)
model1 <- lmer(
  Time ~ XP_CODING*TASK_ORDER + Task*TASK_ORDER + SPSS_Time_sec + Major + Grade + (1 | SUBJ_ID),
  data = df_long
)
summary(model1)

####################################################################
# 2 - Model for Motivation - Task
model2 <- lmer(
  Motiv_4_Task ~ XP_CODING*TASK_ORDER + Task*TASK_ORDER + SPSS_Time_sec + Major + Grade + (1 | SUBJ_ID),
  data = df_long
)
summary(model2)

####################################################################
# 3 - Model for Motivation - Future Learning
model3 <- lmer(
  Motiv_4_Future ~ XP_CODING*TASK_ORDER + Task*TASK_ORDER + Motiv_Future_Pre + SPSS_Time_sec + Major + Grade + (1 | SUBJ_ID),
  data = df_long
)
summary(model3)

####################################################################
# 4 - Model for Effort - Perceived
model4 <- lmer(
  Effort_Perceived ~ XP_CODING*TASK_ORDER + Task*TASK_ORDER + SPSS_Time_sec + Major + Grade + (1 | SUBJ_ID),
  data = df_long
)
summary(model4)

## table of all the models
tab_model(list(model1, model2, model3, model4))

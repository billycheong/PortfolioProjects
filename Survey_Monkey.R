library(readxl)
library(tidyverse)
library(data.table)
library(writexl)

# import data
Data_Survey_Monkey_Output_Edited <- read_excel("Data - Survey Monkey Output Edited.xlsx", 
                                               sheet = "Edited_Data")
# make a copy of the original data
df1 <- Data_Survey_Monkey_Output_Edited

# remove column 2:7
df2 <- Data_Survey_Monkey_Output_Edited[-c(2:7)]

# transform data from wide to long data
df3 <- df2 %>% 
  gather(key = "Question + Sub Question",
         value = "Answer",
         c(-1:-8))

# import question sheet
question_import <- read_excel("Data - Survey Monkey Output Edited.xlsx", 
                                               sheet = "Question_R")
# make a copy of question
question_import2 <- data.frame(question_import)

# left join df3 and question_import2
df4 <- merge(x = df3, y = question_import2,
             by.x = "Question + Sub Question", by.y = "Question...Subquestion", all.x = TRUE)

# remove na answer row 
df5 <- df4[! is.na(df4$Answer),]

# find distinct number of answer to each question 
distinct_answer <- dt %>% group_by(Question) %>% summarise(distinct_answer = n_distinct(`Respondent ID`))
                                                
# merge distinct answer into main data
df6 <- merge(df5, distinct_answer,
             by.x = "Question" , by.y = "Question", all.x = TRUE)

# number of respond to each respond
distinct_respond <- df6 %>% group_by(`Question + Sub Question` , Answer) %>% summarise(distinct_respond = n_distinct(`Respondent ID`))

# merge distinct respond to main data
df7 <- merge(df6, distinct_respond,
             by = c("Question + Sub Question" , "Answer"))

# rename column
colnames(df7)[colnames(df7) == "Identify which division you work in. - Response" ] <- "Division"

colnames(df7)[colnames(df7) == "Identify which division you work in. - Other (please specify)" ] <- "Division Other"

colnames(df7)[colnames(df7) == "Which of the following best describes your position level? - Response" ] <- "Position"

colnames(df7)[colnames(df7) == "Which generation are you apart of? - Response" ] <- "Generation"

colnames(df7)[colnames(df7) == "Please select the gender in which you identify. - Response"  ] <- "Gender"

colnames(df7)[colnames(df7) == "Which duration range best aligns with your tenure at your company? - Response" ] <- "Tenure"

colnames(df7)[colnames(df7) == "Which of the following best describes your employment type? - Response" ] <- "Employment Type"

colnames(df7)[colnames(df7) == "distinct_answer" ] <- "Respondents"

colnames(df7)[colnames(df7) == "distinct_respond" ] <- "Same Answer"


# export as excel file
write_xlsx(df7,"/home/yeti/Documents/Survey_Monkey_Final_Output.xlsx")


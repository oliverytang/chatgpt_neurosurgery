rm(list=ls())

library(irr)

# Input spreadsheet (named core) should be .csv file with 6 columns: Correct_GPT (binary: 0 for incorrect answer vs. 1 for correct answer by ChatGPT), Correct_4 (binary: 0 for incorrect answer vs. 1 for correct answer by GPT-4), Correct_4_Replicate (binary: 0 for incorrect answer vs. 1 for correct answer by GPT-4), High_Order (binary: 0 for first-order question vs. 1 for higher-order question), Words (numeric: total number of words in question), Imaging (binary: 0 for no imaging in question stem vs. 1 for imaging in question stem)
# Each row should correspond with a test question

# Compare performance between human users, ChatGPT, and GPT-4
length = 500 # User-defined total length of dataset
user = 368.6431 # User-defined average number of questions answered correctly by user
prop.test(x=c(user, sum(core$Correct_GPT)), n=c(length, length))
prop.test(x=c(user, sum(core$correct_4)), n=c(length, length))
prop.test(x=c(sum(core$Correct_GPT), sum(core$correct_4)), n=c(length, length))

# Compare word count between correct and incorrect answers
t.test(Words~Correct_GPT, data=core)
t.test(Words~Correct_4, data=core)

# Analyze association between question word count and LLM choosing the correct answer
summary(glm(Correct_GPT~Words, data=core, family="binomial"))
summary(glm(Correct_4~Words, data=core, family="binomial"))

# Analyze association between higher-order question status and LLM choosing the correct answer
summary(glm(Correct_GPT~High_Order, data=core, family="binomial"))
summary(glm(Correct_4~High_Order, data=core, family="binomial"))

# Analyze differences in performance between ChatGPT and GPT-4 by question higher-order status
matrix = matrix(c(table(core$Correct_GPT[which(core$Order==0)]), table(core$Correct_4[which(core$Order==0)])), ncol=2, nrow=2)
chisq.test(matrix)
matrix = matrix(c(table(core$Correct_GPT[which(core$Order==1)]), table(core$Correct_4[which(core$Order==1)])), ncol=2, nrow=2)
chisq.test(matrix)

# Analyze association between question word count and LLM choosing the correct answer
summary(glm(Correct_GPT~Imaging, data=core, family="binomial"))
summary(glm(Correct_4~Imaging, data=core, family="binomial"))

# Analyze differences in performance between ChatGPT and GPT-4 on imaging questions
matrix = matrix(c(table(core$Correct_GPT[which(core$Imaging==1)]), table(core$Correct_4[which(core$Imaging==1)])), ncol=2, nrow=2)
chisq.test(matrix)

# Analyze differences in performance and concordance between two separate trials of GPT-4
prop.test(c(sum(core$Correct_4), sum(core$Correct_4_Replicate)), c(length, length))
kappa2(core[, c("Correct_4", "Correct_4_Replicate")])
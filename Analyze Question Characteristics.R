rm(list=ls())

# Input spreadsheet should be .csv file with 3 columns: Correct (binary: 0 for incorrect answer vs. 1 for correct answer), High_Order (binary: 0 for first-order question vs. 1 for higher-order question), Words (numeric: total number of words in question)
# Each row should correspond with a test question

# Compare word count between correct and incorrect answers
t.test(Words~Correct, data=core)

# Analyze association between question word count and ChatGPT choosing the correct answer
summary(glm(Correct~Words, data=core, family="binomial"))

# Analyze association between higher-order question status and ChatGPT choosing the correct answer
summary(glm(Correct~High_Order, data=core, family="binomial"))
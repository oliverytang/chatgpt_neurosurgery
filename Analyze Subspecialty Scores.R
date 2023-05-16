rm(list=ls())

# Input spreadsheet should be .csv file with 5 columns: Category (string: name of question subspecialty), Correct_GPT (numeric: total number of correct answers by ChatGPT in subspecialty), Correct_4 (numeric: total number of correct answers by GPT-4 in subspecialty), Correct_Human_Percent (numeric: percentage correct for each score, ranging from 0 to 100), Total (numeric: total number of questions in subspecialty)
# First row should be overall score on exam

core = read.csv("Analyze Subspecialty Scores.csv") # Change file path to your spreadsheet

# Generate number of questions answered correctly by human users
core$Correct_Human = core$Total * core$Correct_Human_Percent

# Calculate percentage correctand 95% confidence interval for each score
core$Percentage = core$Correct_Human / core$Total * 100
prop.test(core[1, "Correct_Human"], core[1, "Total"])
core$Percentage = core$Correct_GPT / core$Total * 100
prop.test(core[1, "Correct_GPT"], core[1, "Total"])
core$Percentage = core$Correct_4 / core$Total * 100
prop.test(core[1, "Correct_4"], core[1, "Total"])

# Calculate differences between humans, ChatGPT, and GPT-4 for each subspecialty
for (row in 2:nrow(core)) {
  print(core$Category[row])
  data = c(core$Correct_Human[row], core$Total[row] - core$Correct_Human[row], core$Correct_GPT[row], core$Total[row] - core$Correct_GPT[row])
  test = matrix(data, 2, 2)
  if (core[row, "Total"] < 20) {
    print(fisher.test(test))
  } else {
    print(chisq.test(test))
  }
  data = c(core$Correct_Human[row], core$Total[row] - core$Correct_Human[row], core$Correct_4[row], core$Total[row] - core$Correct_4[row])
  test = matrix(data, 2, 2)
  if (core[row, "Total"] < 20) {
    print(fisher.test(test))
  } else {
    print(chisq.test(test))
  }
  data = c(core$Correct_GPT[row], core$Total[row] - core$Correct_GPT[row], core$Correct_4[row], core$Total[row] - core$Correct_4[row])
  test = matrix(data, 2, 2)
  if (core[row, "Total"] < 20) {
    print(fisher.test(test))
  } else {
    print(chisq.test(test))
  }
}

# Calculate whether performance on subspecialty for humans is significantly different from performance on rest of exam
for (row in 2:nrow(core)) {
  print(core$Category[row])
  data = c(core$Correct_Human[row], core$Total[row] - core$Correct_Human[row], sum(core$Correct_Human[-row]), sum(core$Total[-row]) - sum(core$Correct_Human[-row]))
  test = matrix(data, 2, 2)
  if (core[row, "Total"] < 20) {
    print(fisher.test(test))
  } else {
    print(chisq.test(test))
  }
}

# Calculate whether performance on subspecialty for ChatGPT is significantly different from performance on rest of exam
for (row in 2:nrow(core)) {
  print(core$Category[row])
  data = c(core$Correct_GPT[row], core$Total[row] - core$Correct_GPT[row], sum(core$Correct_GPT[-row]), sum(core$Total[-row]) - sum(core$Correct_GPT[-row]))
  test = matrix(data, 2, 2)
  if (core[row, "Total"] < 20) {
    print(fisher.test(test))
  } else {
    print(chisq.test(test))
  }
}

# Calculate whether performance on subspecialty for GPT-4 is significantly different from performance on rest of exam
for (row in 2:nrow(core)) {
  print(core$Category[row])
  data = c(core$Correct_4[row], core$Total[row] - core$Correct_4[row], sum(core$Correct_4[-row]), sum(core$Total[-row]) - sum(core$Correct_4[-row]))
  test = matrix(data, 2, 2)
  if (core[row, "Total"] < 20) {
    print(fisher.test(test))
  } else {
    print(chisq.test(test))
  }
}
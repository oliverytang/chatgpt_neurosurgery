rm(list=ls())

# Input spreadsheet should be .csv file with 3 columns: Category (string: name of question subspecialty), Correct (numeric: total number of correct questions in subspecialty), Total (numeric: total number of questions in subspecialty)
# First row should be overall score on exam

core = read.csv("Analyze Subspecialty Scores.csv") # Change file path to your spreadsheet

# Calculate percentage correct for each score
core$Percentage = core$Correct / core$Total * 100

# Calculate 95% confidence interval for overall score
prop.test(core[1, "Correct"], core[1, "Total"])

# Calculate whether performance on subspecialty is significantly different from performance on rest of exam
for (row in 2:nrow(core)) {
  print(core$Category[row])
  data = c(core$Correct[row], core$Total[row] - core$Correct[row], sum(core$Correct[-row]), sum(core$Total[-row]) - sum(core$Correct[-row]))
  test = matrix(data, 2, 2)
  if (core[row, "Total"] < 20) {
    print(fisher.test(test))
  } else {
    print(chisq.test(test))
  }
}
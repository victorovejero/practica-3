data.raw <- read.csv("qsar_fish_toxicity.csv",sep=";",
                     stringsAsFactors = TRUE,
                     check.names = TRUE, na.strings = c("", "NA"), header=TRUE)
library(e1071) # SVM



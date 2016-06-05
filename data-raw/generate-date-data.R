# Data Structures ---------------------------------------------------------

vars = c("date",
         "b1",
         "b2",
         "b3",
         "b4")

numRows = 1000
numBehaviors = 4

Behaviors = paste0("b", 1:numBehaviors)

mat = matrix(nrow = 1000, ncol = (numBehaviors + 1))
tab = setNames(data.frame(mat), c("date", Behaviors))

startDate = as.Date("2016-04-01")
nDays = 1000

tab[, "date"] = seq(startDate, by="day", length.out = nDays)

for(i in Behaviors) {
  tab[, i] = sample(c(0, 1), 1000, replace = T)
}

FPdate = tab

use_data(FPdate)

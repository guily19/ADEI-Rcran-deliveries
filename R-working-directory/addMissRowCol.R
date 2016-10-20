
numberNArow <- function(dataFrame, output) {
  output = sum(is.na(dataFrame))
}

missXrow = apply(base, 1, numberNArow, output = 'outputfile')
missXcol = apply(base, 2, numberNArow, output = 'outputfile')

sum(missXcol)
sum(missXrow)

rbind(cresco,missXcol)
cresco$missValues <- missXrow
base$missValues



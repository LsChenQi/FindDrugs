library(rvest)

#----read drug names from HTML---

# set the working directory to use relative path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Read the raw data
nameIndex = LETTERS
drugs = c()
for (i in nameIndex){
  rawHtml = readLines(sprintf('../Data/Drug Names Beginning with %s.html', i), warn = FALSE)
  drugs = c(drugs, rawHtml[grep("(^\t<a data-toggle=\"collapse\")(.*)(</a>$)", rawHtml)])
}
# Find the lines of drugs
drugs = rawHtml[grep("(^\t<a data-toggle=\"collapse\")(.*)(</a>$)", rawHtml)]
# Extract the name of each drug
exp = "(.*)drugName(\\d+)(\">)|</a>"
drugsName = gsub(exp, "", drugs)
# Extract the active ingredients of each drug, the ingredient appears 5 lines after the lines of each drug
ingredientLines = rawHtml[grep("(^\t<a data-toggle=\"collapse\")(.*)(</a>$)", rawHtml) + 5]
exp = ".*\\((.*)\\).*"
ingredients = gsub(exp, "\\1", ingredientLines)
#--------------------------------
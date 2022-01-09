library(rvest)

#----read drug names from HTML---

# set the working directory to use relative path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Read the raw data
nameIndex = LETTERS
drugsName = c()
ingredients = c()
# go through all the files
for (i in nameIndex){
  rawHtml = readLines(sprintf('../Data/Drug Names Beginning with %s.html', i), warn = FALSE)
  drugs = rawHtml[grep("(^\t<a data-toggle=\"collapse\")(.*)(</a>$)", rawHtml)]
  # Extract the name of each drug
  exp = "(.*)drugName(\\d+)(\">)|</a>"
  drugsName = c(drugsName, gsub(exp, "", drugs))
  # Extract the active ingredients of each drug, the ingredient appears 5 lines after the lines of each drug
  ingredientLines = rawHtml[grep("(^\t<a data-toggle=\"collapse\")(.*)(</a>$)", rawHtml) + 5]
  exp = ".*\\((.*)\\).*"
  ingredients = c(ingredients, gsub(exp, "\\1", ingredientLines))
}
#---------------------------------

# According to Wikipedia, the symptoms of African swine fever include 
ASFSymptoms = list(fever = c('fever', 'high temperature'), vomit = c('vomit'), arthritis = c('arthritis'))
SymptomsFlags = matrix(FALSE, ncol = length(ASFSymptoms), nrow = length(drugsName))

#--find functions of each drug------
webDomain = 'https://www.webmd.com/'
for (drugIndex in 1 : length(drugsName)){
  drug = drugsName[drugIndex]
  # replace the space
  drug = gsub(" ", "%20", drug)
  url = sprintf('%sdrugs/2/search?type=drugs&query=%s', webDomain, drug) 
  tryCatch(
    expr = {
      webpage = read_html(url)
    },
    error = function(e){ 
      print(sprintf("%s cannot be read", drug))
      webpage = ""
    }
  )
  exactMatch = webpage %>%
    html_nodes('.drugs-exact-search-list .common-drug-name') %>% 
    html_attr("href")
  
  if (length(exactMatch) > 0){
    # use the first item under exactMatch
    drugDescriptionUrl = sprintf('%s%s', webDomain, exactMatch[1])
    webpage = read_html(drugDescriptionUrl)
    uses = webpage %>%
      html_nodes('.uses-container .monograph-content') %>% 
      html_text("href")
    # go through all symptoms
    for (symptomIndex in 1:length(ASFSymptoms)) {
      # each symptom might use different words
      for (word in ASFSymptoms[[symptomIndex]]){
         if(any(grepl(word, uses)) == TRUE){
           SymptomsFlags[drugIndex,symptomIndex] = TRUE
           break
         }
      }
    }
  }
}
#------------------------------------

# ----Export result------------
#create data frame
df = data.frame(drugName = drugsName,
                activeIngredients = ingredients,
                fever = SymptomsFlags[,1],
                vomit = SymptomsFlags[,2],
                arthritis = SymptomsFlags[,3])
write.csv(df, "../Result/result.csv", row.names = FALSE)

# read result
result = read.csv(file = '../Result/result.csv')
#------------------------------

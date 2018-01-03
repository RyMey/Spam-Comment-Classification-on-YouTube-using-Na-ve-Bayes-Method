## Kelompok 1
## KLASIFIKASI KOMENTAR SPAM PADA SITUS YOUTUBE DENGAN METODE NAÏVE BAYES
## 245 spam (1)
## 204 bukan spam (0)

######################## 1. Prarposes
# baca data
file <- file.choose()
data <- read.csv(file,header = TRUE, sep = ",")
# delete date
data <- subset(data, select = -c(DATE))

# lower case
data$AUTHOR <- tolower(data$AUTHOR)
data$CONTENT <- tolower(data$CONTENT)
# paste ke 1 kolom (isi)
data$COMMENT <- paste(data$AUTHOR,data$CONTENT) 
# remove punctuation
require(tm)
library(tm)
data$COMMENT <- removePunctuation(data$COMMENT)
# delete bukan alfabet
data$COMMENT <- gsub('[^a-zA-Z]', ' ', data$COMMENT)
data$COMMENT <- gsub('\t|\\s+', ' ', data$COMMENT)
# jadikan dalam korpus
koleksi <- data.frame(doc_id=data$COMMENT_ID, text=data$COMMENT)
korpus <- Corpus(DataframeSource(koleksi))


######################## 2. Pengindeksan
tdm <- TermDocumentMatrix(korpus, control = list(
  removePunctuation = TRUE,
  stopwords = FALSE,
  tolower = TRUE,
  stemming = FALSE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stripWhitespace = TRUE))

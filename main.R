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
# delete karakter yang tidak diperlukan
# data$COMMENT <- gsub('[^a-zA-Z]', ' ', data$COMMENT) # delete selain alfabet
# data$COMMENT <- gsub('[0-9]+', ' ', data$COMMENT) # delete angka
data$COMMENT <- gsub('\t|\\s+', ' ', data$COMMENT) # merubah space yg tidak perlu
# jadikan dalam korpus
koleksi <- data.frame(doc_id=data$COMMENT_ID, text=data$COMMENT)
korpus <- Corpus(DataframeSource(koleksi))


######################## 2. Pengindeksan
# stemming
korpus <- tm_map(korpus, stemDocument)
# remove stopword
korpus <- tm_map(korpus, removeWords, stopwords("english"))
# membuat tdm
tdm <- TermDocumentMatrix(korpus)

# menghitung idf
df <- sort(rowSums(as.matrix(tdm)!=0), decreasing=TRUE)
N <- length(colSums(as.matrix(tdm)))
IDF_t <- log(N/df)

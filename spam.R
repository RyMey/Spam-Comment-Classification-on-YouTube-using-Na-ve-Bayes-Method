### install required packages

library(tm)
library(NLP)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(e1071)
library(gmodels)

# Baca Data
setwd("D:/DOCUMENT/MAIL/Kuliah/Temu Kembali Informasi/Project/Spam/")
data <- read.csv("dataEminem.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)
data <- subset(data, select = -c(DATE))
data <- subset(data, select = -c(COMMENT_ID))
data <- subset(data, select = -c(AUTHOR))
# Membuat korpus

data$CLASS <- factor(data$CLASS)
table(data$CLASS)
korpus <- VCorpus(VectorSource(data$CONTENT))
# Pra Proses
korpus <- tm_map(korpus,content_transformer(tolower))
# Menghapus Tanda Baca
korpus <- tm_map(korpus,removePunctuation)
# Menghapus stopwords dalam bahasa inggris
korpus <- tm_map(korpus,removeWords,stopwords("en"))
# Menghapus spasi tambahan
korpus <- tm_map(korpus,stripWhitespace)
# Membuat tdm
tdm <- DocumentTermMatrix(korpus)
# Pra Proses

# Menghitung IDF
df <- sort(rowSums(as.matrix(tdm)!=0), decreasing=TRUE)
N <- length(colSums(as.matrix(tdm)))
IDF_t <- log(N/df)
# Menghitung IDF

# Pembagian Data
# 75% data latih 25% data uji
dataLatih <- tdm[1:336,]
dataUji <- tdm[337:448,]

dataLatihLabel <- data[1:336,]$CLASS
dataUjiLabel <- data[337:448,]$CLASS

# Memisahkan data spam dan bukan spam
spam <- subset(data, CLASS == "1")
notSpam <- subset(data, CLASS == "0")

# Memilih data yang sering muncul
freqWord <- findFreqTerms(dataLatih,5)

# filter tdm dengan freqword
tdm_freq_latih <- dataLatih[,freqWord]
tdm_freq_uji <- dataUji[,freqWord]

# Mengubah kelas menjadi factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

data_latih <- apply(tdm_freq_latih,MARGIN = 2,convert_counts)
data_uji <- apply(tdm_freq_uji,MARGIN = 2, convert_counts)

dataClassifier <- naiveBayes(data_latih,dataLatihLabel)

dataTestPrediction <- predict(dataClassifier,data_uji)

CrossTable(dataTestPrediction, dataUjiLabel,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

#Ubah nilai laplace menjadi 1
dataClassifier2 <- naiveBayes(data_latih,dataLatihLabel,laplace = 1)
dataTestPrediction2 <- predict(dataClassifier2,data_uji)

CrossTable(dataTestPrediction2, dataUjiLabel,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

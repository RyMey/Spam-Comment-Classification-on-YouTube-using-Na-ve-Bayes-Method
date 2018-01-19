### install required packages

library(tm)
library(e1071)
library(class)
######################## 1. Praproses
# Baca Data
file <- file.choose()
data <- read.csv(file,header = TRUE, sep = ",")
data <- subset(data, select = -c(DATE))
data <- subset(data, select = -c(COMMENT_ID))
# lower case
data$AUTHOR <- tolower(data$AUTHOR)
data$CONTENT <- tolower(data$CONTENT)
# paste ke 1 kolom (isi)
data$COMMENT <- paste(data$AUTHOR,data$CONTENT) 

data$CLASS <- factor(data$CLASS)
table(data$CLASS)
korpus <- VCorpus(VectorSource(data$COMMENT))
# Pra Proses
korpus <- tm_map(korpus,content_transformer(tolower))
# Menghapus Tanda Baca
korpus <- tm_map(korpus,removePunctuation)
# Menghapus spasi tambahan
korpus <- tm_map(korpus,stripWhitespace)

######################## 2. Pengindeksan
# stemming
korpus <- tm_map(korpus, stemDocument)
# Menghapus stopwords dalam bahasa inggris
korpus <- tm_map(korpus,removeWords,stopwords("en"))

######################## 3. Pemilihan Fitur
# Membuat tdm
tdm <- DocumentTermMatrix(korpus)
# Menghitung IDF
df <- sort(rowSums(as.matrix(tdm)!=0), decreasing=TRUE)
N <- length(colSums(as.matrix(tdm)))
IDF_t <- log(N/df)

######################## 4,5. Klasifikasi & Evaluasi
# Pembagian Data
# 75% data latih 25% data uji
dataLatih <- tdm[1:336,]
dataUji <- tdm[337:448,]

dataLatihLabel <- data[1:336,]$CLASS
dataUjiLabel <- data[337:448,]$CLASS

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

startK <- ceiling(sqrt(nrow(dataLatih)))-3
predictionLabels <- knn(train = dataLatih, test = dataUji, cl = dataLatihLabel, k = startK)

CrossTable(predictionLabels, dataUjiLabel,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

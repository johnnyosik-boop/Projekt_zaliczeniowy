
#wymagane pakiety
library(tm)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(e1071)

# Dane tekstowe

data <- read.csv("tripadvisor_hotel_reviews.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
head(data, 10)
dim(data)
str(data)
summary(data)
data <- data[data$Review != "" & !is.na(data$Review), ]
dim(data)

# Rozkład ocen

rating_freq <- table(data$Rating)
rating_freq

rating_freq_df <- data.frame(
  Rating = names(rating_freq),
  freq = as.numeric(rating_freq)
  )

rating_freq_df

ggplot(rating_freq_df, aes(x = Rating, y = freq)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Ocena",y = "Liczba recenzji") +
  theme_gdocs() +
  ggtitle("Rozkład ocen w zbiorze TripAdvisor")

# Korpus

corpus <- VCorpus(VectorSource(data$Review))

corpus[[1]]
corpus[[1]][[1]]

# 1. Przetwarzanie i oczyszczanie tekstu

#1.1 Normalizacja i usunięcie zbędnych znaków

corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

corpus <- tm_map(corpus, toSpace, "@")

corpus <- tm_map(corpus, toSpace, "@\\w+")

corpus <- tm_map(corpus, toSpace, "\\|")

corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

corpus <- tm_map(corpus, toSpace, "http\\w*")

corpus <- tm_map(corpus, toSpace, "/")

corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

corpus <- tm_map(corpus, toSpace, "www")

corpus <- tm_map(corpus, toSpace, "~")

corpus <- tm_map(corpus, toSpace, "â€“")

corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, c(
  
  "hotel", "room", "rooms", "stay", "stayed",
  
  "one", "also", "just", "can", "will", "get"
  
))

corpus <- tm_map(corpus, stripWhitespace)

corpus_completed <- corpus

corpus_completed[[1]][[1]]

#1.2 Tokenizacja

tdm <- TermDocumentMatrix(corpus_completed)
tdm <- removeSparseTerms(tdm, 0.98)

tdm

tdm_m <- as.matrix(tdm)

gc()

# 2. Zliczanie częstości słów

v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v),freq = v)
head(tdm_df, 20)


# 3. Eksploracyjna analiza danych

#3.1 Chmura słów

wordcloud(words = tdm_df$word,freq = tdm_df$freq,min.freq = 7,
          max.words = 50, scale = c(3, 0.8),colors = brewer.pal(8, "Dark2"))

#3.2 Wykres najczęstszych słów

word_counts <- tdm_df[1:20, ]
word_counts <- word_counts %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
    ) 

ggplot(word_counts, aes(x = word2, y = freq)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs( x = "Słowa", y = "Liczba wystąpień") +
  theme_gdocs() +
  ggtitle("TOP 20 najczęściej występujących słów w recenzjach hotelowych")

#3.3 Recenzje negatywne i pozytywne

data_negative <- data[data$Rating == 1 | data$Rating == 2, ]
data_positive <- data[data$Rating == 4 | data$Rating == 5, ]

#3.4 Recenzje negatywne

corpus_negative <- VCorpus(VectorSource(data_negative$Review))
corpus_negative <- tm_map( corpus_negative,
  content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

corpus_negative <- tm_map(corpus_negative, toSpace, "@")

corpus_negative <- tm_map(corpus_negative, toSpace, "@\\w+")

corpus_negative <- tm_map(corpus_negative, toSpace, "\\|")

corpus_negative <- tm_map(corpus_negative, toSpace, "[ \t]{2,}")

corpus_negative <- tm_map(corpus_negative, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

corpus_negative <- tm_map(corpus_negative, toSpace, "http\\w*")

corpus_negative <- tm_map(corpus_negative, toSpace, "/")

corpus_negative <- tm_map(corpus_negative, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

corpus_negative <- tm_map(corpus_negative, toSpace, "www")

corpus_negative <- tm_map(corpus_negative, toSpace, "~")

corpus_negative <- tm_map(corpus_negative, toSpace, "â€“")

corpus_negative <- tm_map(corpus_negative, content_transformer(tolower))

corpus_negative <- tm_map(corpus_negative, removeNumbers)

corpus_negative <- tm_map(corpus_negative, removeWords, stopwords("english"))

corpus_negative <- tm_map(corpus_negative, removePunctuation)

corpus_negative <- tm_map(corpus_negative, stripWhitespace)

corpus_negative <- tm_map(corpus_negative, removeWords, c(
  
  "hotel", "room", "rooms", "stay", "stayed",
  
  "one", "also", "just", "can", "will", "get"
  
))

corpus_negative <- tm_map(corpus_negative, stripWhitespace)

tdm_negative <- TermDocumentMatrix(corpus_negative)

tdm_negative <- removeSparseTerms(tdm_negative, 0.98)

tdm_negative_m <- as.matrix(tdm_negative)


v_negative <- sort(rowSums(tdm_negative_m), decreasing = TRUE)

tdm_negative_df <- data.frame(word = names(v_negative),freq = v_negative)
head(tdm_negative_df, 20)


wordcloud( words = tdm_negative_df$word, freq = tdm_negative_df$freq,min.freq = 7,
  max.words = 50,scale = c(3, 0.8),colors = brewer.pal(8, "Dark2"))

negative_counts <- tdm_negative_df[1:20, ]
negative_counts <- negative_counts %>%
   arrange(desc(freq), word) %>%
  mutate(
     word2 = factor(word, levels = rev(unique(word)))
    )

ggplot(negative_counts, aes(x = word2, y = freq)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs( x = "Słowa", y = "Liczba wystąpień" ) +
  theme_gdocs() +
  ggtitle("TOP 20 słów w recenzjach negatywnych")

#3.5 Recenzje pozytywne

corpus_positive <- VCorpus(VectorSource(data_positive$Review))
corpus_positive <- tm_map( corpus_positive,
   content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

corpus_positive <- tm_map(corpus_positive, toSpace, "@")

corpus_positive <- tm_map(corpus_positive, toSpace, "@\\w+")

corpus_positive <- tm_map(corpus_positive, toSpace, "\\|")

corpus_positive <- tm_map(corpus_positive, toSpace, "[ \t]{2,}")

corpus_positive <- tm_map(corpus_positive, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

corpus_positive <- tm_map(corpus_positive, toSpace, "http\\w*")

corpus_positive <- tm_map(corpus_positive, toSpace, "/")

corpus_positive <- tm_map(corpus_positive, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

corpus_positive <- tm_map(corpus_positive, toSpace, "www")

corpus_positive <- tm_map(corpus_positive, toSpace, "~")

corpus_positive <- tm_map(corpus_positive, toSpace, "â€“")

corpus_positive <- tm_map(corpus_positive, content_transformer(tolower))

corpus_positive <- tm_map(corpus_positive, removeNumbers)

corpus_positive <- tm_map(corpus_positive, removeWords, stopwords("english"))

corpus_positive <- tm_map(corpus_positive, removePunctuation)

corpus_positive <- tm_map(corpus_positive, stripWhitespace)

corpus_positive <- tm_map(corpus_positive, removeWords, c(
  
  "hotel", "room", "rooms", "stay", "stayed",
  
  "one", "also", "just", "can", "will", "get"
  
))

corpus_positive <- tm_map(corpus_positive, stripWhitespace)

tdm_positive <- TermDocumentMatrix(corpus_positive)

tdm_positive <- removeSparseTerms(tdm_positive, 0.98)

tdm_positive_m <- as.matrix(tdm_positive)

v_positive <- sort(rowSums(tdm_positive_m), decreasing = TRUE)

tdm_positive_df <- data.frame(word = names(v_positive),freq = v_positive)

head(tdm_positive_df, 20)

wordcloud(words = tdm_positive_df$word,freq = tdm_positive_df$freq,min.freq = 7,
  max.words = 50,scale = c(3, 0.6),colors = brewer.pal(8, "Dark2"))


positive_counts <- tdm_positive_df[1:20, ]
positive_counts <- positive_counts %>%
   arrange(desc(freq), word) %>%
  mutate(
     word2 = factor(word, levels = rev(unique(word)))
    )

ggplot(positive_counts, aes(x = word2, y = freq)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Słowa",y = "Liczba wystąpień") +
  theme_gdocs() +
  ggtitle("TOP 20 słów w recenzjach pozytywnych")

# 4. Analiza TF-IDF

#4.1 Globalna macierz TF-IDF

tdm_tfidf <- TermDocumentMatrix(corpus_completed,
      control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

tdm_tfidf

tdm_tfidf <- removeSparseTerms(tdm_tfidf, 0.98)
tdm_tfidf_m <- as.matrix(tdm_tfidf)

v_tfidf <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)
tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)
head(tdm_tfidf_df, 20)

#4.2 Chmura słów TF-IDF

wordcloud(words = tdm_tfidf_df$word,freq = tdm_tfidf_df$freq, min.freq = 7,
    max.words = 50,scale = c(3, 0.8),colors = brewer.pal(8, "Dark2"))

#4.3 Wykres TOP 20 słów według TF-IDF

tfidf_counts <- tdm_tfidf_df[1:20, ]
tfidf_counts <- tfidf_counts %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
    )

ggplot(tfidf_counts, aes(x = word2, y = freq)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Słowa", y = "Wartość TF-IDF") +
   theme_gdocs() +
  ggtitle("TOP 20 słów według wartości TF-IDF")


# 5. Klasyfikacja recenzji

#5.1 Przygotowanie zmiennej klasy

data$Recommended <- ifelse(
  data$Rating == 1 | data$Rating == 2, "no",
  ifelse(data$Rating == 4 | data$Rating == 5, "yes", "neutral"))

table(data$Recommended)

#5.2 Dane tylko dla klas yes/no

data_classification <- data[data$Recommended == "no" | data$Recommended == "yes", ]
corpus_classification <- VCorpus(VectorSource(data_classification$Review))
corpus_classification <- tm_map(corpus_classification,
  content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

corpus_classification <- tm_map(corpus_classification, toSpace, "@")

corpus_classification <- tm_map(corpus_classification, toSpace, "@\\w+")

corpus_classification <- tm_map(corpus_classification, toSpace, "\\|")

corpus_classification <- tm_map(corpus_classification, toSpace, "[ \t]{2,}")

corpus_classification <- tm_map(corpus_classification, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

corpus_classification <- tm_map(corpus_classification, toSpace, "http\\w*")

corpus_classification <- tm_map(corpus_classification, toSpace, "/")

corpus_classification <- tm_map(corpus_classification, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

corpus_classification <- tm_map(corpus_classification, toSpace, "www")

corpus_classification <- tm_map(corpus_classification, toSpace, "~")

corpus_classification <- tm_map(corpus_classification, toSpace, "â€“")

corpus_classification <- tm_map(corpus_classification, content_transformer(tolower))

corpus_classification <- tm_map(corpus_classification, removeNumbers)

corpus_classification <- tm_map(corpus_classification, removeWords, stopwords("english"))

corpus_classification <- tm_map(corpus_classification, removePunctuation)

corpus_classification <- tm_map(corpus_classification, stripWhitespace)

corpus_classification <- tm_map(corpus_classification, removeWords, c(
  
  "hotel", "room", "rooms", "stay", "stayed",
  
  "one", "also", "just", "can", "will", "get"
  
))

corpus_classification <- tm_map(corpus_classification, stripWhitespace)

#5.3 Macierz TF-IDF do klasyfikacji

dtm_classification <- DocumentTermMatrix(corpus_classification,
  control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  dtm_classification <- removeSparseTerms(dtm_classification, 0.95)

dtm_classification

classification_df <- as.data.frame(as.matrix(dtm_classification))
classification_df$Recommended <- data_classification$Recommended
classification_df$Recommended <- factor(classification_df$Recommended, levels = c("no", "yes"))
dim(classification_df)
table(classification_df$Recommended)
gc()

#5.4 Podział na zbiór treningowy i testowy

set.seed(123)
yes_class <- classification_df[classification_df$Recommended == "yes", ]
no_class <- classification_df[classification_df$Recommended == "no", ]
yes_train_indices <- sample(1:nrow(yes_class), size = floor(0.8 * nrow(yes_class)))
no_train_indices <- sample(1:nrow(no_class), size = floor(0.8 * nrow(no_class)))

trainData <- rbind(yes_class[yes_train_indices, ], no_class[no_train_indices, ])
testData <- rbind( yes_class[-yes_train_indices, ],no_class[-no_train_indices, ])



#5.5 Model SVM

svm_model <- svm(Recommended ~ ., data = trainData,kernel = "linear", probability = TRUE)


#5.6 Predykcja

predictions <- predict(svm_model, newdata = testData)

confusion_matrix <- table(Predicted = predictions,Actual = testData$Recommended)
print(confusion_matrix)

#5.7 Ocena modelu

TP <- confusion_matrix["yes", "yes"]
TN <- confusion_matrix["no", "no"]
FP <- confusion_matrix["yes", "no"]
FN <- confusion_matrix["no", "yes"]

cat("\nTrue Positives (TP):", TP,
    "\nTrue Negatives (TN):", TN,
    "\nFalse Positives (FP):", FP,
    "\nFalse Negatives (FN):", FN, "\n")


precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy <- (TP + TN) / sum(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nAccuracy:", round(accuracy, 2),
    "\nPrecision:", round(precision, 2),
    "\nRecall:", round(recall, 2),
    "\nSpecificity:", round(specificity, 2),
    "\nF1 Score:", round(f1_score, 2), "\n")


#5.8 Wykres metryk

metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Specificity", "F1 Score"),
  Value = c(accuracy, precision, recall, specificity, f1_score)

  )

ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(width = 0.5, color = "black") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(title = "Metryki klasyfikacji SVM",x = "",y = "Wartość") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


#5.9 Macierz pomyłek

confusion_df <- as.data.frame(as.table(confusion_matrix))
confusion_df$Label <- c("True Negative (TN)","False Positive (FP)", 
                        "False Negative (FN)","True Positive (TP)")

ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Count") +
  labs(title = "Macierz pomyłek SVM",x = "Wartość rzeczywista",y = "Wartość przewidziana") +
  theme_minimal(base_size = 14) 




library(geniusr)
library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(udpipe)
library(wordcloud)
library(textdata)
library(reshape2)
library(igraph)

genius_token()

lyrics <- get_lyrics_search(artist_name = "Francesco Guccini",
                            song_title = "Canzone dei dodici mesi")

months <- c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno",
            "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre")

song_months <- c(
  rep(months[1], 4), rep(months[2], 4), rep(months[3], 4), rep(NA, 4),
  rep(months[4], 4), rep(months[5], 4), rep(months[6], 4), rep(NA, 4),
  rep(months[7], 2), rep(months[8], 2), rep(months[9], 4), rep(months[10], 4),
  rep(NA, 4), rep(months[11], 4), rep(months[12], 4), rep(NA, 6)
)

lyrics$month <- song_months

lyrics_by_month <- lyrics %>%
  group_by(month) %>%
  summarise(month_lyrics = paste(line, collapse = " ")) %>%
  arrange(match(month, months))

lyrics_by_month %>% head() 

doc_ids <- vector()
for(i in 1:nrow(lyrics_by_month)){
  id <- paste("doc", toString(i), sep = "")
  doc_ids <- doc_ids %>% append(id)
}

lyrics_by_month <- lyrics_by_month %>% mutate(doc_id = doc_ids, .before = 1)

corpus <- corpus(lyrics_by_month$month_lyrics, docnames = lyrics_by_month$doc_id)
summary(corpus)

cat(as.character(corpus[1]))

corpus_tokens <- corpus %>%
  quanteda::tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower()

txt <- sapply(corpus_tokens, FUN=function(x) paste(x, collapse = "\n"))
udpipe_download_model(language = "italian-isdt", model_dir = "resources/")
lang_model <- udpipe_load_model(file = "resources/italian-isdt-ud-2.5-191206.udpipe")
outL <- udpipe_annotate(lang_model, x = txt, tokenizer = "vertical", trace = TRUE) %>%
  as.data.frame()
it_stopwords <- readLines("https://raw.githubusercontent.com/stopwords-iso/stopwords-it/master/stopwords-it.txt")
outL <- outL %>% filter(!(token %in% it_stopwords) & !(lemma %in% it_stopwords))

outL %>% select(doc_id, token, lemma, upos) %>% sample_n(5)

outL_reduced <- outL %>% filter(upos %in% c("NOUN", "PROPN", "ADJ", "VERB"))

# fct_inorder preserves original order of the column
lemmatized_lyrics <- outL_reduced %>% group_by(doc_id = fct_inorder(doc_id)) %>%
  summarise(lemmatized = paste(lemma, collapse = " "))
lyrics_by_month <- lyrics_by_month  %>% right_join(lemmatized_lyrics, by = "doc_id")

corpus <- lyrics_by_month$lemmatized %>% corpus(docnames = lyrics_by_month$doc_id)

DTM <- corpus %>% tokens() %>% dfm()

DTM

DTM %>% dim()

words <- colnames(DTM)
freqs <- colSums(DTM)
wordlist <- data.frame(words, freqs)
wordlist %>% arrange(-freqs) %>% head()

par(mar=c(1,1,0.5,1))
wordcloud(words = wordlist$words, freq = wordlist$freqs, scale = c(3.5, 0.35), max.words = 50, random.order = F,
          colors = RColorBrewer::brewer.pal(name = "Dark2", n = 4))
text(0.5, 1, "wordcloud with TF ponderation", font = 2)


tf_idf <- dfm_tfidf(DTM)
freqs_tf_idf <- colSums(tf_idf)
words_tf_idf <- colnames(tf_idf)
wordlist_tf_idf <- data.frame(words = words_tf_idf, freqs = freqs_tf_idf)
wordlist_tf_idf %>% arrange(-freqs) %>% head(10)

par(mar=c(1,1,0.2,1))
wordcloud(words = wordlist_tf_idf$words, freq = wordlist_tf_idf$freqs,
          scale = c(3.5, 0.35), max.words = 50, random.order = F,
          colors = RColorBrewer::brewer.pal(name = "Dark2", n = 4))
text(0.5, 1, "wordcloud with TF-IDF ponderation", font = 2)

binDTM <- DTM %>% dfm_weight("boolean")
coocCounts <- t(binDTM) %*% binDTM
as.matrix(coocCounts[16:18, 16:18])

coocTerm <- "sole"
k <- nrow(binDTM)
ki <- sum(binDTM[, coocTerm])
kj <- colSums(binDTM)
names(kj) <- colnames(binDTM)
kij <- coocCounts[coocTerm, ]

mutualInformationSig <- log(k * kij / (ki * kj))
mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]

dicesig <- 2 * kij / (ki + kj)
dicesig <- dicesig[order(dicesig, decreasing=TRUE)]

logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
               + (k - ki - kj + kij) * log(k - ki - kj + kij) 
               + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
               - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
logsig <- logsig[order(logsig, decreasing=T)]

resultOverView <- data.frame(
  names(sort(kij, decreasing=T)[1:10]), sort(kij, decreasing=T)[1:10],
  names(mutualInformationSig[1:10]), mutualInformationSig[1:10], 
  names(dicesig[1:10]), dicesig[1:10], 
  names(logsig[1:10]), logsig[1:10],
  row.names = NULL)
colnames(resultOverView) <- c("Freq-terms", "Freq", "MI-terms", "MI", "Dice-Terms", "Dice", "LL-Terms", "LL")
print(resultOverView)

source("resources/calculateCoocStatistics.R")
numberOfCoocs <- 10
coocTerm <- "sapere"
coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")
print(coocs[1:numberOfCoocs])
resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

# Fill the data.frame to produce the correct number of lines
tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
# Entry of the search word into the first column in all lines
tmpGraph[, 1] <- coocTerm
# Entry of the co-occurrences into the second column of the respective line
tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
# Set the significances
tmpGraph[, 3] <- coocs[1:numberOfCoocs]

# Attach the triples to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)

# Iteration over the most significant numberOfCoocs co-occurrences of the search term
for (i in 1:numberOfCoocs){
  
  # Calling up the co-occurrence calculation for term i from the search words co-occurrences
  newCoocTerm <- names(coocs)[i]
  coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")
  
  #print the co-occurrences
  coocs2[1:10]
  
  # Structure of the temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
  tmpGraph[, 1] <- newCoocTerm
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
  
  #Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
}

# Sample of some examples from resultGraph
resultGraph[sample(nrow(resultGraph), 6), ]


# set seed for graph plot
set.seed(1)

# Create the graph object as undirected graph
graphNetwork <- graph.data.frame(resultGraph, directed = F)

# Identification of all nodes with less than 2 edges
verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]
# These edges are removed from the graph
graphNetwork <- delete.vertices(graphNetwork, verticesToRemove) 

# Assign colors to nodes (search term blue, others orange)
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 

# Set edge colors
E(graphNetwork)$color <- adjustcolor("DarkGray", alpha.f = .5)
# scale significance between 1 and 10 for edge width
E(graphNetwork)$width <- scales::rescale(E(graphNetwork)$sig, to = c(1, 10))

# Set edges with radius
E(graphNetwork)$curved <- 0.15 
# Size the nodes by their degree of networking (scaled between 5 and 15)
V(graphNetwork)$size <- scales::rescale(log(degree(graphNetwork)), to = c(5, 15))

# Define the frame and spacing for the plot
par(mai=c(0,0,1,0)) 

# Final Plot
plot(
  graphNetwork,             
  layout = layout.fruchterman.reingold, # Force Directed Layout 
  main = paste(coocTerm, ' Graph'),
  vertex.label.family = "sans",
  vertex.label.cex = 0.8,
  vertex.shape = "circle",
  vertex.label.dist = 0.5,          # Labels of the nodes moved slightly
  vertex.frame.color = adjustcolor("darkgray", alpha.f = .5),
  vertex.label.color = 'black',     # Color of node names
  vertex.label.font = 2,            # Font of node names
  vertex.label = V(graphNetwork)$name,      # node names
  vertex.label.cex = 1 # font size of node names
)


sentiment_lexicon <- read.table("resources/Italian-NRC-EmoLex.txt",
                                header = TRUE, sep = "\t")
sentiment_lexicon_corpus <- sentiment_lexicon %>% filter(Italian.Word %in% colnames(DTM))
positive_terms <- sentiment_lexicon_corpus %>%
  filter(positive == 1) %>%
  select(Italian.Word) %>% pull()
negative_terms <- sentiment_lexicon_corpus %>%
  filter(negative == 1) %>%
  select(Italian.Word) %>% pull()
counts_positive <- rowSums(DTM[, positive_terms])
counts_negative <- rowSums(DTM[, negative_terms])
counts_all_terms <- rowSums(DTM)
relative_sentiment_frequencies <- data.frame(
  positive = counts_positive / counts_all_terms,
  negative = counts_negative / counts_all_terms
)

sentiments_by_month <- aggregate(relative_sentiment_frequencies,
                                by = list(month = lyrics_by_month$month), mean)

sentiments_by_month %>% head()
df_sentiment <- melt(sentiments_by_month, id.vars = "month")
ggplot(data = df_sentiment, aes(x = month, y = value, fill = variable)) + 
  geom_bar(stat="identity", position="stack") + coord_flip()

positive_months <- aggregate(
  relative_sentiment_frequencies, by = list(month = lyrics_by_month$month),
  mean) %>% filter(positive > negative)
positive_months

negative_months <- aggregate(
  relative_sentiment_frequencies, by = list(month = lyrics_by_month$month),
  mean) %>% filter(negative > positive)
negative_months

neutral_months <- aggregate(
  relative_sentiment_frequencies, by = list(month = lyrics_by_month$month),
  mean) %>% filter(positive == negative)
neutral_months

anger_terms <- sentiment_lexicon_corpus %>%
  filter(anger == 1) %>%
  select(Italian.Word) %>% pull()
fear_terms <- sentiment_lexicon_corpus %>%
  filter(fear == 1) %>%
  select(Italian.Word) %>% pull()
joy_terms <- sentiment_lexicon_corpus %>%
  filter(joy == 1) %>%
  select(Italian.Word) %>% pull()
sadness_terms <- sentiment_lexicon_corpus %>%
  filter(sadness == 1) %>%
  select(Italian.Word) %>% pull()

counts_anger <- rowSums(DTM[, anger_terms])
counts_fear <- rowSums(DTM[, fear_terms])
counts_joy <- rowSums(DTM[, joy_terms])
counts_sadness <- rowSums(DTM[, sadness_terms])

relative_emotion_frequencies <- data.frame(
  anger = counts_anger / counts_all_terms,
  fear = counts_fear / counts_all_terms,
  joy = counts_joy / counts_all_terms,
  sadness = counts_sadness / counts_all_terms
)

emotions_by_month <- aggregate(relative_emotion_frequencies,
                              by = list(month = lyrics_by_month$month), mean)

head(emotions_by_month)

df_emotions <- melt(emotions_by_month, id.vars = "month")
ggplot(data = df_emotions, aes(x = month, y = value, fill = variable)) + 
  geom_bar(stat="identity", position="stack") + coord_flip()

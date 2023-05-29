library(geniusr)
library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(udpipe)
library(wordcloud)
library(textdata)
library(reshape2)

genius_token()

artist_id <- search_artist("Caparezza")$artist_id
artist_songs <- get_artist_songs(artist_id)

songs_ids <- c()
for (song in artist_songs$content) {
  song_id <- song$id
  songs_ids <- songs_ids %>% append(song_id)
}


songs_titles <- c()
songs_albums <- c()
songs_lyrics <- c()
for (i in 1:length(songs_ids)) {
  cat("Getting", i, "of", length(songs_ids), "id:", songs_ids[i], "\n")
  song <- get_song(songs_ids[i])$content
  song_title <- song$title
  song_album <- song$album$name
  song_lyrics <- list(get_lyrics_id(songs_ids[i]))
  # some songs have no album associated with them, so we won't consider them
  # additionally, some songs have no lyrics, we are excluding them too
  if (!is.null(song_album) & nrow(song_lyrics[[1]]) != 0){
    songs_titles <- songs_titles %>% append(song_title)
    songs_albums <- songs_albums %>% append(song_album)
    songs_lyrics <- songs_lyrics %>% append(song_lyrics) 
  }
}

# collapse all lyrics lines into a single text
for (i in 1:length(songs_lyrics)){
  songs_lyrics[[i]] <- songs_lyrics[[i]]$line %>% paste(collapse = "\n")
}

songs_lyrics <- unlist(songs_lyrics)

songs <- data.frame(title = songs_titles, album = songs_albums, lyrics = songs_lyrics)
songs %>% head(1)

# we are going to ignore alternative versions of the same song, such as
# live versions, radio edits and remixes
# we are also keeping only the main albums, ignoring specials or compilations
albums <- c("?! (Caparezza ?!)", "Verità Supposte", "Habemus Capa",
            "Le Dimensioni Del Mio Caos", "Il Sogno Eretico", "Museica",
            "Prisoner 709", "Exuvia")
songs <- songs %>%
  filter(!grepl("Live|Radio Edit|Radio Version|Remix|RMX", title) & album %in% albums)

# finally, we order songs chronologically by the album
songs <- songs %>% arrange(match(album, albums), title)

# add id to each song
doc_ids <- vector()
for(i in 1:nrow(songs)){
  id <- paste("doc", toString(i), sep = "")
  doc_ids <- doc_ids %>% append(id)
}
songs <- songs %>% mutate(doc_id = doc_ids, .before = 1)

table(songs$album) %>% as.data.frame() %>% arrange(-Freq)

caparezza_corpus <- corpus(songs$lyrics, docnames = songs$id)
summary(caparezza_corpus)

cat(as.character(caparezza_corpus[1]))

corpus_tokens <- caparezza_corpus %>%
  quanteda::tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower()

txt <- sapply(corpus_tokens, FUN=function(x) paste(x, collapse = "\n"))
udpipe_download_model(language = "italian-isdt", model_dir = "resources/")
lang_model <- udpipe_load_model(file = "italian-isdt-ud-2.5-191206.udpipe")
outL <- udpipe_annotate(lang_model, x = txt, tokenizer = "vertical", trace = TRUE) %>%
  as.data.frame()
it_stopwords <- readLines("https://raw.githubusercontent.com/stopwords-iso/stopwords-it/master/stopwords-it.txt")
outL <- outL %>% filter(!(token %in% it_stopwords) & !(lemma %in% it_stopwords))

outL %>% select(doc_id, token, lemma, upos) %>% sample_n(5)

outL_reduced <- outL %>% filter(upos %in% c("NOUN", "PROPN", "ADJ", "VERB"))

# fct_inorder preserves original order of the column
lemmatized_lyrics <- outL_reduced %>% group_by(doc_id = fct_inorder(doc_id)) %>%
  summarise(lemmatized = paste(lemma, collapse = " "))
songs <- songs %>% right_join(lemmatized_lyrics, by = "doc_id")

caparezza_corpus <- songs$lemmatized %>% corpus(docnames = songs$id)

collocations <- caparezza_corpus %>% tokens() %>% textstat_collocations %>%
  arrange(-count) %>% head(10)

DTM <- caparezza_corpus %>% tokens() %>% tokens_compound(collocations[c(4,6),]) %>%
  tokens_remove("") %>% dfm()

DTM

DTM %>% dim()

words <- colnames(DTM)
freqs <- colSums(DTM)
wordlist <- data.frame(words, freqs)
wordlist %>% arrange(-freqs) %>% head()

corpus_size <- sum(wordlist$freqs)
corpus_size

vocabulary_size <- nrow(wordlist)
vocabulary_size

words_occurrencies <- wordlist %>% group_by(freqs) %>% summarise(vK = n()) %>% arrange(-vK)
words_occurrencies

lexicon_width <- vocabulary_size/corpus_size
lexicon_width
language_refinement <- words_occurrencies$vK[1] / colSums(words_occurrencies)[2]
language_refinement

wordcloud(words = wordlist$words, freq = wordlist$freqs, scale = c(3.5, 0.35), max.words = 50, random.order = F,
          colors = RColorBrewer::brewer.pal(name = "Dark2", n = 4))
text(0.5, 1, "wordcloud with TF ponderation", font = 2)


tf_idf <- dfm_tfidf(DTM)
freqs_tf_idf <- colSums(tf_idf)
words_tf_idf <- colnames(tf_idf)
wordlist_tf_idf <- data.frame(words = words_tf_idf, freqs = freqs_tf_idf)
wordlist_tf_idf %>% arrange(-freqs) %>% head(10)

wordcloud(words = wordlist_tf_idf$words, freq = wordlist_tf_idf$freqs, scale = c(3.5, 0.35), max.words = 50, random.order = F,
          colors = RColorBrewer::brewer.pal(name = "Dark2", n = 4))
text(0.5, 1, "wordcloud with TF ponderation", font = 2)


lemmatized_lyrics_by_album <- songs %>% group_by(album) %>%
  summarise(lemmatized = paste(lemmatized, collapse = " ")) %>% arrange(match(album, albums))
corpus_album <- lemmatized_lyrics_by_album$lemmatized %>% corpus()

DTM_album <- corpus_album %>% tokens() %>% dfm()
DTM_album
wordlist_album <- DTM_album %>% as.matrix() %>% t()
comparison.cloud(wordlist_album, scale = c(2, 1), max.words = 50, random.order = F,
          colors = RColorBrewer::brewer.pal(name = "Dark2", n = 8))
text(0.5, 1, "wordcloud with TF ponderation", font = 2)




#https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
sentiment_lexicon <- read.table("resources/Italian-NRC-EmoLex.txt", header = TRUE,
                              sep = "\t")
sentiment_lexicon_corpus <- emotion_lexicon %>% filter(Italian.Word %in% colnames(DTM))
positive_terms <- sentiment_lexicon_corpus %>% filter(positive == 1) %>%
  select(Italian.Word) %>% pull()
negative_terms <- sentiment_lexicon_corpus %>% filter(positive == 0) %>%
  select(Italian.Word) %>% pull()
counts_positive <- rowSums(DTM[, positive_terms])
counts_negative <- rowSums(DTM[, negative_terms])
counts_all_terms <- rowSums(DTM)
relative_sentiment_frequencies <- data.frame(
  positive = counts_positive / counts_all_terms,
  negative = counts_negative / counts_all_terms
)
sentiments_by_album <- aggregate(relative_sentiment_frequencies,
                                 by = list(album = songs$album), mean)

head(sentiments_by_album)
df <- melt(sentiments_by_album, id.vars = "album")
ggplot(data = df, aes(x = album, y = value, fill = variable)) + 
  geom_bar(stat="identity", position="stack") + coord_flip()

positive_songs <- aggregate(
  relative_sentiment_frequencies, by = list(album = songs$title),
  mean) %>% filter(positive > negative)









# mamma mia mammà peso


count <- 1
for(i in songs$lyrics){
  if(grepl("don't", i)){
    print(songs$title[[count]])
    readline()
  }
  count <- count + 1
}



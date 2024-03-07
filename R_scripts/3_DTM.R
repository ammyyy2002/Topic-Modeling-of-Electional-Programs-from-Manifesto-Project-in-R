library(quanteda)

# Datenframe zu Corpus-Objekt
corp_prep <- corpus(result, docid_field = 1, text_field = 4)

# erweiterte Stoppwörter
# https://github.com/stopwords-iso/stopwords-de/blob/master/stopwords-de.txt 
stopwords_deutsch <- readLines("C://projects/projektarbeit/stopwords_deutsch.txt", encoding = "UTF-8")
# selbstdefinierte unbedeutende Topwörter
topwords_deutsch <- readLines("C://projects/projektarbeit/topwords_deutsch.txt", encoding = "UTF-8")


# Tokenisierung und Normalisierung: kleinbuchstaben, Zeichen, Stopp- und Topwörter entfernen
corp_toks <- corp_prep %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = stopwords_deutsch, padding = T) %>%
  tokens_remove(pattern = topwords_deutsch, padding = T)


# Erfasse häufigsten 250 Kollokationen, die mindestens 25 mal auftreten, als Liste
corp_kollokationen <- quanteda.textstats::textstat_collocations(corp_toks, min_count = 25)
corp_kollokationen <- corp_kollokationen[1:250, ]
kollokationen <- corp_kollokationen$collocation
kollokationen_liste <- strsplit(kollokationen, " ")
corp_toks <- tokens_compound(corp_toks, kollokationen_liste)


# Dokument-Term-Matrix und entferne nicht repräsentative Wörter
DTM <- corp_toks %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)

# Entferne potenziell leere Reihen/Zeilen
leere_reihen <- rowSums(DTM) <= 0
DTM <- DTM[!leere_reihen, ]
result <- result[!leere_reihen, ]

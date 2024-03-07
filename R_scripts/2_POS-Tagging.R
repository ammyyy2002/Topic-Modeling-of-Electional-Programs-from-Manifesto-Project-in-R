library(dplyr)
library(quanteda)
library(udpipe)
library(tidyverse)
options(stringsAsFactors = FALSE)

# Lade POS_Tagging Model für Tagging und Lemmatisierung  (Dependency Parsing)
m_ger <- udpipe::udpipe_download_model(language = "german-gsd")
m_ger <- udpipe_load_model(file = m_ger$file_model)

# Annotiere jeden Textabschnitt
pb <- txtProgressBar(min = 1,max=nrow(result), style=3)
for (i in 1:nrow(result))
{
  setTxtProgressBar(pb,i)
  
  text <- result$text[i]
  text <- gsub("[ß]", "ss", text)
  text_prepared <- str_squish(text)
  
  # Annotieren am Originaltext und speicheren als data.frame
  df_text_annot <- udpipe::udpipe_annotate(m_ger, x = text_prepared) %>% 
    as.data.frame() %>%
    dplyr::select(-sentence)
  
  # Filtern aller lemmatisierten Nomen und Eigennamen
  options(width = 60)
  knitr::opts_chunk$set(tidy.opts=list(width.cutoff=80), tidy=TRUE)
  text_tagged <- df_text_annot %>% filter(upos %in% c('NOUN','PROPN'))
  text_filtered <- paste(text_tagged$lemma, collapse = " ", sep = "")
  
  result$text[i] <- text_filtered
}

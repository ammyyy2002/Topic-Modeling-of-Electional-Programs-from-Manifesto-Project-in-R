library(ldatuning)
library(topicmodels)

data("AssociatedPress", package="topicmodels")

result <- FindTopicsNumber(
  DTM,
  topics = seq(from = 30, to = 80, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

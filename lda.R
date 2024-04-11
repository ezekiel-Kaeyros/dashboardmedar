library(topicmodels)
library(tidytext)
library(LDAvis)
library(quanteda)
#str(llis.display)


sotu_corpus <- corpus(data$description)
corp = corpus_reshape(sotu_corpus, to = "sentences")
#corp = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
dfm = dfm(corp, remove_punct=T, remove=stopwords("english"))
dfm = dfm_trim(dfm, min_docfreq = 5)

dtm = convert(dfm, to = "topicmodels")
set.seed(1)
m = LDA(dtm, method = "Gibbs", k = 4,  control = list(alpha = 0.1))
m

terms(m, 5)

dtm = dtm[slam::row_sums(dtm) > 0, ]
phi = as.matrix(posterior(m)$terms)
theta <- as.matrix(posterior(m)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
          doc.length = doc.length, term.frequency = term.freq)
widget <- createWidget(json)
serVis(json)

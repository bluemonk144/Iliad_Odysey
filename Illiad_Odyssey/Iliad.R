options(stringsAsFactors = FALSE)
library(quanteda)
require(topicmodels)


Iliad <- readLines("data/Iliad.txt", encoding = "UTF-8", warn = FALSE)



Iliad_df <- data.frame(Paragraph = Iliad, stringsAsFactors = FALSE)

write.csv(Iliad_df, "data/Iliad.csv", row.names = FALSE, fileEncoding = "UTF-8")
Iliad <- read.csv("data/Iliad.csv")

colnames(Iliad) <- "Paragraph"


Iliad = Iliad[-(13310:13609),, drop = FALSE]
Iliad = Iliad[-(1:47), , drop = FALSE]







iliad_corpus <- corpus(Iliad$Paragraph)



corpus_tokens <- iliad_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma,
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T)


iliad_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens,min_count = 10)

iliad_collocations <- iliad_collocations[1:250, ]
corpus_tokens <- corpus_tokens[!is.na(corpus_tokens)]
iliad_collocations <- na.omit(iliad_collocations)

corpus_tokens <- tokens_compound(corpus_tokens, iliad_collocations)

DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 10)




freq <- c("make","man","son")

characters <- c(
  "ulysses", "penelope", "telemachus", "achilles", "agamemnon", "ajax", "alcinous", "amphimedon",
  "amphinomus", "anticlea", "antinous", "arete", "argos", "autolycus", "cassandra", "cicones",
  "circe", "ctesippus", "demodocus", "eumaeus", "eurycleia", "eurymachus", "eurynome", "helios",
  "hermes", "icarus", "irus", "laertes", "leocritus", "medon", "melantho", "melanthius", "menelaus",
  "mentes", "mentor", "nausicaa", "nestor", "perimedes", "phaeacians", "philoetius", "polites",
  "polycaste", "polybus", "polyphemus", "procris", "proteus", "theoclymenus", "themis", "tiresias",
  "zeus", "aeolus", "agelaus", "antiphates", "arcesius", "argives", "aretaon", "artemis", "asteris",
  "cadmus", "caeneus", "calchas", "charybdis", "clytius", "dolius", "dolon", "elephenor", "elpenor",
  "epicasta", "epicurus", "epitherses", "eumolpus", "eurymedon", "eurybates", "eurylochus", "europides",
  "eurypylus", "helen", "hesperides", "hippotes", "hyperion", "icarius", "ixion", "lamus",
  "muses", "nireus", "oileus", "orion", "periboea", "phemios", "phemius", "phemius", "philoetius",
  "piraeus", "pythagoras", "sisyphus", "thalpius", "theseus", "thrasymedes", "thyestes",
  "tityus", "tlepolemus", "tolmides", "trojans", "achaeans", "trojan", "hector", "priam", "atreus", "ajax_son", "tydeus", 
  "telamon", "aeneas", "patroclus", "danaans", "peleus", "idomeneus", "meriones", "diomed","troy"
)

DTM <- DTM[, !(colnames(DTM) %in% freq)]

DTM <- DTM[, !(colnames(DTM) %in% characters)]


sel_idx <- rowSums(DTM) > 0 | grepl("^BOOK\\s+[IVXLCDM]+$", Iliad$Paragraph, ignore.case = TRUE)
Iliad <- Iliad[sel_idx, , drop = FALSE]  

DTM <- DTM[sel_idx, ]





K <- 6

topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 500,
  seed = 1,
  verbose = 25,
  alpha = 0.02))


tmResult <- posterior(topicModel)

beta <- tmResult$terms 

theta <- tmResult$topics

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")


library(LDAvis)
library("tsne")
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)




topicToFilter <- 6



topicThreshold <- 0.1
selectedDocumentIndexes <- (theta[, topicToFilter] >= topicThreshold)
filteredCorpus <- iliad_corpus %>%
  corpus_subset(subset = selectedDocumentIndexes)

filteredCorpus



library(reshape2)
library(ggplot2)
library(pals)


textdata <- Iliad


textdata$Book <- cumsum(grepl("^BOOK\\s+[IVXLCDM]+\\.$", Iliad$Paragraph, ignore.case = TRUE))




textdata$time_period <- factor(textdata$Book, 
                               levels = 1:24, 
                               labels = paste0("Book ", 1:24))


topic_proportion_per_time <- aggregate(theta, by = list(time_period = textdata$time_period), mean)


colnames(topic_proportion_per_time)[2:(K+1)] <- topicNames


vizDataFrame <- melt(topic_proportion_per_time, id.vars = "time_period")


ggplot(vizDataFrame, aes(x=time_period, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topics") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

require(wordcloud2)

topicToViz <- 2

top40terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]
words <- names(top40terms)

probabilities <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]

wordcloud2(data.frame(words, probabilities), shuffle = FALSE)


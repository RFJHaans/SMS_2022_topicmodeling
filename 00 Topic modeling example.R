######################################################
#
#
# Written on April 25th, 2022 by Richard Haans (haans@rsm.nl)
# Tested in R 4.2.0. 
# tm package version 0.7-8
# stm package version 1.3.6
#
#
######################################################
# The "tm" package enables the text mining infrastructure that we will use for LDA.
if (!require("tm")) install.packages("tm")
# The "stm" package enables the estimation of the correlated topic model.
if (!require("stm")) install.packages("stm") 
# The "huge" package is used in making the correlations map.
if (!require("huge")) install.packages("huge") 


#########################################
### Get the data, turn into a corpus, and clean it up
#########################################
# Load data: a series of plaintext files in the "data" folder
# corpus  <- VCorpus(DirSource("data", recursive = FALSE), readerControl = list(reader=readPlain)) #specifies the exact folder where my text file(s) is for analysis with tm.
# save.image("01 Text data loaded.RData")
# Note: you can find the .zip file containing this folder on the Github repository. 

load(url("https://github.com/RFJHaans/SMS_2022_topicmodeling/blob/main/01%20Text%20data%20loaded.RData?raw=true"))

corpusclean = tm_map(corpus, removeNumbers)
corpusclean = tm_map(corpusclean, removePunctuation)
# 3) Transform all upper-case letters to lower-case.
corpusclean = tm_map(corpusclean,  content_transformer(tolower))
# 4) Remove stopwords which do not convey any meaning.
corpusclean = tm_map(corpusclean, removeWords, stopwords("english"))
# 5) And strip whitespace.
corpusclean = tm_map(corpusclean , stripWhitespace)
# 6) Remove additional terms; uncomment and replace words if you want to use.
# corpusclean <- tm_map(corpusclean, removeWords, c("word","word"))


# We then convert the corpus to a "Document-term-matrix" (dtm)
dtm =DocumentTermMatrix(corpusclean)
dtm

# Convert to STM format.
stmdata <- readCorpus(dtm, type = c("slam"))

# From Lindstedt (2019:311):
# "For shorter, focused corpora (i.e., those ranging from a few hundred to a few thousand documents in size),
#     an initial choice between five and 50 topics is best, whereas for larger,
#       unfocused corpora (i.e., those ranging from tens of thousands to hundreds of thousands
#         of documents in size or larger), previous research has found that between 60 and 100 topics are best (Roberts et al. 2018)."
kResult <- searchK(stmdata$documents, stmdata$vocab, K=c(10,20,30,40,50),
                   data=stmdata$meta, init.type = "Spectral", heldout.seed = 123456789,
                   max.em.its = 150, verbose = TRUE, control = list())
# Plot fit statistics.
plot(kResult)
# Note; exclusivity not shown in graphs. Get table.
kResult

# Plot the exclusivity/coherence frontier.
plot(kResult$results$semcoh, kResult$results$exclus, xlab = "Semantic Coherence", ylab = "Exclusivity")
text(kResult$results$semcoh, kResult$results$exclus, labels = paste("K", kResult$results$K), pos = 1)

# More fine-grained run of numbers.
kResult2 <- searchK(stmdata$documents, stmdata$vocab, K=c(15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35),
                    data=stmdata$meta, init.type = "Spectral", heldout.seed = 123456789,
                    max.em.its = 150, verbose = TRUE, control = list())
# Plot fit statistics.
plot(kResult2)
# Note; exclusivity not shown in graphs. Get table.
kResult2

# Plot the exclusivity/coherence frontier.
plot(kResult2$results$semcoh, kResult2$results$exclus, xlab = "Semantic Coherence", ylab = "Exclusivity")
text(kResult2$results$semcoh, kResult2$results$exclus, labels = paste("K", kResult2$results$K), pos = 1)


topicnr = 24
stmmodel <- stm(stmdata$documents, stmdata$vocab, topicnr,
                data = stmdata$meta,
                init.type = "Spectral", seed = 123456789,
                max.em.its = 75, verbose = TRUE, reportevery = 5,
                control = list())

# Get the top terms (here; 10) for each topic.
labels <- labelTopics(stmmodel, n = 10)
labels

# Only write the top probabilities-based terms (the most commonly used compared to the others)
topprob <- as.data.frame(labels$prob)
t(topprob)
write.table(topprob, file = "topprob.csv", sep=',',row.names = TRUE, col.names=NA)

# Block below can be used to write loadings (and frequencies) per term
logbeta <- as.data.frame(stmmodel$beta$logbeta)
logbeta <- as.data.frame(t(logbeta))
vocab <- as.data.frame(stmmodel$vocab)
wordcounts <- stmmodel$settings$dim$wcounts$x
vocab_out <- cbind(vocab,wordcounts,logbeta)
write.table(vocab_out, file = "terms.csv", sep=',',row.names = FALSE)

# An alternative weighting that can be used. Here without further text cleaning it leads to lots of weird terms. 
FREX <- as.data.frame(labels$frex)
write.table(FREX, file = "FREX.csv", sep=',',row.names = TRUE, col.names=NA)

# Plot the correlations between topics, using the "huge" method (see help file for command for info)
mod.out.corr <- topicCorr(stmmodel, method = "huge")
plot(mod.out.corr )

# Writing the loadings per document
loadings <- make.dt(stmmodel)
documents <- as.data.frame(dtm$dimnames$Docs)
loadings_out <- cbind(documents,loadings)
write.table(loadings_out, file = "loadings.csv", sep=',',row.names = TRUE, col.names=NA)

# Summary of topics in the overall corpus
plot(stmmodel, type = "summary", xlim = c(0, .3))


#################################################################
#
# What happens when we make different decisions?
#
#################################################################
# 1) Keep only terms that occur at least 50 times.
minoccur = 50
# 2) Keep only those words that occur in at least 10 of the documents. 
mindocs = 10
# Note that this is completed on the corpus, not the DTM. 
smalldtm = DocumentTermMatrix(corpusclean, control=list(dictionary = findFreqTerms(dtm,minoccur,Inf), 
                                                        bounds = list(global = c(mindocs,Inf))))
smalldtm

# Convert to STM format.
stmdata_small <- readCorpus(smalldtm, type = c("slam"))

# From Lindstedt (2019:311):
# "For shorter, focused corpora (i.e., those ranging from a few hundred to a few thousand documents in size),
#     an initial choice between five and 50 topics is best, whereas for larger,
#       unfocused corpora (i.e., those ranging from tens of thousands to hundreds of thousands
#         of documents in size or larger), previous research has found that between 60 and 100 topics are best (Roberts et al. 2018)."
kResult_small <- searchK(stmdata_small$documents, stmdata_small$vocab, K=c(10,20,30,40,50),
                   data=stmdata_small$meta, init.type = "Spectral", heldout.seed = 123456789,
                   max.em.its = 150, verbose = TRUE, control = list())
# Plot fit statistics.
plot(kResult_small)
# Note; exclusivity not shown in graphs. Get table.
kResult_small

# Plot the exclusivity/coherence frontier.
plot(kResult_small$results$semcoh, kResult_small$results$exclus, xlab = "Semantic Coherence", ylab = "Exclusivity")
text(kResult_small$results$semcoh, kResult_small$results$exclus, labels = paste("K", kResult_small$results$K), pos = 1)

# More fine-grained run of numbers.
kResult2_small <- searchK(stmdata_small$documents, stmdata_small$vocab, K=c(15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35),
                    data=stmdata_small$meta, init.type = "Spectral", heldout.seed = 123456789,
                    max.em.its = 150, verbose = TRUE, control = list())
# Plot fit statistics.
plot(kResult2_small)
# Note; exclusivity not shown in graphs. Get table.
kResult2_small

# Plot the exclusivity/coherence frontier.
plot(kResult2_small$results$semcoh, kResult2_small$results$exclus, xlab = "Semantic Coherence", ylab = "Exclusivity")
text(kResult2_small$results$semcoh, kResult2_small$results$exclus, labels = paste("K", kResult2_small$results$K), pos = 1)


topicnr_small = 29
stmmodel_small <- stm(stmdata_small$documents, stmdata_small$vocab, topicnr_small,
                data = stmdata_small$meta,
                init.type = "Spectral", seed = 123456789,
                max.em.its = 75, verbose = TRUE, reportevery = 5,
                control = list())

# Get the top terms (here; 10) for each topic.
labels_small <- labelTopics(stmmodel_small, n = 10)
labels_small

# Only write the top probabilities-based terms (the most commonly used compared to the others)
topprob_small <- as.data.frame(labels_small$prob)
write.table(topprob_small, file = "topprob_small.csv", sep=',',row.names = TRUE, col.names=NA)

# Block below can be used to write loadings (and frequencies) per term
logbeta_small <- as.data.frame(stmmodel_small$beta$logbeta)
logbeta_small <- as.data.frame(t(logbeta_small))
vocab_small <- as.data.frame(stmmodel_small$vocab)
wordcounts_small <- stmmodel_small$settings$dim$wcounts$x
vocab_out_small <- cbind(vocab_small,wordcounts_small,logbeta_small)
write.table(vocab_out_small, file = "terms_small.csv", sep=',',row.names = FALSE)

# An alternative weighting that can be used. Here it leads to a bit more sensible terms than before. 
FREX_small <- as.data.frame(labels_small$frex)
write.table(FREX_small, file = "FREX_small.csv", sep=',',row.names = TRUE, col.names=NA)

# Plot the correlations between topics, using the "huge" method (see help file for command for info)
mod.out.corr_small <- topicCorr(stmmodel_small, method = "huge")
plot(mod.out.corr_small)

# Writing the loadings per document
loadings_small <- make.dt(stmmodel_small)
documents_small <- as.data.frame(smalldtm$dimnames$Docs)
loadings_out_small <- cbind(documents_small,loadings_small)
write.table(loadings_out_small, file = "loadings_small.csv", sep=',',row.names = TRUE, col.names=NA)

# Summary of topics in the overall corpus
plot(stmmodel_small, type = "summary", xlim = c(0, .3))

save.image("02 Results done.RData")



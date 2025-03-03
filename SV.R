#text data import using quanteda

library(quanteda)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda.textstats)
library(tm)

library(readtext)
library(tidyverse)
library(epubr)
library(broom)

#in this way we can read reports in pdf and extract document variables from the filenames
#the filenames need all to be in exactly the right format
data_SV <- readtext("./nederlands/*.txt", 
                    docvarsfrom = "filenames", 
                    docvarnames = c("title", "author"),
                    dvsep = "-")




#make a corpus






corpus_SV <- data_SV %>% corpus(.)

token_SV <- tokens(
  corpus_SV,
  split_hyphens = TRUE,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  include_docvars = TRUE
)
tokens_SV_nostop <- tokens_select(token_SV, pattern = stopwords("nl"), selection = "remove")
print(tokens_SV_nostop)

library(stopwords)
stoplijst <- scan(file = "stoplijstNL.txt", what = list(word = ""))

stopwordsNL <- c(stoplijst, "â", "des", "ã",  "n", "onze", "hen", "wij", 
                 "we", "wel", "alle", "alleen", "komt", "zullen", "moeten",
                 "zoals", "zegt", "gaat", "komen", "zegt", "zulke",
                 "b.v.", "b.v", "m.a.w.", "m.a.w",  "i.p.v.", "i.p.v",  "kr", "daarom", "staat", "zodat", 
                 "MatteÃ", "MatteÃ_s", "s", "br", "Â", "Matt", "Â", "smith", "geliefde", "broer", 
                 "hartelijke", "hartelijk", "groeten", "j.o", "johan", "horten", 
                 "elias", "aslaksen", "ter", "wille", "fijne", "brief", "dank", "moge", "groet", "vrede",
                 "levens", "bedankt", "zo'n", "œ", "œde", "wanneer", "immers", "nee", "slechts",
                 "elke", "zichzelf", "zeer")
pattern <- c("gods", "drã_bak", "broeders", "jezusâ")
replacement <- c("god", "drobak", "broeder", "jezus")


tokens_SV_nostop <- tokens_remove(tokens_SV_nostop, stopwordsNL)
tokens_SV_nostop <- tokens_replace(tokens_SV_nostop, pattern = pattern, replacement = replacement)
tokens_SV_stemmed <- tokens_wordstem(tokens_SV_nostop, language = "nld")


dfmmat <- dfm(tokens_SV_nostop)



tstat_freq <- textstat_frequency(dfmmat, n = 10, groups = author)
tstat_freq %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_linedraw() +
  facet_wrap(. ~ group)

#n-grams
toks_ngram <- tokens_ngrams(tokens_SV_nostop, n = 2:4)
head(toks_ngram[[1]], 40)
ngram_freq <- textstat_frequency(dfm(toks_ngram), n = 20, groups = author)
ngram_freq %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  labs(x = NULL, y = "Frequency") +
  coord_flip() +
  theme_linedraw() +
  facet_wrap(. ~ group) +
  theme(axis.title = element_text(size = rel(2)),
        axis.text = element_text(size = 5))

#overzicht
table(data_SV$title, data_SV$author)
# totaal aantal woorden
nt <- ntoken(tokens_SV_nostop) %>% as.data.frame()
colnames(nt) <- c("aantal_tokens")
sum(nt$aantal_tokens)

ndoc(tokens_SV_nostop)


#similarity
tstat_dist <- as.dist(textstat_dist(dfmmat))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance", ylab = NULL)


#Topic analysis
set.seed(100)
if (require("stm")) {
  my_lda_fit20 <- stm(dfmmat, K = 20, verbose = FALSE)
  plot(my_lda_fit20)
}


#or
SV_lda <-
  topicmodels::LDA(
    dfmmat,
    k = 10,
    method = "Gibbs",
    control = list(seed = 1965, verbose = 0)
  )


topicmodels::topics(SV_lda)
#turn into dataframe
lda_topics <- tidytext::tidy(SV_lda, matrix = "beta")

top_terms <- lda_topics %>%
  dplyr::group_by(topic) %>%
  dplyr::top_n(10, beta) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(topic, -beta)
#plot beta probability
top_terms %>%
  dplyr::mutate(term = reorder(term, beta)) %>%
  ggplot2::ggplot(ggplot2::aes(term, beta, fill = factor(topic))) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~ topic, scales = "free") +
  ggplot2::coord_flip() +
  ggthemes::theme_economist_white()




# keyness (relateive frequencies)

tstat_key <- textstat_keyness(dfmmat)
textplot_keyness(tstat_key)


#*********************************************************************************

















token_sr <- tokens(
  corpus_sdg,
  split_hyphens = TRUE,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  include_docvars = TRUE
)
tokens_sr_nostop <- tokens_select(token_sr, pattern = stopwords("nl"), selection = "remove")
print(tokens_sr_nostop)

toks_ngram <- tokens_ngrams(tokens_sr_nostop, n = 2:4)
toks_ngram

#look-up dictionary 
dict <- dictionary(file = "dict_sdg_targets.yml")

dict_tokens_sr <- tokens_lookup(token_sr, dictionary = dict, levels = 1)


sr_dfm <- dfm(tokens_SV_nostop)
sr_dfm

topfeatures(sr_dfm, 20)
dfm_trim(sr_dfm, max_termfreq = 0.95, termfreq_type = "quantile" )
head(dfm_trim,20)
dfm_trim


#tokenize AR
token_ar <- tokens(
  corpus_ar,
  split_hyphens = TRUE,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE,
  include_docvars = TRUE
)
tokens_ar_nostop <- tokens_select(token_ar, pattern = stopwords("en"), selection = "remove")
print(tokens_ar_nostop)











#look-up dictionary 
dict <- dictionary(file = "dict_sdg_targets.yml")
dict <- dictionary(list(TAG = c("tax"))) 
dict_tokens <- tokens_lookup(tokens_ar_nostop, dictionary = dict, levels = 2)


mydfm <- dfm(dict_tokens)
topfeatures(mydfm, 20)
dfm_year <- dfm_group(mydfm, groups = year)
head(dfm_year, 20)

set.seed(1, sample.kind = "Rounding")
textplot_network(sr_dfm, omit_isolated = FALSE,
                 edge_color = "#1F78B4",
                 edge_alpha = 0.5,
                 edge_size = 2,
                 vertex_color = "#4D4D4D",
                 vertex_size = 2,
                 vertex_labelcolor = NULL,
                 vertex_labelfont = NULL,
                 vertex_labelsize = 5,
                 offset = NULL,
)


dict3<- dictionary(file = "dict_sdg_small.yml")



set.seed(100)
textplot_wordcloud(mydfm, min_count = 6, random_order = FALSE,
                   rotation = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))


#make document feature matrix
mydfm <- dfm(tokens_nostop,
             tolower = TRUE,
             dfm_stem = TRUE)
head(mydfm, 5)




#trim the dfm
mydfm.un.trim <-
  dfm_trim(
    mydfm,
    min_docfreq = 0.075,
    # min 7.5%
    max_docfreq = 0.90,
    # max 90%
    docfreq_type = "prop"
  ) 

sdg_lda <-
  topicmodels::LDA(
    mydfm.un.trim,
    k = 6,
    method = "Gibbs",
    control = list(seed = 1965, verbose = 0)
  )


topicmodels::topics(sdg_lda)
#table of count of topics
table(topicmodels::topics(sdg_lda))
#terms
topicmodels::terms(sdg_lda, 5)

#turn into dataframe
lda_topics <- tidytext::tidy(sdg_lda, matrix = "beta")

top_terms <- lda_topics %>%
  dplyr::group_by(topic) %>%
  dplyr::top_n(10, beta) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(topic, -beta)
#plot beta probability
top_terms %>%
  dplyr::mutate(term = reorder(term, beta)) %>%
  ggplot2::ggplot(ggplot2::aes(term, beta, fill = factor(topic))) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~ topic, scales = "free") +
  ggplot2::coord_flip() +
  ggthemes::theme_economist_white()


ap_documents <- tidytext::tidy(sdg_lda, matrix = "gamma")
ap_documents

library(seededlda)
dict <- dictionary(file = "sdg_dict.yml")
print(dict)


set.seed(1234, sample.kind = "Rounding")
slda <- textmodel_seededlda(mydfm.un.trim, dict, residual = TRUE)
print(terms(slda, 20))
topics(slda)

library("topicmodels")
data("AssociatedPress", package = "topicmodels")

## We fit 6 topics.
## We specify five seed words for five topics, the sixth topic has no
## seed words.
library("slam")
set.seed(123)
i <- rep(1:5, each = 5)
j <- sample(1:ncol(AssociatedPress), 25)
SeedWeight <- 500 - 0.1
deltaS <- simple_triplet_matrix(i, j, v = rep(SeedWeight, 25),
                                nrow = 6, ncol = ncol(AssociatedPress))

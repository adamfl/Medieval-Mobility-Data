library(tidytext)
library(dplyr)
library(topicmodels)
library(tidyr)

## load data from a file
## theo_text contains cleaned-up text
load(file="lda_demo.RData", verbose=T)

## display first 1000 characters of the text
substr(theo_text, 1, 1000)

## this is the pattern for capturing the entity and its context
m = gregexpr("( [a-z]+){2} ((St[.]? )?[A-Z][a-z]+ )([a-z]+ ){2}",
             theo_text, perl=F)

## extract the matches and save them as text vector
v = unlist(regmatches(theo_text, m))

## extract the entity from the context
m2 = gregexpr("((St[.]? )?[A-Z][a-z]+ )", v)

## entity will be used as name of our 'document'
d_name = unlist(regmatches(v, m2))

## extract the context words
context = regmatches(v, m2, invert=T)

## extract left context
l_context = sapply(context, '[', 1)
## extract right context
r_context = sapply(context, '[', 2)

## do the clean-up - remove spaces at the beginning of lines
l_context = gsub("^ ", "", l_context)
r_context = gsub("^ ", "", r_context)

## add suffixes to indicate left or right side
l_context = gsub(" ", "_L ", l_context)
r_context = gsub(" ", "_R ", r_context)

## combine left and right words into one text string 
## each string is a document
w_context = paste(l_context, r_context, sep=" ")


## create the corpus data frame
text = data.frame(d_name = d_name, context = w_context,
                  stringsAsFactors = F)

## use package 'dplyr' and 'tidytext' to create a document-term matrix
dtm = text %>% 
  unnest_tokens(input=context, output=word) %>% 
  count(d_name, word) %>% 
  cast_dtm(document=d_name, term=word, value=n)

## display information for the document-term matrix
dtm

## fit LDA topic model for 2 topics
## value of alpha influences the proportions of topics in documents
mod = LDA(x=dtm, k=2, method="Gibbs",
          control=list(alpha=1.1, delta=0.1, seed=12345,
                       burnin=1000, 
                       iter=10000, thin=1))

## extract the matrix with probabilities of topics in documents
result = tidy(mod, "gamma") %>% spread(topic, gamma)

write.csv(result, "result.csv", row.names=F)

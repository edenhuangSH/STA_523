source("functions_def.R")

# ----------------------------------------------------------------------
# Preprocessing: 

# ----------------------------------------------------------------------
# Import a list of libraries: 
library(tm)             # text-mining packages
library(topicmodels)    # text models including LDA
library(SnowballC)      # word-stemming package

library(rvest)          # html webscraping
library(XML)            # for webscraping
require(jobbR)          # Indeed API wrapper

library(googleVis)      # for gvisMap()
library(ggmap)          # for geocode()
library(wordcloud)      # for word cloud function
library(RColorBrewer)   # Color Brewer palletes

# some necessary usual libraries:
library(dplyr)
require(stringr)
require(magrittr)
library(shiny)

library(rgeos)          # for map_jobs()
library(rgdal)
library(httr)

# I. fitting LDA model:

# check if lda_model exists and makes model if not
files = dir('data')

if (!('lda_model.RData' %in% files)) {
  # Download the data and description for "Data Analyst"
  dir.create('data', showWarnings = FALSE)
  job_train_lda = JobDescript("data", 10)
  save(job_train_lda, file = file.path('data','job_train_lda.RData'))
  
  # Clean the descriptions
  job_corpus = data2corpus(job_train_lda)
  clean_job_corpus = clean_corpus(job_corpus)
  stem_job_corpus = clean_job_corpus %>% tm_map(stemDocument)
  
  # transform into document term matrix
  dtm = DocumentTermMatrix(stem_job_corpus)
  
  # remove terms that are in greater than 80% of documents
  dtm_reduced = removeCommonTerms(dtm, 0.8)
  dtm_dense = removeSparseTerms(dtm_reduced, 0.9)
  
  # Run the LDA model and get the 5 clusters
  set.seed(0)
  lda_model = LDA(dtm_dense, 3)
  
  # save LDA model
  save(lda_model, file = file.path('data','lda_model.RData'))
}

# # II. Obtain relative job title of each category:
# cat <- cbind(job_train_lda$results.jobtitle, job_train_lda$results.company,
#              topics(lda_model) %>%
#                matrix(ncol = 1)) %>%
#   data.frame(stringsAsFactors = FALSE) %>%
#   setNames(c("JobTitle", "Company", "Cluster"))
# 
# category <- data.frame()
# for (i in 1:3) {
#   category <- rbind(category,
#                     cbind(
#                       cat[which(cat$Cluster == i), 1] %>%
#                         table() %>%
#                         sort(decreasing = TRUE) %>%
#                         head(n = 3L) %>%
#                         as.data.frame() %>%
#                         setNames(c("JobTitle", "JobFrequents")),
#                       cat[which(cat$Cluster == i), 2] %>%
#                         table() %>%
#                         sort(decreasing = TRUE) %>%
#                         head(n = 3L) %>%
#                         as.data.frame() %>%
#                         setNames(c("Company", "ComFrequents")),
#                       cat = i))
# }



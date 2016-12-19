source("functions_def.R")
states = read.csv("states.csv")

# I. fitting LDA model:

# check if lda_model exists and makes model if not
files = dir('data')

if (!('lda_model.RData' %in% files)) {
  # Download the data and description for "Data Analyst"
  dir.create('data', showWarnings = FALSE)
  
  job_train = data.frame()
  for (i in seq_along(states$State)) {
    location = as.character(states$State[i])
    new_jobs = JobDescript("data", 20, location)
    new_jobs$location = as.character(new_jobs$location)
    job_train = bind_rows(job_train, new_jobs)
  }
  
  save(job_train, file = file.path('data','job_train.RData'))
  
  # Clean the descriptions
  job_corpus = data2corpus(job_train)
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

# --------------------------------------------------------------------------
# Webscraping for mapping 

if (!(dir.exists('cities'))) {
  GET("http://www.mapcruzin.com/fcc-wireless-shapefiles/cities-towns.zip",
      write_disk("cities.zip"))
  unzip("cities.zip", exdir="cities")
  
  # read in the shapefile
  shp = readOGR("cities/citiesx020.shp", "citiesx020")
  
  # extract the city centroids with name and state
  geo =
    gCentroid(shp, byid=TRUE) %>%
    data.frame() %>%
    select(lon = x, lat = y) %>%
    mutate(city=shp@data$NAME, state=shp@data$STATE)
  
}

# --------------------------------------------------------------------------
# load data for shiny

job_category = setNames(seq(1,3), c('Cluster 1', 'Cluster 2', 'Cluster 3'))
states = c(' ', states)
load(file.path('data', 'lda_model.RData'))
load(file.path('data', 'job_train.RData'))


# --------------------------------------------------------------------------
# Obtain relative job title of each category

cat <- cbind(job_train_lda$results.jobtitle, job_train_lda$results.company,
             topics(lda_model) %>%
               matrix(ncol = 1)) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  setNames(c("JobTitle", "Company", "Cluster"))

for (i in 1:3) {
  assign(paste0("title", i), cate[which(cat$Cluster == i), 1] %>%
           table() %>%
           sort(decreasing = TRUE) %>%
           head(n = 3L) %>%
           as.data.frame() %>%
           setNames(c("JobTitle", "Frequents")))
  assign(paste0("company", i), cate[which(cat$Cluster == i), 2] %>%
           table() %>%
           sort(decreasing = TRUE) %>%
           head(n = 3L) %>%
           as.data.frame() %>%
           setNames(c("Company", "Frequents")))
}

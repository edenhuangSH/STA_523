source("preprocessing.R")

# ----------------------------------------------------------------------
# Text mining functions - cleaning and transforming
#   data2corpus - converts data frame into text corpus object
#   clean_corpus - applied text cleaning functions to corpus object
#   removeCommonTerms and removePattern - utility functions for clean_corpus

# converts output of JobDescript into tm corpus object
data2corpus = function(indeed_dataframe) {
  
  matrix(indeed_dataframe$job.descript, ncol = 1) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  DataframeSource() %>%
  VCorpus()
}

# removes words that appear in at least n fraction of documents in the corpus
removeCommonTerms = function(dtm, frac) {
  
  t = (table(dtm$j) < dtm$ncol * frac)
  termIndex = as.numeric(names(t))
  dtm[,termIndex]
}

# replaces regex pattern matches with spaces
removePattern = content_transformer(function(x, pattern) gsub(pattern, ' ', x))

# takes raw corpus and returns a clean corpus
clean_corpus = function(corpus) {

  corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removePattern, '-') %>%
    tm_map(removePattern, '/') %>%
    tm_map(removePattern, '\\.') %>%
    tm_map(removePattern, "/") %>%
    tm_map(removePattern, "@") %>%
    tm_map(removePattern, "\\|") %>%
    tm_map(removePattern, ',') %>%
    tm_map(removePattern, '.<.+>') %>%
    tm_map(removeWords, stopwords('en')) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)

}

# ----------------------------------------------------------------------
# LDA predict - takes a document term matrix and lda_model and outputs
#               which cluster a document belongs in
LDA_predict = function(lda_model, dtm) {
  test_probs = posterior(lda_model, dtm)
  test_topics = apply(test_probs$topics, 1, which.max)
  return(test_topics)
}

# -------------------------------------------------------------------------
# Word Cloud Function: plot a word cloud from a corpus
WordCloud = function(corpus) {

  removed_dictionary = c("job", "one", "must", "gender",
                         "minimum", "key", "work", "new", "will",
                         "years", "including", "position", "make",
                         "people", "can", "make", "use", "able",
                         "etc", "look", 'will', 'work', 'able',
                         "require", "look", "within", "various", "may",
                         "should", "would", "include", "use", "using",
                         "responsibilities", "duties", "well", 
                         "experience", "requires")

  corpus = corpus %>% tm_map(removeWords, removed_dictionary)

  wordcloud(corpus,
            scale=c(5,0.25),
            max.words=70,
            random.order=FALSE,
            rot.per=0.35,
            use.r.layout=FALSE,
            colors= brewer.pal(8, "Dark2"))
}




# ----------------------------------------------------------------------
# Mapping function - takes argument a dataframe and returns the location of subset data on Google Map

# reference: https://github.com/dashee87/jobbR
#            https://cran.r-project.org/web/packages/ggmap/ggmap.pdf

# First we derive the dataset geo (lat/long of an arbitrary city) through web-scraping:
# httr's write_disk can act like a cache: it won't download if the file exists

GET("http://www.mapcruzin.com/fcc-wireless-shapefiles/cities-towns.zip", 
    write_disk("cities.zip"))
unzip("cities.zip", exdir="cities")

# read in the shapefile
shp <- readOGR("cities/citiesx020.shp", "citiesx020")

# extract the city centroids with name and state
geo <- 
  gCentroid(shp, byid=TRUE) %>%
  data.frame() %>%
  select(lon = x, lat = y) %>%
  mutate(city=shp@data$NAME, state=shp@data$STATE)

# Now the map_job() function: 
map_jobs <- function(locations){
  
  if(is.null(locations)){
    return(gvisMap(	data.frame(locationvar = 'Durham', tipvar= ''), locationvar = "locationvar" , tipvar ='tipvar',
                    options=list(showTip=TRUE, 
                                 zoomLevel = 2,
                                 showLine=TRUE, 
                                 enableScrollWheel=TRUE,
                                 mapType='normal', 
                                 useMapTypeControl=TRUE,
                                 width="100%", height="75vh")))
  }
  if(nrow(locations) == 0) 
    return(list(html = list(chart = '<h1>No jobs found</h1>')))
  
  locations = locations[locations$results.city != "",]
  
  city = paste0(locations$results.city,", ",locations$results.state)
  geo_city = paste0(geo$city,", ",geo$state)
  
  locations$latitude = as.numeric(sapply(city, function (x) {
    geo$lat[geo_city == x]
  }))
  locations$longitude = as.numeric(sapply(city, function (x) {
    geo$lon[geo_city == x]
  }))
  
  # check that the lat/long of the address is nonempty:
  locations$lat_long <- paste(locations$latitude, locations$longitude, sep = ':')
  locations_complete = locations[!is.na(locations$latitude),]
  
  locations_complete$tip =  with(locations_complete, paste('<a href="', results.url, '"target="_blank"',  '">', results.jobtitle,'</a>',
                                                           '<br>',  results.company, '-', results.city, '<br>', results.snippet))
  
  ds_map <- gvisMap(locations_complete, locationvar = "lat_long" , tipvar = 'tip',
                    options=list(showTip=TRUE, 
                                 showLine=TRUE, 
                                 enableScrollWheel=TRUE,
                                 mapType='normal', 
                                 useMapTypeControl=TRUE,
                                 width="100%", height="75vh", margin= "0",border="none" ))
  
  # test: plot(ds_map)
  ds_map
}









source("functions_def.R")
source("preprocessing.R")

job_category = setNames(seq(1,3), c('Cluster 1', 'Cluster 2', 'Cluster 3'))
states = read.csv("states.csv")
# load(file.path('data', 'lda_model.RData'))

# ui.R
shinyApp(
  ui = fluidPage(
    # Application title
    titlePanel("Job Search with Indeed"),

    # Sidebar with a slider input for sock parameters
    sidebarPanel(
        #Input the job category to search
        selectInput("category", "Select a Job Cluster:", choices = job_category, selected = FALSE),
        hr(),
        #Input the location to search
        selectInput("state", "Select a State:", choices = states$State, selected = FALSE),
        hr(),
        hr(),
        p('Shiny App made by Team Standard Deviants in R, using the jobbR, wordcloud, googleVis and other packages.'),
        p('Special thanks to Prof. Colin Rundel and the amazing course STA 523 at Duke University:'),
        hr(),
        HTML('<a href="http://www2.stat.duke.edu/~cr173/Sta523_Fa16">
             <img src="http://chem.duke.edu/sites/chem.duke.edu/themes/dukechem/images/duke-footer-logo.png" width="150" height="60" border="0" alt="View the STA 523 Course Website"> </a>'),
        tags$div(
          HTML('<br><br><span id=indeed_at><a href="http://www.indeed.com/">jobs</a> powered by <a
               href="http://www.indeed.com/" title="Job Search"><img
               src="http://www.indeed.com/p/jobsearch.gif" style="border: 0;
               vertical-align: middle;" width="80" height="25" alt="Indeed job search"></a></span>'
          )
        )
    ),

    # main panel with tabbed interface
    mainPanel(
      # # output a table to explain the meaning of category:
      # tableOutput("category"),
      hr(),
      # two tabsets to show the word cloud plot.
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("plot")),
        tabPanel("Job Map", htmlOutput("view"))
      )
    )
  ),

  # server.R
  server = function(input, output) {
    # # the outputs for table:
    # output$category = renderTable({
    #   data.frame(category$JobTitle[which(category$cat == input$category)],
    #              category$Company[which(category$cat == input$category)]) %>%
    #     setNames(c("Job Title", "Company"))
    # })

    # render plots for word cloud:
    output$plot = renderPlot({
        df = data()
        corpus = clean_corpus(data2corpus(df))

        if(is.null(df$cluster)==TRUE) {
          print('<h1>No jobs in this category found in the state</h1>')
        }
        else{
          WordCloud(corpus)
        }
      })

    # update the scraping process each time when a different state is seleceted:
    jobs = reactive({
      JobDescript(query = "data", number = 50, input$state)
    })

    # transform the dataframe to document term matrix and do some cleaning:
    data = reactive({
      dat = jobs()

      # clean the dataframe:
      job_dat = data2corpus(dat)
      clean_dat = clean_corpus(job_dat)
      stem_clean_dat = clean_dat %>% tm_map(stemDocument)
      dtm = DocumentTermMatrix(stem_clean_dat) # transform into document term matrix

      # remove terms that are in greater than 80% of documents
      dtm_reduced = removeCommonTerms(dtm, 0.8)
      dtm_dense = removeSparseTerms(dtm_reduced, 0.9)
      dat$cluster = LDA_predict(lda_model, dtm_dense)
      dat = dat %>% filter(cluster==input$category)
    })

    # plot the locations on Google Map:
    output$view = renderGvis({
      map_jobs(data())
    })
  }
)





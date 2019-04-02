library(shiny)
library(dplyr)
library(LDAvis)
library(DT)
library(tidyr)
library(NLP)
library(tm)
library(NLP)
library(slam)
library(topicmodels )
library(tidytext)
library(servr)
library(SnowballC)
library(tsne)
library(rlang)

##############################


#down <- read.csv("data/test_food.csv")

#functions
svd_tsne <- function(x) {tsne(svd(x)$u)}

# age selection
age_sel <- function(df, age){
  if (age == 'all') {
    x = df
  }
  else {
    x = df[df['age']==age,]
  }
  return(x)
}
# selectors: country
country_sel <- function(df, country){
  
  if (country == 'all') {
    x = df
  }
  else {
    x = df[df['country']==country,]
  }
  return(x)
}

# selector: question
question_sel <- function(df, question){
  if (question == 'all') {
    return(df)
  }
  else {
    #col = df[,6]
    #x = df[[col == question,]]
    return(df[df['question']== question,])
  }
}


topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 2) stop("The model must contain > 1 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    mds.method = svd_tsne,
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}




ui <- fluidPage(
  titlePanel("Most frequently used terms in answer - survey analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      
      # select input for age group
      selectInput(inputId = "age", 
                  label = "Select the age:",
                  choices = c("all",
                              "18 - 35 years" = "18-35", 
                              "35 - 45 years" = "35-45", 
                              "45 - 55 years" = "45-55",
                              "55 - 65 years" = "55-65",
                              "65+ years" = "65+"), 
                  selected = "all"),
      # select country:
      selectInput(inputId = "country",
                  label = "Select the country",
                  choices = c("all", "United States", "Australia", "Somewhere else", "Great Britain", "Canada"), 
                  selected = "all"),
      
      # Select variable for x-axis
      selectInput(inputId = "question", 
                  label = "Select a question: ",
                  choices = c("all","question 1" = "q1",
                              "question 2" = "q2",
                              "question 3" = "q3",
                              "question 4" = "q4",
                              "question 5" = "q5",
                              "question 6" = "q6",
                              "question 7" = "q7"), 
                  selected = "all"),
      # select number of clusters between 2 - 20
      sliderInput("cluster", label = "Number of clusters:",
                  min = 2, max = 20, value = 2, step = 1)
    ),
    
    # Output(s)
    mainPanel(
      p("This app was created for topic modelling using LDA to analyse surveys with free text answers. Participants are asked to provide their 
age and to choose one of the provided options for nationality. Aditionally, up to 7 open questions can be asked and analysed by the interviewer. 
The given answers are then saved in the columns q1 - q7.
        Please provide the following column names: " , style = "font-family:'times'; font-si16pt"),
      code("id, age, country,	q1,	q2,	q3,	q4,	q5,	q6,	q7"),
      p("You can either import your own survey data or download and import the provided test dataset (https://v110.shinyapps.io/test/)", 
        style = "font-family:'times'; font-si16pt"),
      p("The questions of the test dataset are as follows: ",  style = "font-family:'times'; font-si16pt"),
      p("1) What's your 'comfort food' ?"),
      p("2) Why do you eat the comfort food ?"),
      p("3) Describe your current diet."),
      p("4) If you chnaged your diet recently, what have you changed ?"),
      p("5) What was your favorite meal when you were a kid ?"),
      p("6) What do you considere a healthy meal ?"),
      p("7) What do you think is the ideal diet ?"), 
      
      #textOutput(outputId = "description"),
      #DT::dataTableOutput("contents"),
      visOutput(outputId = "lda")
    )
  )
)
####################################################################################################

server <- function(input, output) {
  
  
  
  dataframe <-reactive({
    if (is.null(input$file1))
      return(NULL)                
    data <-read.csv(input$file1$datapath, header = T, encoding = 'UTF-8', stringsAsFactors = F)
    df <- data %>% gather(question, answer, q1:q7)
    df <- age_sel(country_sel(question_sel(df, input$question), input$country), input$age)
    df.title <- data.frame(doc_id=row.names(df), text=df$answer)
    corpus <- VCorpus(DataframeSource(df.title))
    df.dtm <- DocumentTermMatrix(corpus, control = list(stemming = TRUE, stopwords = TRUE,
                                                        minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE))
    df.dtm.sparse <-  removeSparseTerms(df.dtm, 0.98)
    df.rowTotals <- apply(df.dtm.sparse , 1, sum) #Find the sum of words in each Document
    df.dtm.sparse   <- df.dtm.sparse[df.rowTotals> 0, ]
    df.dtm.sparse.LDA <- LDA(df.dtm.sparse, k = input$cluster, control = list(seed = 42))
    return(topicmodels2LDAvis(df.dtm.sparse.LDA))
  })
  
 
    
  
  
  output$lda <- renderVis({
    dataframe()
  })
  
  
}

shinyApp(ui, server)
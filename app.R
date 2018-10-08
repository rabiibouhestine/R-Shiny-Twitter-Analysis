#####################################################################################
################################ APP GLOBAL PARAMETERS ############################## 
#####################################################################################



# Loading Packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyalert)
library(shinyjs)
library(data.table)
library(rtweet)
library(tm)
library(sentimentr)
library(plotly)
library(wordcloud2)



# Creating Twitter API Token
create_token(
  app = "Data_Visualised_Scrapper",
  consumer_key = "bUOewpV8fG2lnxu1mHAPAJ3OL",
  consumer_secret = "jkf7cVRnzA2qVvugNhOn5u8wfZ0stFFrbe55Mx8hGoRaMYBySZ",
  access_token = "760218576540270592-fnlpijxiID3decuCWYNAqE77B3fhgLR",
  access_secret = "NgKpPPUNa9eYN17LiSVToQF5GBqhuAPjNxvyWYttglwAM")






#####################################################################################
################################# APP UI (INTERFACE) ################################ 
#####################################################################################



# Constructing the app UI
ui <- tagList(
  
  navbarPage(
  # Making the Navbar Fixed at the top
  position = "fixed-top",
  # Using the cerulean theme
  theme = shinytheme("cerulean"),
  # Naming the app
  title = div(icon("twitter"), "Twitter Analysis"),
  # Setting the name that is shown by the navigator tab
  windowTitle = "Twitter Analysis",
  # Constructing the Profile Analysis Tab
  tabPanel(div(icon("user"), "Profile Analysis"),
           # Leaving padding of 70px at the top
           tags$style(type="text/css", "body {padding-top: 70px;}"),
           # Loading Shiny Alert
           useShinyalert(),
           # Loading Shiny Js  
           useShinyjs(),
           # Constructing the Sidebar Panel
           sidebarPanel(
             width = 3, 
             h5("Please type a valid Twitter ID, choose the number of tweets to extract, then click on the 'Refresh' Button to generate the Analysis :"),
             br(),br(),
             # The text input field that takes a Twitter user ID
             textInput(inputId="twitterID", label=h6("Twitter ID (@)"),value = "elonmusk"),
             # The slider input that takes the number of tweets to extract
             sliderInput(inputId="numTweets",
                         label=h6("Number of tweets to extract"),
                         min = 10,
                         max=3000,
                         value=25),
             # The action button that triggers the process when clicked
             div(align="center",actionButton("go","  Refresh",icon("refresh"),width="60%", class = "btn-primary")),
             br(),br(),br(),br(),br(),
             h5("Made by Rabii Bouhestine !")
           ),
           # Constructing the Main Panel
           mainPanel(
             width = 9, 
             tags$head(
               # Loading the css file
               tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
             ),
             fluidRow(
               # The user profile banner
               column(12,
                      htmlTemplate("userProfile.html")
               )
             ),br(),
             fluidRow(
               # Most Used Words in Tweets
               column(6,
                      box(width = "100%",
                          title = h4("Most Used Words in Tweets:"),
                          wordcloud2Output("tweetsWordCount")
                            )
               ),
               # Tweets Sentiment Analysis
               column(6,
                      box(width = "100%",
                          title = h4("Tweets Sentiment Analysis:"),
                          plotlyOutput("tweetsWordSentiment")
                      )
               )
             ),br(),
             fluidRow(
               # Daily Number of Tweets
               column(12,
                      box(width = "100%",
                          title = h4("Daily Number of Tweets:"),
                          plotlyOutput("tweetsDailyCount")
                      )
                   )
               ),br()
             )
           ),
  # Constructing the Hashtag Analysis Tab
  tabPanel(div(icon("hashtag"), "Hashtag Analysis"),
           # Leaving padding of 70px at the top
           tags$style(type="text/css", "body {padding-top: 70px;}"),
           # Loading Shiny Alert
           useShinyalert(),
           # Constructing the Sidebar Panel
           sidebarPanel(
             width = 3, 
             h5("Please type a Hashtag (#), choose the number of tweets to extract, then click on the 'Refresh' Button to generate the Analysis :"),
             br(),br(),
             # The text input field that takes a Twitter hashtag
             textInput(inputId="hashtag", label=h6("Hashtag (#)"),value = "tesla"),
             # The slider input that takes the number of tweets to extract
             sliderInput(inputId="hashtagNumTweets",
                         label=h6("Number of tweets to extract"),
                         min = 10,
                         max=3000,
                         value=30),
             # The action button that triggers the process when clicked
             div(align="center",actionButton("hashtagGo","  Refresh",icon("refresh"),width="60%", class = "btn-primary")),
             br(),br(),br(),br(),br(),
             h5("Made by Rabii Bouhestine !")
           ),
           # Constructing the Main Panel
           mainPanel(
             width = 9, 
             tags$head(
               # Loading the css file
               tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
               ),
             fluidRow(
               # The Most Popular Tweet
               column(12,
                      box(width = "100%",
                          title = h4("Most Popular Tweet:"),
                          htmlTemplate("tweet.html")
                          )
               )
             ),br(),
             fluidRow(
               # Most Used Words in Tweets
               column(6,
                      box(width = "100%",
                          title = h4("Most Used Words in Tweets:"),
                          wordcloud2Output("hashtagTweetsWordCount")
                          )
                      ),
               # Tweets Sentiment Analysis
               column(6,
                      box(width = "100%",
                          title = h4("Tweets Sentiment Analysis:"),
                          plotlyOutput("hashtagTweetsWordSentiment")
                          )
                      )
               ),br()
             )
           )
  ),
  # Putting a link to source code on the right of the navbar
  tags$script(HTML("var header = $('.navbar > .container-fluid');
                       header.append('<div style=\"float:right;margin-top:15px;margin-left:10px;\"><a target=\"_blank\" href=\"http://google.com\"><i style=\"color:#FFFFFF;\" class=\"fa fa-code\"> Source Code</i></a></div>');
                       console.log(header)"))
  
)






#####################################################################################
################################# APP SERVER FUNCTIONS ############################## 
#####################################################################################



server <- function(input, output, session) {

  
  
  ############################### PROFILE ANALYSIS TAB ##############################
  
  # Initialisation of idTest
  # idTest allows us to test if the user ID of the input is valid 
  idTest = reactiveVal(0)
  
  
  
  # When the "refresh" button is clicked, this code is processed
  observeEvent(input$go, {
    # Making the page go to the top
    shinyjs::runjs("window.scrollTo(0, 0)")
    # Showing a loading message
    withProgress(message = "Testing Validity...", value = 0.75, {
      # Calling the lookup_users and get_timelines functions
      # Calling get_timelines with one tweet, because in here, we are just testing
      user = lookup_users(input$twitterID)
      tweets = get_timelines(input$twitterID, n = 1)
      # if those calls result in an empty table, showing an error message
      if(length(user)==0 | length(tweets)==0){
        # using shinyalert to show an error message
        shinyalert("Wrong User ID !",
                   "Please provide a valid Twitter user ID then click on 'Refresh'",
                   type = "error")
      }
      # if those calls result in a valid table, incrementing the idTest
      # Changing the value of idTest just so it can be captured later on
      else{
        newValue <- idTest() + 1
        idTest(newValue)
      }
    })
    
  })
  
  
  
  # When idTest value changes, this code is processed
  data <- eventReactive(idTest(), ignoreNULL = FALSE, {
    # Showing a loading message
    withProgress(message = "Scrapping Tweets...", value = 0.75, {
      # Calling the lookup_users and get_timelines functions again
      # This time, with the number of tweets in the input
      user = lookup_users(input$twitterID)
      tweets = get_timelines(input$twitterID, n = input$numTweets)
    })
    # After the tweets are extracted, showing a message
    showNotification("Updates May Take Few Seconds...",type="error",duration = 3)
    return(list(user=user,tweets=tweets))
  })

  
  
  # Rendering the user profile Banner
  output$userBannerPic <- renderUI({
    user <- data()$user
    a(style=paste0("background-image: url(",user$profile_banner_url,"/1500x500);
                    background-position: 0 50%;
                    background-size: 100% auto;
                    border-bottom: 1px solid #e1e8ed;
                    border-radius: 4px 4px 0 0;
                    height: 95px;
                    width: 100%;
                    display: block !important;"))
  })
  
  
  
  # Rendering the user profile picture
  output$userProfilePic <- renderUI({
    user <- data()$user
    image = gsub("normal.jpg","400x400.jpg",user$profile_image_url)
    img(src = image,height="45px",class="twPc-avatarImg")
  })
  
  
  
  # Rendering the user name
  output$userName <- renderText({
    user <- data()$user
    user$name
  })
  
  
  
  # Rendering the user id
  output$userID <- renderText({
      user <- data()$user
      paste0("@",user$screen_name)
  })

  
  
  # Rendering the user number of tweets
  output$userNbTweets <- renderText({
      user <- data()$user
      prettyNum(user$statuses_count,big.mark=",", preserve.width="none")
  })
  
  
  
  # Rendering the user number of follows
  output$userNumFollowing <- renderText({
    user <- data()$user
    prettyNum(user$friends_count,big.mark=",", preserve.width="none")
  })
  
  
  
  # Rendering the user number of followers
  output$userNumFollowers <- renderText({
    user <- data()$user
    prettyNum(user$followers_count,big.mark=",", preserve.width="none")
  })
  
  
  
  # Rendering the user number of likes
  output$userNumLikes <- renderText({
    user <- data()$user
    prettyNum(user$favourites_count,big.mark=",", preserve.width="none")
  })
  
  
  
  # Rendering the user number of favorites
  output$userNumFavorites <- renderText({
    user <- data()$user
    prettyNum(user$favorite_count,big.mark=",", preserve.width="none")
  })
  
  
  
  # Rendering the user number of retweets
  output$userNumRetweets <- renderText({
    user <- data()$user
    prettyNum(user$retweet_count,big.mark=",", preserve.width="none")
  })
  
  
  
  # Rendering the user number of listed
  output$userNumListed <- renderText({
    user <- data()$user
    prettyNum(user$listed_count,big.mark=",", preserve.width="none")
  })
  
  
  
  # Rendering the user date of account creation
  output$userCreatedAt <- renderText({
    user <- data()$user
    prettyNum(user$account_created_at)
  })
  
  
  
  # Rendering the user tweets word cloud
  output$tweetsWordCount <- renderWordcloud2({
    # Preparing the plot data
    tweets = data()$tweets
    tweets = tweets$text
    tweets = iconv(tweets, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    tweets = gsub("rt", "", tweets)
    tweets = gsub("@\\w+", "", tweets)
    tweets = gsub("[[:punct:]]", "", tweets)
    tweets = gsub("http\\w+", "", tweets)
    tweets = gsub("https\\w+", "", tweets)
    tweets = gsub("[ |\t]{2,}", "", tweets)
    tweets = gsub("^ ", "", tweets)
    tweets = gsub(" $", "", tweets)
    tweets = gsub("amp", "", tweets)
    tweets = Corpus(VectorSource(tweets))
    tweets = tm_map(tweets, function(x)removeWords(x,stopwords()))
    tweets = as.matrix(TermDocumentMatrix(tweets))
    tweets = data.frame(word = rownames(tweets), freq = rowSums(tweets), row.names = NULL)
    # constructing the plot
    wordcloud2(tweets)
  })

  
  
  # Rendering the user tweets sentiment analysis
  output$tweetsWordSentiment <- renderPlotly({
    # Preparing the plot data
    tweets = data()$tweets
    tweets = tweets$text
    tweets = iconv(tweets, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    tweets = gsub("rt", "", tweets)
    tweets = gsub("@\\w+", "", tweets)
    tweets = gsub("[[:punct:]]", "", tweets)
    tweets = gsub("http\\w+", "", tweets)
    tweets = gsub("https\\w+", "", tweets)
    tweets = gsub("[ |\t]{2,}", "", tweets)
    tweets = gsub("^ ", "", tweets)
    tweets = gsub(" $", "", tweets)
    sentiment = sentiment_by(tweets)
    sentiment = sentiment[ave_sentiment!=0]
    sentiment = as.data.table(sentiment)
    sentiment = sentiment[ave_sentiment < -0.75,sentiment:="Very Negative"]
    sentiment = sentiment[ave_sentiment >= -0.75 & ave_sentiment <= -0.25,sentiment:="Negative"]
    sentiment = sentiment[ave_sentiment > -0.25 & ave_sentiment < 0.25,sentiment:="Neutral"]
    sentiment = sentiment[ave_sentiment >= 0.25 & ave_sentiment <= 0.75,sentiment:="Postive"]
    sentiment = sentiment[ave_sentiment > 0.75,sentiment:="Very Postive"]
    sentiment = sentiment[,sentiment:=factor(sentiment, levels = c("Very Negative","Negative","Neutral","Postive","Very Postive"))]
    sentiment = as.data.table(sentiment[, table(sentiment)])
    sentiment = sentiment[,sentiment:=factor(sentiment, levels = c("Very Negative","Negative","Neutral","Postive","Very Postive"))]
    # constructing the plot
    plot_ly(sentiment,
            x = ~sentiment,
            y = ~N,
            type = 'bar',
            marker = list(color = c('#f2421a',
                                    '#f56924',
                                    '#3399cc',
                                    '#9ed670',
                                    '#009246'))) %>%
      layout(xaxis = list(title = "Sentiment"),
             yaxis = list(title = "Frequency"))
  })
  
  
  
  # Rendering the user tweets daily count
  output$tweetsDailyCount <- renderPlotly({
    # Preparing the plot data
    tweets = data()$tweets
    tweets = as.data.table(tweets$created_at)
    tweets = tweets[,x:=as.Date(x,"%Y-%m-%d")]
    tweets = tweets[,.N,by="x"]
    # constructing the plot
    plot_ly(tweets, x = ~x, y = ~N, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Days"),
             yaxis = list(title = "Number of Tweets"))
  })

  
  
  ############################### HASHTAG ANALYSIS TAB ##############################
  
  # Initialisation of hashtagTest
  # hashtagTest allows us to test if the hashtag the input is valid 
  hashtagTest = reactiveVal(0)

  
  
  # When the "refresh" button is clicked, this code is processed
  observeEvent(input$hashtagGo, {
    # Making the page go to the top
    shinyjs::runjs("window.scrollTo(0, 0)")
    # Showing a loading message
    withProgress(message = "Testing Validity...", value = 0.75, {
      # Calling the search_tweets function
      # Calling search_tweets with 10 tweets, because in here, we are just testing
      tweets = search_tweets(input$hashtag, n = 10, include_rts = FALSE)
      # if those calls result in an empty table, showing an error message
      if(length(tweets)==0){
        # using shinyalert to show an error message
        shinyalert("No Tweets were found with this hashtag !",
                   "Please provide another hashtag then click on 'Refresh'",
                   type = "error")
      }
      # if those calls result in a valid table, incrementing the hashtagTest
      # Changing the value of hashtagTest just so it can be captured later on
      else{
        newValue <- hashtagTest() + 1
        hashtagTest(newValue)
      }
    })
  })

  
  
  # When hashtagTest value changes, this code is processed
  hashtagData <- eventReactive(hashtagTest(), ignoreNULL = FALSE, {
    # Showing a loading message
    withProgress(message = "Scrapping Tweets...", value = 0.75, {
      # Calling the search_tweets function again
      # This time, with the number of tweets in the input
      tweets = search_tweets(input$hashtag, n = input$hashtagNumTweets, include_rts = FALSE)
      # Extracting the most popular tweet
      mostPopularTweet = tweets[tweets$retweet_count==max(tweets$retweet_count),]
      mostPopularTweet = mostPopularTweet[1,]
    })
    # After the tweets are extracted, showing a message
    showNotification("Updates May Take Few Seconds...",type="error",duration = 3)
    return(list(tweets=tweets,mostPopularTweet=mostPopularTweet))
  })
  
  
  
  # Rendering the most popular tweet user info
  output$hashtagUserName <- renderUI({
    # Preparing data
    user <- hashtagData()$mostPopularTweet
    # Constructing info  
    HTML(paste0("<strong class='tweetEntry-fullname'>",
                user$name,
                "</strong>",
                "<span class='tweetEntry-username'>",
                "<b>",
                "  @",user$screen_name,
                "</b>",
                "</span>",
                "<span class='tweetEntry-timestamp'>",
                "  - ",user$created_at,
                "</span>"))
    
  })
  
  
  
  # Rendering the most popular tweet user profile picture
  output$hashtagUserProfilePic <- renderUI({
    # Preparing data
    user <- hashtagData()$mostPopularTweet
    # Showing picture  
    image = gsub("normal.jpg","400x400.jpg",user$profile_image_url)
    img(src = image,height="45px",class="tweetEntry-avatar")

  })
  
  
  
  # Rendering the most popular tweet content
  output$hashtagTweetText <- renderUI({
    # Preparing data
    user <- hashtagData()$mostPopularTweet
    # Showing text
    HTML(user$text)
  })
  
  
  
  # Rendering the most popular tweet number of likes and retweets
  output$hashtagRetweets <- renderUI({
    # Preparing data
    user <- hashtagData()$mostPopularTweet
    # Showing likes and retweets
    HTML(paste0("<i class='fa fa-retweet' style='width: 80px'> ",user$retweet_count,"</i>",
                "<i class='fa fa-heart' style='width: 80px'> ",user$favorite_count,"</i>"))
    
  })

  
  
  # Rendering tweets word cloud
  output$hashtagTweetsWordCount <- renderWordcloud2({
    # Preparing the plot data
    tweets = hashtagData()$tweets
    tweets = tweets$text
    tweets = iconv(tweets, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    tweets = gsub("rt", "", tweets)
    tweets = gsub("@\\w+", "", tweets)
    tweets = gsub("[[:punct:]]", "", tweets)
    tweets = gsub("http\\w+", "", tweets)
    tweets = gsub("https\\w+", "", tweets)
    tweets = gsub("[ |\t]{2,}", "", tweets)
    tweets = gsub("^ ", "", tweets)
    tweets = gsub(" $", "", tweets)
    tweets = gsub("amp", "", tweets)
    tweets = Corpus(VectorSource(tweets))
    tweets = tm_map(tweets, function(x)removeWords(x,stopwords()))
    tweets = as.matrix(TermDocumentMatrix(tweets))
    tweets = data.frame(word = rownames(tweets), freq = rowSums(tweets), row.names = NULL)
    # Constructing the plot
    wordcloud2(tweets)
    
  })
  
  
  
  # Rendering tweets sentiment analysis
  output$hashtagTweetsWordSentiment <- renderPlotly({
    # Preparing the plot data
    tweets = hashtagData()$tweets
    tweets = tweets$text
    tweets = iconv(tweets, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    tweets = gsub("rt", "", tweets)
    tweets = gsub("@\\w+", "", tweets)
    tweets = gsub("[[:punct:]]", "", tweets)
    tweets = gsub("http\\w+", "", tweets)
    tweets = gsub("https\\w+", "", tweets)
    tweets = gsub("[ |\t]{2,}", "", tweets)
    tweets = gsub("^ ", "", tweets)
    tweets = gsub(" $", "", tweets)
    sentiment = sentiment_by(tweets)
    sentiment = sentiment[ave_sentiment!=0]
    sentiment = as.data.table(sentiment)
    sentiment = sentiment[ave_sentiment < -0.75,sentiment:="Very Negative"]
    sentiment = sentiment[ave_sentiment >= -0.75 & ave_sentiment <= -0.25,sentiment:="Negative"]
    sentiment = sentiment[ave_sentiment > -0.25 & ave_sentiment < 0.25,sentiment:="Neutral"]
    sentiment = sentiment[ave_sentiment >= 0.25 & ave_sentiment <= 0.75,sentiment:="Postive"]
    sentiment = sentiment[ave_sentiment > 0.75,sentiment:="Very Postive"]
    sentiment = sentiment[,sentiment:=factor(sentiment, levels = c("Very Negative","Negative","Neutral","Postive","Very Postive"))]
    sentiment = as.data.table(sentiment[, table(sentiment)])
    sentiment = sentiment[,sentiment:=factor(sentiment, levels = c("Very Negative","Negative","Neutral","Postive","Very Postive"))]
    # Constructing the plot
    plot_ly(sentiment,
            x = ~sentiment,
            y = ~N,
            type = 'bar',
            marker = list(color = c('#f2421a',
                                    '#f56924',
                                    '#3399cc',
                                    '#9ed670',
                                    '#009246'))) %>%
      layout(xaxis = list(title = "Sentiment"),
             yaxis = list(title = "Frequency"))
  })

}


#####################################################################################
##################################### APP CALL ###################################### 
#####################################################################################



shinyApp(ui = ui, server = server)



#####################################################################################
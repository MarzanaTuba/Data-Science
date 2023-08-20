library(rvest)
library(tidyverse)
library(dplyr)
library(DescTools)
library(mosaicData)
library(ggplot2)
library(shiny)
library(shinydashboard)



get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

movies = data.frame()

for (page_result in seq(from = 1, to = 901, by = 300)) {
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                page_result, "&ref_=adv_nxt")
  page = read_html(link)
  
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>%
    html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)
  
  
  movies = rbind(movies, data.frame(name, year, rating, synopsis, cast, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

write.csv(movies, "movies.csv")

#import data from csv in r
read.csv("movies.csv")




#remove /10 from rate column data
movies$year <- gsub("\\(|\\?", "", as.character(movies$year))
movies$year <- gsub("\\)|\\?", "", as.character(movies$year))
movies$year <- gsub("\\I|\\?", "", as.character(movies$year))

#handling missing data
sum(is.na(movies$name))
sum(is.na(movies$year))
sum(is.na(movies$rating))
sum(is.na(movies$synopsis))

#data transformation
movies <- transform(movies, year = as.integer(year))
movies <- transform(movies, rating = as.integer(rating))

#data type
typeof(movies$year)
typeof(movies$rating)

#smooth noisy data
boxplot(movies$year)
boxplot(movies$rating)

#remove outliner
boxplot(movies$year, outline = FALSE)


#rounding to 2dp
movies$rating = round(movies$rating, 2)

#mean of rating
mean(movies$rating)

#median of rating
median(movies$rating)

#mode of rating
Mode(movies$rating)

#range of rating
max(movies$rating) - min(movies$rating)

#variance of rating
var(movies$rating)

#Standard deviation
sd(movies$rating)

#Quartile of rating
quantile(movies$rating)

#Interquartile range
IQR(movies$rating)



#histogram of rating
Movie_Rating = hist(movies$rating)


#data visualization 
ggplot(movies, aes ( x = rating ,y = year))+
  geom_point(color="red",alpha=.7,size=2)+geom_smooth(method="lm") +
  labs(title = "Relationship between Year & Movie_Rating",
       x = "Rating",y = "Year between 1970 to 2020")


#Define ui
ui <- dashboardPage(
  dashboardHeader(title = "Adventure Movie Ratings"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Distribution of Movie Ratings",
        status = "primary",
        solidHeader = TRUE, 
        width = 6,
        plotOutput("rating_hist")
        
      ), 
      box(
        title = "Relationship between Year and Movie Rating",
        status = "primary",
        solidHeader = TRUE, 
        width = 6,
        plotOutput("rating_year")
            )
    ),
    fluidRow(
      box(
        title = "Top 100 Movies",
        status = "primary",
        solidHeader = TRUE, 
        width = 12, 
        dataTableOutput("top10_movies")
        
        
      )
    )
  )
)

#Define server
server <- function(input, output){
  
  #Movie rating histogram
  output$rating_hist <- renderPlot({
    ggplot(movies, aes(x = rating)) + 
      geom_histogram(color = "black", fill = "blue", binwidth = 0.25) +
      ggtitle("Distribution of Movie Ratings") +
      labs(x = "Movie Rating", y = "Frequency")
  })
  
  
  #Movie rating vs year scatterplot 
  output$rating_year <- renderPlot({
    ggplot(movies, aes(x = rating, y = year)) + 
      geom_point(color = "red", alpha = .7, size = 2) +
      geom_smooth(method = "lm") + 
      ggtitle("Relationship between Year and Movie Rating") +
      labs(x = "Rating", y = "Year between 1970 to 2020")
    
  })
  
  #Top 10 movies table
  output$top10_movies <- renderDataTable({
    movies %>%
      arrange(desc(rating)) %>%
      select(name, rating, year, cast) %>%
      head(10)
  })
}

#Run the app
shinyApp(ui, server)
  


# pck = c("shiny","shinydashboard","rhandsontable","sp","data.table","tidyverse")
# tbi = setdiff(pck,installed.packages())
# if(length(tbi)>0){
#   install.packages(tbi)
# }
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(sp)
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
workingdirectory = "C:/Users/Mustafa Oguz Turkkan/Documents/GitHub/etm01-Delicioussaw/"
setwd(workingdirectory)
matches=readRDS("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")

setDT(matches)
matches[,timestamp:=as_datetime(date,tz='Turkey')]

#get rid of unused columns for better visuality
matches[,c("leagueId","matchId","date","type")]=list(NULL)

#get all the unique teams list
teams <- union(unique(matches$home), unique(matches$away))
teams <- sort(teams)
teams

#I got to see different names for same teams so we need to adjust them to have the same name
matches$home[matches$home=="manchester-utd"]="manchester united"
matches$home[matches$home=="manchester-united"]="manchester united"
matches$home[matches$home=="manchester-city"]="manchester city"
matches$home[matches$home=="crystal-palace"]="crystal palace"
matches$home[matches$home=="newcastle utd"]="newcastle"
matches$home[matches$home=="stoke"]="stoke city"
matches$home[matches$home=="west-ham"]="west ham"

matches$away[matches$away=="manchester-utd"]="manchester united"
matches$away[matches$away=="manchester-united"]="manchester united"
matches$away[matches$away=="manchester-city"]="manchester city"
matches$away[matches$away=="crystal-palace"]="crystal palace"
matches$away[matches$away=="newcastle utd"]="newcastle"
matches$away[matches$away=="stoke"]="stoke city"
matches$away[matches$away=="west-ham"]="west ham"

teams <- union(unique(matches$home), unique(matches$away))
teams <- sort(teams)
teams

#create a column called season and add season information to the data based on the dates of matches
matches$season <- list(0)

for(i in 1:nrow(matches)){
  if(as.numeric(format(matches[i,timestamp], '%m')) > 6){
    matches$season[i] <- paste(format(matches[i,timestamp], '%Y'), "-", as.character(as.numeric(format(matches[i,timestamp], '%Y')) + 1))
  }else{
    matches$season[i] <- paste(as.character(as.numeric(format(matches[i,timestamp], '%Y')) - 1), "-", format(matches[i,timestamp], '%Y'))
  }
}

# 4 pivot tables are created to obtain average goals for home,away and conceded goals for home, away.then the tables are combined by using merge function.
avg_goals_table=matches[,c('score_home','score_away'):=tstrsplit(score,':')]
matches[,c("score_home","score_away")]=list(NULL)

avg_goals_table[,score_home:=as.numeric(score_home)]
avg_goals_table[,score_away:=as.numeric(score_away)]

avg_goal_away=avg_goals_table[,list(avg_goal_away=mean(score_away,na.rm=T)),by=list(season,away)]
colnames(avg_goal_away) <- c("season", "team","avg_goal_away")
avg_goal_away$season_team=paste(avg_goal_away$season,avg_goal_away$team)

avg_conceded_goal_away=avg_goals_table[,list(avg_conceded_goal_away=mean(score_home,na.rm=T)),by=list(season,away)]
colnames(avg_conceded_goal_away) <- c("season", "team","avg_conceded_goal_away")
avg_conceded_goal_away$season_team=paste(avg_conceded_goal_away$season,avg_conceded_goal_away$team)

avg_goal_home=avg_goals_table[,list(avg_goal_home=mean(score_home,na.rm=T)),by=list(season,home)]
colnames(avg_goal_home) <- c("season", "team","avg_goal_home")
avg_goal_home$season_team=paste(avg_goal_home$season,avg_goal_home$team)

avg_conceded_goal_home=avg_goals_table[,list(avg_conceded_goal_home=mean(score_away,na.rm=T)),by=list(season,home)]
colnames(avg_conceded_goal_home) <- c("season", "team","avg_conceded_goal_home")
avg_conceded_goal_home$season_team=paste(avg_conceded_goal_home$season,avg_conceded_goal_home$team)

masterpiece=merge(avg_goal_away,avg_conceded_goal_away,by=c('season_team'),all.x=TRUE)
masterpiece=merge(masterpiece,avg_conceded_goal_home,by=c('season_team'),all.x=TRUE)
masterpiece=merge(masterpiece,avg_goal_home,by=c('season_team'),all.x=TRUE)
masterpiece[,c("season.x","team.x","season.y","team.y","season_team")]=list(NULL)
masterpiece[,c("season.y","team.y")]=list(NULL)

#change the data from wide to long for use in plot
avg_plot<-reshape(masterpiece,idvar=c("team.x","season.x"),
                  varying=c("avg_goal_home", "avg_conceded_goal_home","avg_conceded_goal_away","avg_goal_away"),
                  v.name=c("average"),
                  times=c("avg_goal_home", "avg_conceded_goal_home","avg_conceded_goal_away","avg_goal_away"),
                  new.row.names = 1:720,direction="long")
avg_plot <- avg_plot %>% rename(types = time)

#Shiny code
ui <- dashboardPage(
  dashboardHeader(title = "Team Scores & Goals Statistics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Match Scores", tabName = "matches", icon = icon("dashboard")),
      menuItem("Average Goals", tabName = "goals", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "matches",
              fluidRow(
                column(width=9,
                       selectInput(inputId = "season",label="Choose a season",choices = unique(matches$season),selected = "2010 - 2011"),
                       selectInput(inputId = "team",label="Choose a team",choices = unique(teams),selected = "arsenal"),
                       dataTableOutput("matches_table")
                       
                )
                
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "goals",
              fluidRow(
                column(width=9,
                       selectInput(inputId = "season_goals",label="Choose a season",choices = unique(avg_plot$season.x),selected = "2010 - 2011"),
                       selectInput(inputId = "team_goals",label="Choose a team",choices = unique(teams),selected = "arsenal"),
                       plotOutput("avg_goal_plot")
                       
                )
                
                
              )      
      )
      
    )
  )
)


server <- function(input, output) {
  
  output$matches_table <-renderDataTable({
    
    
    table_to_show=matches %>% 
      filter(season==input$season) %>% 
      filter(home==input$team | away==input$team)
    
    table_to_show
  })
  
  output$avg_goal_plot <- renderPlot({
    
    ggplot(avg_plot %>% 
             filter(season.x==input$season_goals) %>% 
             filter(team.x==input$team_goals)) +
          geom_bar(aes(x=types,y=average,fill=types),stat = "identity") +
          geom_text(aes(x=types,y=average,label=average),size=5,colour="red",vjust=0)
    
  })
  
}

shinyApp(ui, server)

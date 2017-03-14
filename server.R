# Fantasy Football server script

library(shiny)
library(plyr)
#library(XML)
library(ggvis)
#library(jsonlite)
#library(RCurl)
#library(XML)

gaa_data <- read.csv("LeagueResults_01_17.csv", stringsAsFactors = F)

format(gaa_data$Date , format="%d/%b/%Y")
gaa_data$Date <- as.Date(gaa_data$Date, "%d/%m/%Y")
available_teams <- sort(unique(gaa_data$Team_Name))

shinyServer(function(input, output) {
  
  gaa_date <- reactive({
    gaa_data[which(gaa_data$Date>=input$season_range_c[1] & gaa_data$Date<=input$season_range_c[2]),]
  })
  
  
  y_numerator <- reactive({
    output <- data.frame(team=available_teams, stat=0)
    if(input$stat_choice_y == "goals"){
      for(i in 1:nrow(output)){
        output[i,"stat"] <- sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Team_Goals"]) + 
          sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Opp_Goals"])
      }
    }else if(input$stat_choice_y == "goals_conc"){
      for(i in 1:nrow(output)){
        output[i,"stat"] <- sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Opp_Goals"]) + 
          sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Team_Goals"])
      }
    }
    return(output)
  })
  
  y_denominator <- reactive({
    output <- data.frame(team=available_teams, stat=1)
    if(input$stat_choice_y_per == "no_div"){
      return(output)
    }else if(input$stat_choice_y_per == "p_game"){
      for(i in 1:nrow(output)){
        output[i,"stat"] <- sum(gaa_date()$Team_Name==output[i,"team"]) + sum(gaa_date()$Opp_Name==output[i,"team"])
      }
    }else if(input$stat_choice_y_per == "p_goal_score"){
      for(i in 1:nrow(output)){
        output[i,"stat"] <- sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Team_Goals"]) + 
          sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Opp_Goals"])
      }
    }else if(input$stat_choice_y_per == "p_goal_conc"){
      for(i in 1:nrow(output)){
        output[i,"stat"] <- sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Opp_Goals"]) + 
          sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Team_Goals"])
      }
    }
    return(output)
  })
  
  x_numerator <- reactive({
    output <- data.frame(team=available_teams, stat=0)
    if(input$stat_choice_x == "goals"){
      for(i in 1:nrow(output)){
        output[i,"stat"] <- sum(gaa_data[which(gaa_data$Team_Name==output[i,"team"]), "Team_Goals"]) + 
          sum(gaa_data[which(gaa_data$Opp_Name==output[i,"team"]), "Opp_Goals"])
      }
    }else if(input$stat_choice_x == "goals_conc"){
      for(i in 1:nrow(output)){
        output[i,"stat"] <- sum(gaa_data[which(gaa_data$Team_Name==output[i,"team"]), "Opp_Goals"]) + 
          sum(gaa_data[which(gaa_data$Opp_Name==output[i,"team"]), "Team_Goals"])
      }
    }
    return(output)
  })
  
  x_denominator <- reactive({
    output <- data.frame(team=available_teams, stat=1)
    if(input$stat_choice_y_per == "no_div"){
      return(output)
    }else if(input$stat_choice_x_per == "p_game"){
      for(i in 1:nrow(output)){
        output[i,"stat"] <- sum(gaa_date()$Team_Name==output[i,"team"]) + sum(gaa_date()$Opp_Name==output[i,"team"])
      }
    }else if(input$stat_choice_x_per == "p_goal_score"){
      for(i in 1:nrow(output)){
        output[i,"stat"] <- sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Team_Goals"]) + 
          sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Opp_Goals"])
      }
    }else if(input$stat_choice_x_per == "p_goal_conc"){
      for(i in 1:nrow(output)){
        output[i,"stat"] <- sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Opp_Goals"]) + 
          sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Team_Goals"])
      }
    }
    return(output)
  })
  
  
  output$plot_stats_custom <- renderPlot({
    #plot(gaa_data$Team_Goals, gaa_data$Opp_Goals)
    plot(y_numerator()$stat/y_denominator()$stat, x_numerator()$stat/x_denominator()$stat)
  })
  
  
  })






  
  
  
  
  
  
  

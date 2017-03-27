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
available_teams <- sort(unique(c(gaa_data$Team_Name,gaa_data$Opp_Name)))

team_colours_data <- data.frame(team=c("ANTRIM", "ARMAGH", "CARLOW", "CAVAN", "CLARE", "CORK", "DERRY", 
                           "DONEGAL", "DOWN", "DUBLIN", "FERMANAGH", "GALWAY", "KERRY", "KILDARE", "KILKENNY", 
                           "LAOIS", "LEITRIM", "LIMERICK", "LONDON", "LONGFORD", "LOUTH", "MAYO", "MEATH", 
                           "MONAGHAN", "OFFALY", "ROSCOMMON", "SLIGO", "TIPPERARY", "TYRONE", 
                           "WATERFORD", "WESTMEATH", "WEXFORD", "WICKLOW"), 
                           team_neat=c("Antrim", "Armagh", "Carlow", "Cavan", "Clare", "Cork", "Derry", 
                                  "Donegal", "Down", "Dublin","Fermanagh", "Galway", "Kerry", "Kildare", "Kilkenny", 
                                  "Laois", "Leitrim", "Limerick", "London", "Longford", "Louth", "Mayo", "Meath", 
                                  "Monaghan", "Offaly", "Roscommon", "Sligo", "Tipperary", "Tyrone", 
                                  "Waterford", "Westmeath", "Wexford", "Wiclow"), 
                        team_colour1=c("yellow", "orange", "green", "blue", "yellow", "red", "red",
                                       "green", "red", "darkblue", "green", "darkred", "green", "white", "black",
                                       "blue", "green", "green", "white", "blue", "red", "green", "green",
                                       "white", "green", "blue", "black", "blue", "white",
                                       "blue", "darkred", "purple", "blue"),
                        team_colour2=c("white", "white", "red", "white", "blue", "red", "white",
                                       "gold", "black", "lightblue", "white", "white", "yellow", "white", "darkgoldenrod1",
                                       "white", "gold", "white", "green", "gold", "white", "red", "gold",
                                       "blue", "gold", "yellow", "white", "yellow", "red",
                                       "white", "white", "gold", "yellow"),
                        stringsAsFactors = F)

shinyServer(function(input, output) {
  
  #home_away_y <- input$home_away_y
  #home_away_x <- input$home_away_x
  home_away_x <- home_away_y <- "all"
  
  gaa_date <- reactive({
    gaa_data[which(gaa_data$Date>=input$season_range_c[1] & gaa_data$Date<=input$season_range_c[2]),]
  })
  
  plot_labels <- reactiveValues(ylab=NULL,
                                xlab=NULL,
                                main_y=NULL,
                                main_y_per=NULL,
                                main_x=NULL,
                                main_x_per=NULL,
                                ylab_ha = NULL,
                                xlab_ha = NULL)
  
  
  team_colours <- reactive({
    active_teams <- sort(unique(c(gaa_date()$Team_Name, gaa_date()$Opp_Name)))
    return(team_colours_data[which(team_colours_data$team %in% active_teams),])
  })
    # team= team_colours_data$team,
    #                              colour1 = team_colours_data$team_colour1,
    #                              colour2 = team_colours_data$team_colour2
  
  y_numerator <- reactive({
    active_teams <- sort(unique(c(gaa_date()$Team_Name, gaa_date()$Opp_Name)))
    output <- data.frame(team=active_teams, stat=0)
    if(input$stat_choice_y == "goals"){
      plot_labels$main_y <<- plot_labels$ylab <<- "Goals scored"
      score_type <- "Goals"
      team_a <- "Team"
      team_b <- "Opp"
    }else if(input$stat_choice_y == "goals_conc"){
      plot_labels$main_y <<- plot_labels$ylab <<- "Goals conceded"
      score_type <- "Goals"
      team_a <- "Opp"
      team_b <- "Team"
    }else if(input$stat_choice_y == "points"){
      plot_labels$main_y <<- plot_labels$ylab <<- "Points scored"
      score_type <- "Points"
      team_a <- "Team"
      team_b <- "Opp"
    }else if(input$stat_choice_y == "points_conc"){
      plot_labels$main_y <<- plot_labels$ylab <<- "Points conceded"
      score_type <- "Points"
      team_a <- "Opp"
      team_b <- "Team"
    }else if(input$stat_choice_y == "tot_score"){
      plot_labels$main_y <<- plot_labels$ylab <<- "Total scored"
      score_type <- "Score"
      team_a <- "Team"
      team_b <- "Opp"
    }else if(input$stat_choice_y == "tot_conc"){
      plot_labels$main_y <<- plot_labels$ylab <<- "Total conceded"
      score_type <- "Score"
      team_a <- "Opp"
      team_b <- "Team"
    }
    for(i in 1:nrow(output)){
      output[i,"stat"] <- switch(home_away_y,
                                 "all" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), paste(team_a,score_type, sep = "_")]) + 
                                   sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), paste(team_b,score_type, sep = "_")]),
                                 "home" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), paste(team_a,score_type, sep = "_")]),
                                 "away" = sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), paste(team_b,score_type, sep = "_")]))
    }
    return(output)
  })
  # y_numerator <- reactive({
  #   active_teams <- sort(unique(c(gaa_date()$Team_Name, gaa_date()$Opp_Name)))
  #   output <- data.frame(team=active_teams, stat=0)
  #   if(input$stat_choice_y == "goals"){
  #     plot_labels$main_y <<- plot_labels$ylab <<- "Goals scored"
  #     for(i in 1:nrow(output)){
  #       output[i,"stat"] <-  switch(home_away_y,
  #                                   "all" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Team_Goals"]) + 
  #                                     sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Opp_Goals"]),
  #                                   "home" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Team_Goals"]),
  #                                   "away" = sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Opp_Goals"]))
  #       
  #     }
  #   }else if(input$stat_choice_y == "goals_conc"){
  #     plot_labels$main_y <<- plot_labels$ylab <<- "Goals conceded"
  #     for(i in 1:nrow(output)){
  #         output[i,"stat"] <- switch(home_away_y,
  #                                    "all" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Opp_Goals"]) + 
  #                                      sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Team_Goals"]),
  #                                    "home" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Opp_Goals"]),
  #                                    "away" = sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Team_Goals"]))
  #     }
  #   }else if(input$stat_choice_y == "points"){
  #     plot_labels$main_y <<- plot_labels$ylab <<- "Points scored"
  #     for(i in 1:nrow(output)){
  #       output[i,"stat"] <- switch(home_away_y,
  #                                  "all" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Opp_Points"]) + 
  #                                    sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Team_Points"]),
  #                                  "home" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Opp_Points"]),
  #                                  "away" = sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Team_Points"]))
  #     }
  #   }else if(input$stat_choice_y == "points_conc"){
  #     plot_labels$main_y <<- plot_labels$ylab <<- "Points conceded"
  #     for(i in 1:nrow(output)){
  #       output[i,"stat"] <- switch(home_away_y,
  #                                  "all" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Opp_Points"]) + 
  #                                    sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Team_Goals"]),
  #                                  "home" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), "Opp_Points"]),
  #                                  "away" = sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), "Team_Points"]))
  #     }
  #   }
  #   return(output)
  # })
  
  y_denominator <- reactive({
    active_teams <- sort(unique(c(gaa_date()$Team_Name, gaa_date()$Opp_Name)))
    output <- data.frame(team=active_teams, stat=1)
    if(input$stat_choice_y_per == "no_div"){
      plot_labels$main_y_per <<- NULL
      return(output)
    }else if(input$stat_choice_y_per == "p_game"){
      plot_labels$main_y_per <<- "per game"
      for(i in 1:nrow(output)){
        output[i,"stat"] <-  switch(home_away_y,
                                    "all" = sum(gaa_date()$Team_Name==output[i,"team"]) + sum(gaa_date()$Opp_Name==output[i,"team"]),
                                    "home" = sum(gaa_date()$Team_Name==output[i,"team"]),
                                    "away" = sum(gaa_date()$Opp_Name==output[i,"team"]))
      }
    }else{
      if(input$stat_choice_y_per == "p_goal_score"){
        plot_labels$main_y_per <<- "per goal scored"
        score_type <- "Goals"
        team_a <- "Team"
        team_b <- "Opp"
      }else if(input$stat_choice_y_per == "p_goal_conc"){
        plot_labels$main_y_per <<- "per goal conceded"
        score_type <- "Goals"
        team_a <- "Opp"
        team_b <- "Team"
      }else if(input$stat_choice_y_per == "p_points"){
        plot_labels$main_y_per <<- "per point scored"
        score_type <- "Points"
        team_a <- "Team"
        team_b <- "Opp"
      }else if(input$stat_choice_y_per == "p_points_conc"){
        plot_labels$main_y_per <<- "per point conceded"
        score_type <- "Points"
        team_a <- "Opp"
        team_b <- "Team"
      }else if(input$stat_choice_y_per == "p_tot_score"){
        plot_labels$main_y_per <<- "per total scored"
        score_type <- "Score"
        team_a <- "Team"
        team_b <- "Opp"
      }else if(input$stat_choice_y_per == "p_tot_conc"){
        plot_labels$main_y_per <<- "per total conceded"
        score_type <- "Score"
        team_a <- "Opp"
        team_b <- "Team"
      }
      
      for(i in 1:nrow(output)){
        output[i,"stat"] <- switch(home_away_y,
                                   "all" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), paste(team_a,score_type, sep = "_")]) + 
                                     sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), paste(team_b,score_type, sep = "_")]),
                                   "home" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), paste(team_a,score_type, sep = "_")]),
                                   "away" = sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), paste(team_b,score_type, sep = "_")]))
      }
    }
    return(output)
  })
  
  
  
  x_numerator <- reactive({
    active_teams <- sort(unique(c(gaa_date()$Team_Name, gaa_date()$Opp_Name)))
    output <- data.frame(team=active_teams, stat=0)
    if(input$stat_choice_x == "goals"){
      plot_labels$main_x <<- plot_labels$xlab <<- "Goals scored"
      score_type <- "Goals"
      team_a <- "Team"
      team_b <- "Opp"
    }else if(input$stat_choice_x == "goals_conc"){
      plot_labels$main_x <<- plot_labels$xlab <<- "Goals conceded"
      score_type <- "Goals"
      team_a <- "Opp"
      team_b <- "Team"
    }else if(input$stat_choice_x == "points"){
      plot_labels$main_x <<- plot_labels$xlab <<- "Points scored"
      score_type <- "Points"
      team_a <- "Team"
      team_b <- "Opp"
    }else if(input$stat_choice_x == "points_conc"){
      plot_labels$main_x <<- plot_labels$xlab <<- "Points conceded"
      score_type <- "Points"
      team_a <- "Opp"
      team_b <- "Team"
    }else if(input$stat_choice_x == "tot_score"){
      plot_labels$main_x <<- plot_labels$xlab <<- "Total scored"
      score_type <- "Score"
      team_a <- "Team"
      team_b <- "Opp"
    }else if(input$stat_choice_x == "tot_conc"){
      plot_labels$main_x <<- plot_labels$xlab <<- "Total conceded"
      score_type <- "Score"
      team_a <- "Opp"
      team_b <- "Team"
    }
      for(i in 1:nrow(output)){
        output[i,"stat"] <- switch(home_away_x,
                                   "all" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), paste(team_a,score_type, sep = "_")]) + 
                                     sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), paste(team_b,score_type, sep = "_")]),
                                   "home" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), paste(team_a,score_type, sep = "_")]),
                                   "away" = sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), paste(team_b,score_type, sep = "_")]))
      }
    return(output)
  })
  
  x_denominator <- reactive({
    active_teams <- sort(unique(c(gaa_date()$Team_Name, gaa_date()$Opp_Name)))
    output <- data.frame(team=active_teams, stat=1)
    if(input$stat_choice_x_per == "no_div"){
      plot_labels$main_x_per <<- NULL
      return(output)
    }else if(input$stat_choice_x_per == "p_game"){
      plot_labels$main_x_per <<- "per game"
      for(i in 1:nrow(output)){
        output[i,"stat"] <-  switch(home_away_x,
                                    "all" = sum(gaa_date()$Team_Name==output[i,"team"]) + sum(gaa_date()$Opp_Name==output[i,"team"]),
                                    "home" = sum(gaa_date()$Team_Name==output[i,"team"]),
                                    "away" = sum(gaa_date()$Opp_Name==output[i,"team"]))
      }
    }else {
      if(input$stat_choice_x_per == "p_goal_score"){
        plot_labels$main_x_per <<- "per goal scored"
        score_type <- "Goals"
        team_a <- "Team"
        team_b <- "Opp"
      }else if(input$stat_choice_x_per == "p_goal_conc"){
        plot_labels$main_x_per <<- "per goal conceded"
        score_type <- "Goals"
        team_a <- "Opp"
        team_b <- "Team"
      }else if(input$stat_choice_x_per == "p_points"){
        plot_labels$main_x_per <<- "per point scored"
        score_type <- "Points"
        team_a <- "Team"
        team_b <- "Opp"
      }else if(input$stat_choice_x_per == "p_points_conc"){
        plot_labels$main_x_per <<- "per point conceded"
        score_type <- "Points"
        team_a <- "Opp"
        team_b <- "Team"
      }else if(input$stat_choice_x_per == "p_tot_score"){
        plot_labels$main_x_per <<- "per total scored"
        score_type <- "Score"
        team_a <- "Team"
        team_b <- "Opp"
      }else if(input$stat_choice_x_per == "p_tot_conc"){
        plot_labels$main_x_per <<- "per total conceded"
        score_type <- "Score"
        team_a <- "Opp"
        team_b <- "Team"
      }

      for(i in 1:nrow(output)){
        output[i,"stat"] <- switch(home_away_x,
                                   "all" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), paste(team_a,score_type, sep = "_")]) + 
                                     sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), paste(team_b,score_type, sep = "_")]),
                                   "home" = sum(gaa_date()[which(gaa_date()$Team_Name==output[i,"team"]), paste(team_a,score_type, sep = "_")]),
                                   "away" = sum(gaa_date()[which(gaa_date()$Opp_Name==output[i,"team"]), paste(team_b,score_type, sep = "_")]))
      }
    }
    return(output)
  })
  
  
  
  
  
  
  output$plot_stats_custom <- renderPlot({

    y_dat <- y_numerator()$stat/y_denominator()$stat
    x_dat <- x_numerator()$stat/x_denominator()$stat
    
    if(input$season_range_c[1] >= "2017-01-01"){  clab2 <- "this season"
    }else{  clab2 <- paste("between",format(input$season_range_c[1],"%d %b '%y"),"and",format(input$season_range_c[2],"%d %b '%y")) }
    
    plot_labels$y_lab_ha <<- switch(home_away_y,
                        "all" = NULL,
                        "home" = "(home)",
                        "away" = "(away)")
    
    plot_labels$y_lab_ha <<- switch(home_away_x,
                        "all" = NULL,
                        "home" = "(home)",
                        "away" = "(away)")
    
    
    # if(input$custom_boundaries == TRUE){
    #   mymax <- max(c(x_dat,y_dat))
    #   mymin <- min(c(x_dat,y_dat))
    #   my_xlim = c(mymin, mymax)# + (abs(range(x_dat)[1] - range(x_dat)[2])/10))
    #   my_ylim = c(mymin, mymax)
    # }else{
    #   my_xlim = c(min(x_dat),max(x_dat) +  (abs(range(x_dat)[1] - range(x_dat)[2])/10))
    #   my_ylim = range(y_dat)
    # }
    
    #labels for click data
    click_xlab <<- paste0(plot_labels$xlab," ",plot_labels$main_x_per)
    click_ylab <<- paste0(plot_labels$ylab," ",plot_labels$main_y_per)
    
    #plot_data_cx <- round(x_dat/plot_data_cx2, digits=4)
    click_xdata <<- x_dat
    #plot_data_cy <- round(y_dat/plot_data_cy2, digits=4)
    click_ydata <<- y_dat
    
    #par(bg="grey")
    #plot(y_numerator()$stat/y_denominator()$stat, x_numerator()$stat/x_denominator()$stat)
    plot(x_dat, y_dat,
         #main = paste(cylab,per_cylab,"vs.",cxlab,per_cxlab,clab2),
         ylab = paste(plot_labels$ylab, plot_labels$main_y_per, plot_labels$y_lab_ha), xlab=paste(plot_labels$xlab, plot_labels$main_x_per, plot_labels$x_lab_ha),
         main = paste(plot_labels$main_y, plot_labels$main_y_per,"vs.", plot_labels$main_x, plot_labels$main_x_per, plot_labels$clab2),
         pch = 19, cex=1.4, col = team_colours()$team_colour1)
         #xlim = my_xlim, ylim = my_ylim)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "light grey")
    
    points(x_dat, y_dat,
           pch = 19, cex=1.4, col = team_colours()$team_colour1
    )
    
    points(x_dat, y_dat,
           pch = 4, col = team_colours()$team_colour2, lwd=1.2
    )
    text(x_dat, y_dat, team_colours()$team_neat, pos=4)
  })
  
  observe({
    if(!is.null(input$custom_plot_click$x)&&!is.null(input$custom_plot_click$y)&&!is.null(click_xdata)){
      selected <- c(input$custom_plot_click$x,input$custom_plot_click$y)
      closest_team <- which.min(
        sqrt(apply((cbind(click_xdata, y_numerator()$stat/y_denominator()$stat)- matrix(selected,nrow=length(click_xdata),byrow=T,ncol=2))^2,1,sum))
      )
      output$info_cus <- renderText({
        paste0(team_colours()$team_neat[closest_team],":\ ",click_xlab,": ", round(click_xdata[closest_team],2),",\ ",click_ylab,": ", round(click_ydata[closest_team],2))
      })
    }else{
      output$info_cus <- renderText({
        paste0("Click on graph to see exact values.")
      })
    }
  })
  
  
  })






  
  
  
  
  
  
  

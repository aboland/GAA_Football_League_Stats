library(shiny)
library(ggvis)


# Define UI for application to display data
shinyUI(fluidPage(#theme="bootstrap.css",
  
  # Application title
  titlePanel("GAA Football League"),
  
  
  # Sidebar ---------------------------------------------------------------------------------------
      navlistPanel(widths =c(2, 10),
                   
                   tabPanel("League Statistics",
                            # tabsetPanel(
                              # tabPanel("Statistics",
                                       fluidRow(
                                         column(3,
                                                selectInput("stat_choice_y", label = h4("Stat 1"), 
                                                            choices = list("Goals scored" = "goals",
                                                                           "Goals conceded" = "goals_conc",
                                                                           "Points scored" = "Team_Points",
                                                                           "Points conceded" = "Opp_Points"), selected = "goals"),
                                                selectInput("stat_choice_y_per", label = NULL, 
                                                            choices = list("By" = "no_div",
                                                                           "Per game" = "p_game",
                                                                           "Per goal scored" = "p_goal_score",
                                                                           "Per goal conceded" = "p_goal_conc",
                                                                           "At home" = "p_home",
                                                                           "Away" = "p_away"), selected = "no_div")),
                                         column(3,
                                                selectInput("stat_choice_x", label = h4("Stat 2"), 
                                                            choices = list("Goals scored" = "goals",
                                                                           "Goals conceded" = "goals_conc",
                                                                           "Points scored" = "Team_Points",
                                                                           "Points conceded" = "Opp_Points"), selected = "goals_conc"),
                                                selectInput("stat_choice_x_per", label = NULL, 
                                                            choices = list("By" = "no_div",
                                                                           "Per game" = "p_game",
                                                                           "Per season" = "p_goal",
                                                                           "Per goal conceded" = "p_goal_conc",
                                                                           "At home" = "p_home",
                                                                           "Away" = "p_away"), selected = "no_div")),
                                         column(4,
                                                #h4("Time range"),
                                                #checkboxInput("this_season", label = "All seasons", value = FALSE),
                                                #sliderInput("season_range", label= h4("Season"),
                                                #            min = 2000, max = 2016, value = c(2015, 2016),step=1, sep=""),
                                                dateRangeInput("season_range_c", label= h4("Date range"),
                                                               format = "dd-mm-yyyy", 
                                                               start = "2003-02-01", 
                                                               end = "2013-03-31",
                                                               #end = Sys.Date()-1, 
                                                               min="2003-02-01"),
                                                checkboxInput("custom_boundaries","Fixed aspect", value = FALSE)
                                         )),
                                       plotOutput("plot_stats_custom", click = "custom_plot_click"),# , height="auto", width = "100%"),
                                       #plotOutput("plot_stats"),
                                       textOutput("info_cus")#,
                                       #ggvisOutput("myggplot")
                              ),
                              tabPanel("Head to head"#,
                                       # fluidRow(
                                       #   column(2,
                                       #          selectInput("hh_stat_choice", label = h4("Statistic"), 
                                       #                      choices = list("Goals scored" = "goals",
                                       #                                     "Goals conceded" = "goals_conc",
                                       #                                     "Shots on target" = "starget",
                                       #                                     "Shots" = "shots",
                                       #                                     #"Goals per shot on target" = "gperst",
                                       #                                     #"Goals per shot" = "gpers",
                                       #                                     "Corners" = "corners",
                                       #                                     "Fouls" = "fouls",
                                       #                                     "Yellow cards" = "ycard",
                                       #                                     "Red cards" = "rcard",
                                       #                                     "Goals by halftime" = "halfgoals"), selected = "goals")),
                                       #   column(3,
                                       #          uiOutput("hh_teamA"),
                                       #          uiOutput("hh_teamB")
                                       #   ),
                                       #   #column(3,
                                       #   #       uiOutput("hh_teamB")
                                       #   #),
                                       #   column(4,
                                       #          #h4("Time range"),
                                       #          #checkboxInput("this_season", label = "All seasons", value = FALSE),
                                       #          #sliderInput("season_range", label= h4("Season"),
                                       #          #            min = 2000, max = 2016, value = c(2015, 2016),step=1, sep=""),
                                       #          dateRangeInput("hh_season_range", label= h4("Date range"),
                                       #                         format = "dd-mm-yyyy", start = "2016-08-13", end = Sys.Date()-1, min="2000-08-09"),
                                       #          checkboxInput("cumul_sum","Cumulative sum", value = TRUE)
                                       #   )
                                       #   ),
                                       # plotOutput("plot_hh")
                                       
                              )
                            
                   )
                   # ),
                   # 
                   # tabPanel("Player Data",
                   #          uiOutput("dt_field_choices"),
                   #          DT::dataTableOutput("dt_data_display")
                   #          
                   # )#,

))

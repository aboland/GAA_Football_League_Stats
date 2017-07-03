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
                                                # radioButtons("home_away_y",NULL,
                                                #              choices = list("all"="all",
                                                #                             "home"="home",
                                                #                             "away"="away")),
                                                # selectInput("home_away_y", label = h4("Stat 1"), 
                                                #             choices = list("All"="all",
                                                #                            "Home"="home",
                                                #                            "Away"="away"), selected = "all"),
                                                selectInput("stat_choice_y", label = h4("Stat 1"), 
                                                            choices = list("Goals scored" = "goals",
                                                                           "Goals conceded" = "goals_conc",
                                                                           "Points scored" = "points",
                                                                           "Points conceded" = "points_conc",
                                                                           "Total scored" = "tot_score",
                                                                           "Total conceded" = "tot_conc"), selected = "goals"),
                                                selectInput("stat_choice_y_per", label = NULL, 
                                                            choices = list("By" = "no_div",
                                                                           "Per game" = "p_game",
                                                                           "Per goal scored" = "p_goal_score",
                                                                           "Per goal conceded" = "p_goal_conc",
                                                                           "Per points scored" = "p_points",
                                                                           "Per points conceded" = "p_points_conc",
                                                                           "Per total scored" = "p_tot_score",
                                                                           "Per total conceded" = "p_tot_conc"#,
                                                                           # "At home" = "p_home",
                                                                           # "Away" = "p_away"
                                                                           ), selected = "no_div")),
                                         column(3,
                                                # selectInput("home_away_x", label = h4("Stat 2"),
                                                #             choices = list("All"="all",
                                                #                            "Home"="home",
                                                #                            "Away"="away"), selected = "all"),
                                                selectInput("stat_choice_x", label = h4("Stat 2"), 
                                                            choices = list("Goals scored" = "goals",
                                                                           "Goals conceded" = "goals_conc",
                                                                           "Points scored" = "points",
                                                                           "Points conceded" = "points_conc",
                                                                           "Total scored" = "tot_score",
                                                                           "Total conceded" = "tot_conc"), selected = "goals_conc"),
                                                selectInput("stat_choice_x_per", label = NULL, 
                                                            choices = list("By" = "no_div",
                                                                           "Per game" = "p_game",
                                                                           "Per goal scored" = "p_goal_score",
                                                                           "Per goal conceded" = "p_goal_conc",
                                                                           "Per points scored" = "p_points",
                                                                           "Per points conceded" = "p_points_conc",
                                                                           "Per total scored" = "p_tot_score",
                                                                           "Per total conceded" = "p_tot_conc"#,
                                                                           # "At home" = "p_home",
                                                                           # "Away" = "p_away"
                                                                           ), selected = "no_div")),
                                         column(4,
                                                #h4("Time range"),
                                                #checkboxInput("this_season", label = "All seasons", value = FALSE),
                                                #sliderInput("season_range", label= h4("Season"),
                                                #            min = 2000, max = 2016, value = c(2015, 2016),step=1, sep=""),
                                                dateRangeInput("season_range_c", label= h4("Date range"),
                                                               format = "dd-mm-yyyy", 
                                                               start = "2017-01-01", 
                                                               end = "2017-03-19",
                                                               #end = Sys.Date()-1, 
                                                               min="2001-02-10"),
                                                #checkboxInput("custom_boundaries","Fixed aspect", value = FALSE),
                                                selectInput("select_division", label = NULL, 
                                                            choices = list("Division" = "all",
                                                                           "Division 1"="1",
                                                                           "Division 2"="2",
                                                                           "Division 3"="3",
                                                                           "Division 4"="4"
                                                            ), selected = "all")
                                         )),
                                       plotOutput("plot_stats_custom", click = "custom_plot_click"),# , height="auto", width = "100%"),
                                       #plotOutput("plot_stats"),
                                       textOutput("info_cus")#,
                                       #ggvisOutput("myggplot")
                              ),
                              tabPanel("Head to head",
                                       helpText("Under construction")
                              )
                            
                   )

))

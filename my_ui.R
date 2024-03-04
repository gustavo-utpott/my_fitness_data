# script for the ui

# cabeçalho
header <- dashboardHeader( 
  title = "My Fitness Data",
  titleWidth = 400
)

# aba do lado
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Activities", tabName = "actv", icon = icon("person-running")),
    menuItem("Heart Rate", tabName = "heart", icon = icon("heart-pulse")),
    menuItem("Sleep", tabName = "sleep", icon = icon("bed")),
    menuItem("Diet", tabName = "diet", icon = icon("apple-whole"))
  ),
  width = 180
)

# corpo
body <- dashboardBody(
  tabItems(
    tabItem(
      "actv",
      fluidPage(
        fluidRow(
          valueBoxOutput("box_steps_1", width = 3),
          valueBoxOutput("box_steps_2", width = 3),
          valueBoxOutput("box_steps_3", width = 3),
          valueBoxOutput("box_active_time", width = 3),
          dateRangeInput(
            "date_range",
            "Date Range",
            start = min_date,
            end = max_date,
            min = min_date,
            max = max_date,
            language = "en"#pt-BR"
          ),
          tabBox(
            title = "Steps data",
            id = "steps", height = "450px", width = 12,
            tabPanel("Daily", plotlyOutput("chart_steps_1")),
            tabPanel("Weekly", plotlyOutput("chart_steps_2")),
            tabPanel("Monthly", plotlyOutput("chart_steps_3"))
          ),
          tabBox(
            title = "Active time data",
            id = "active_time", height = "450px", width = 12,
            tabPanel("Daily", plotlyOutput("chart_act_time_1")),
            tabPanel("Weekly", plotlyOutput("chart_act_time_2")),
            tabPanel("Monthly", plotlyOutput("chart_act_time_3"))
          )
        )
      )
    ), 
    tabItem(
      "heart",
      fluidPage(
        fluidRow(
          valueBoxOutput("box_heart_1", width = 3),
          valueBoxOutput("box_heart_2", width = 3),
          valueBoxOutput("box_heart_3", width = 3),
          valueBoxOutput("box_heart_4", width = 3),
          checkboxGroupInput(
            "wearable", 
            label = h3("Data from which wearable"), 
            choices = list("galaxy fit 2", "galaxy watch 4"),
            selected = "galaxy watch 4"
          ),
          tabBox(
            title = "Resting Heart rate",
            id = "resting_heart_rate", height = "450px", width = 12,
            tabPanel("Daily", plotlyOutput("chart_rhr_1")),
            tabPanel("Weekly", plotlyOutput("chart_rhr_2")),
            tabPanel("Monthly", plotlyOutput("chart_rhr_3"))
          ),
          tabBox(
            title = "Max Heart rate",
            id = "max_heart_rate", height = "450px", width = 12,
            tabPanel("Daily", plotlyOutput("chart_mhr_1")),
            tabPanel("Weekly", plotlyOutput("chart_mhr_2")),
            tabPanel("Monthly", plotlyOutput("chart_mhr_3"))
          ),
          tabBox(
            title = "Mean Heart Rate",
            id = "mean_heart_rate", height = "450px", width = 12,
            tabPanel("Daily", plotlyOutput("chart_mean_hr_1")),
            tabPanel("Weekly", plotlyOutput("chart_mean_hr_2")),
            tabPanel("Monthly", plotlyOutput("chart_mean_hr_3"))
          )
        )
      )
    ),
    tabItem(
      "sleep",
      fluidPage(
        fluidRow(
          valueBoxOutput("box_sleep_1", width = 3),
          valueBoxOutput("box_sleep_2", width = 3),
          valueBoxOutput("box_sleep_3", width = 3),
          valueBoxOutput("box_sleep_4", width = 3),
          dateRangeInput(
            "date_range2",
            "Date Range",
            start = min_date_sleep,
            end = max_date_sleep,
            min = min_date_sleep,
            max = max_date_sleep,
            language = "en"#pt-BR"
          ),
          tabBox(
            title = "Time Slept",
            id = "total_sleep", height = "450px", width = 12,
            tabPanel("Daily", plotlyOutput("chart_total_sleep_1")),
            tabPanel("Weekly", plotlyOutput("chart_total_sleep_2")),
            tabPanel("Monthly", plotlyOutput("chart_total_sleep_3"))
          ),
          tabBox(
            title = "Sleep Phases",
            id = "sleep_phases", height = "450px", width = 12,
            tabPanel("Daily", plotlyOutput("chart_sleep_phases_1")),
            tabPanel("Weekly", plotlyOutput("chart_sleep_phases_2")),
            tabPanel("Monthly", plotlyOutput("chart_sleep_phases_3"))
          ),
          tabBox(
            title = "Bedtime and Wake Up Time",
            id = "bedtime_and_w", height = "450px", width = 12,
            tabPanel("Daily", plotlyOutput("chart_bed_wake_1")),
            tabPanel("Weekly", plotlyOutput("chart_bed_wake_2")),
            tabPanel("Monthly", plotlyOutput("chart_bed_wake_3"))
          )
        )
      )
    )
  )
)


ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)
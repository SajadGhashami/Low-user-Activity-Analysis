#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(waiter)
library(shinycssloaders)
library(DT)
library(DBI)
library(tidyverse)
library(tidyr)
library(ggthemes)

# Global option
options(shiny.maxRequestSize=3000*1024^2)

rsconnect::setAccountInfo(name='xxx', token='xxxx', secret='xxxx')

con <- DBI::dbConnect(odbc::odbc(),
                      #Snowflake
                      #SnowflakeDSIIDriver
                      Driver       = "SnowflakeDSIIDriver",
                      Server       = "ed87949.us-east-1.snowflakecomputing.com",
                      UID          = rstudioapi::askForPassword("Database user"),
                      PWD          = rstudioapi::askForPassword("Database password"),
                      Database     = "EDW",
                      Warehouse    = "COMPUTE_LARGE",
                      Schema       = "dim"
                      #,
                      #authenticator = "externalbrowser"
)
mywh <- DBI::dbSendQuery(con, 'use role analyst_role')
#mywh <- DBI::dbSendQuery(con, 'use role developer_role')
mywh <- DBI::dbSendQuery(con, 'use warehouse COMPUTE_LARGE')

# Random seed
set.seed(27)

trythequery <- tryCatch(DBI::dbGetQuery(con,
                                        "SELECT *
                                            FROM analytics.sandbox.teacher_daily_activity_63_days_in_sy_pulled_20220902
                                            limit 5"),
                        error=function(e) NULL)

filter_BUY_ACCOUNT_NAME <- tryCatch(DBI::dbGetQuery(con,
                                                   "SELECT DISTINCT BUY_ACCOUNT_NAME
                                            FROM analytics.sandbox.teacher_daily_activity_63_days_in_sy_pulled_20220902
                                            "),
                                   error=function(e) NULL)

filter_TCH_LICENSE_COMBO <- tryCatch(DBI::dbGetQuery(con,
                                                    "SELECT DISTINCT TCH_LICENSE_COMBO
                                            FROM analytics.sandbox.teacher_daily_activity_63_days_in_sy_pulled_20220902
                                            "),
                                    error=function(e) NULL)

# filter_SCH_NAME <- tryCatch(DBI::dbGetQuery(con,
#                                                     "SELECT DISTINCT SCH_NAME
#                                             FROM analytics.sandbox.teacher_daily_activity_63_days_in_sy_pulled_20220902
#                                             "),
#                                     error=function(e) NULL)

excludecols <- c("DATE", "BUY_ACCOUNT_ID", "SY_LABEL", "BUY_DATE_SCHOOL_STARTS", "TCH_USERID", "TCH_DATE_LAST_LOGIN")

analyzecols <- trythequery %>% names()
analyzecols <- analyzecols[! analyzecols %in% excludecols]
# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "High/Low Activity Analysis", titleWidth = 200),
  dashboardSidebar( width = 200,
                    sidebarMenu(
                      menuItem("Cohort Analysis", tabName = "cohort", icon = icon("table")),
                      
                      selectInput(
                        'firstsy',
                        'Choose initial SY',
                        choices= c('SY:2018 - 2019','SY:2019 - 2020','SY:2020 - 2021','SY:2021 - 2022','SY:2022 - 2023'),
                        selected = 'SY:2021 - 2022',
                        multiple = FALSE,
                        selectize = TRUE,
                        width = NULL,
                        size = NULL
                      ),
                      selectInput(
                        'secondsy',
                        'Choose final SY',
                        choices= c('SY:2018 - 2019','SY:2019 - 2020','SY:2020 - 2021','SY:2021 - 2022','SY:2022 - 2023'),
                        selected = 'SY:2022 - 2023',
                        multiple = FALSE,
                        selectize = TRUE,
                        width = NULL,
                        size = NULL
                      ),
                      selectInput(
                        'sy_active',
                        'Teachers are Active in SY?',
                        choices= c("", 'TRUE','FALSE'),
                        selected = TRUE,
                        multiple = FALSE,
                        selectize = FALSE,
                        width = NULL,
                        size = NULL
                             ),
                      selectInput(
                        'TCH_IS_TARGET', 
                        'Is Target Teacher',
                        choices= c("", '1','0'),
                        selected = '1',
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'TCH_IS_FULL_PAID', 
                        'Is FULL PAID Teacher',
                        choices= c("", '1','0'),
                        selected = '1',
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      dateRangeInput("daterange", "Date range:",
                                     start  = "2018-07-01",
                                     end    = "2024-06-30",
                                     min    = "2018-07-01",
                                     max    = "2024-07-21",
                                     format = "yy/mm/dd",
                                     separator = "-"),
                      dateRangeInput("TCH_DATE_LAST_LOGIN_range", "TCH DATE LAST LOGIN range:",
                                     start  = "2018-07-01",
                                     end    = "2024-06-30",
                                     min    = "2018-07-01",
                                     max    = "2024-07-21",
                                     format = "yy/mm/dd",
                                     separator = "-"),
                      selectizeInput('BUY_ACCOUNT_NAME',
                                     'BUY ACCOUNT NAME',
                                     choices = c("", filter_BUY_ACCOUNT_NAME),
                                     selected = NULL,
                                     multiple = TRUE,
                                     options = NULL),
                      selectInput(
                        'BUY_RECORD_TYPE',
                        'BUY RECORD TYPE',
                        choices= c("", 'Partner','Network', 'School'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'BUY_SEGMENT',
                        'BUY SEGMENT',
                        choices= c("", 'SMB Proactive','SMB Reactive', 'Mid-Market','Enterprise (Strategic)', 'Enterprise (Non-Strategic)'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'BUY_REGION',
                        'BUY REGION',
                        choices= c("", 'West Region','Missing State/International', 'Central Region','East Region'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'BUY_DATE_SCHOOL_STARTS', 
                        'BUY DATE SCHOOL STARTS',
                        choices= c("", expand.grid(paste0(1:12, sep = "-"), 1:31, stringsAsFactors = FALSE) %>% transmute(comb= paste0(Var1,Var2)) %>% arrange(comb)  %>% pull(comb)  ),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'BUY_HAS_PL_SY', 
                        'BUY HAS PL SY',
                        choices= c("", "0", "1"  ),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'BUY_TOT_PL_SY', 
                        'BUY TOT PL SY',
                        choices= c("", "0", "1"  ),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'SCH_CRT', 
                        'School CRT',
                        choices= c("", 'CRT Banned','Possible Ban In Progress','No Restriction','unknown'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      
                      selectInput(
                        'TCH_LICENSE_TYPE', 
                        'Teacher License Type',
                        choices= c("", 'trial','subscription','Free'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'TCH_LICENSE_TYPE', 
                        'Teacher License Type',
                        choices= c("", 'trial','subscription','Free'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'TCH_LICENSE_TYPE', 
                        'Teacher License Type',
                        choices= c("", filter_TCH_LICENSE_COMBO),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'TCH_IS_K2', 
                        'IS K2 Teacher',
                        choices= c("", '1','0'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'TCH_IS_ELEM', 
                        'IS Elementary Teacher',
                        choices= c("", '1','0'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'TCH_IS_MS', 
                        'IS MS Teacher',
                        choices= c("", '1','0'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'TCH_IS_HS', 
                        'IS HS Teacher',
                        choices= c("", '1','0'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'TCH_SUB_GRADE_BAND', 
                        'Subject Grade Band',
                        choices= c("", 'Middle School - Science Only Target Subject','Elementary - Multi-Target Subject',
                                   'Multi-Grade - SS Only Target Subject', 'High School - Multi-Target Subject',
                                   'Non Target Teacher', 'High School - Social Studies Only Target Subject',
                                   'High School - ELA Only Target Subject', 'Middle School - Multi-Target Subject',
                                   'Multi-Grade - Multi-Target Subject', 'Middle School - Social Studies Only Target Subject',
                                   'Elementary - Social Studies Only Target Subject', 'Middle School - ELA Only Target Subject',
                                   'Multi-Grade - ELA Only Target Subject', 'Elementary - ELA Only Target Subject', 'Elementary - Science Only Target Subject',
                                   'Multi-Grade - Science Only Target Subject'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'TCH_EVER_ACTIVE_SY', 
                        'Ever Active SY Teacher',
                        choices= c("", '1','0'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      ),
                      selectInput(
                        'TCH_ACTIVE_LAST_SY', 
                        'Active Last SY Teacher',
                        choices= c("", '1','0'),
                        selected = NULL,
                        multiple = TRUE,
                        selectize = TRUE,
                        size = NULL
                      )
                      
                    )
  ),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  dashboardBody(
    
    tabItems(
      # First tab content
      
      tabItem(tabName = "cohort",
              box(width=12, title = "Analysis Variables:",
                  status = "warning",
                  solidHeader = TRUE,
                  "Please fill the inputs or leave it as it is and then press Run", 
              selectInput(
                'groupcolumns',
                'Columns to be analyzed',
                choices= analyzecols,
                selected = c('SCH_STATE', 'SCH_CRT'),
                multiple = TRUE,
                selectize = TRUE,
                width = '100%',
                size = NULL
              ),
              sliderInput("DaySY",
                          "Last DAY in SY",
                          1, 365, value = c(1,60)),
              column(12,
               
                     
              
              column(5,   actionButton("run", "Run the Analysis", icon("play"), width = '100%',  class = "btn-primary btn-lg",
                                       style="color: #0A6EFA; text-align:center; background-color: #E7F1FF; border-color: #2e6da4;  font-size:150%"))
              )),
              box(width=12, title = "Now you can alter your results:",
                  status = "success",
                  solidHeader = TRUE,
                  "Once you are sure about your variables(Chosen above) you can freely change following filters",
                  column(12,
                         column(2,
                                numericInput(
                                  'n',
                                  'N (Number of records)',
                                  5,
                                  min = 1,
                                  max = 100,
                                  step = NA,
                                  width = NULL
                                )),       
                         
                  column(2,
                         radioButtons("topdown", "Top",
                                      c("Growing" = "g",
                                        "Declining" = "d"),
                                      inline = TRUE)),
                  
                  column(2,
                  radioButtons("status", 
                               "What to do with one-year graphs",
                               c("Keep" = "1",
                                 "Exclude" = "2"),
                               inline = TRUE)),
                  
                  column(4,
                  selectInput(
                    'drop.na.columns',
                    'Drop records without values from these columns',
                    choices= analyzecols,
                    selected = NULL,
                    multiple = TRUE,
                    selectize = TRUE,
                    size = NULL)
                  )
                  )
              ),
              tabBox(
                side = "left", height = "2500px",
                width = 12,
                selected = "Graph Output",
                tabPanel("Graph Output", withSpinner(plotOutput("diffchart"))),
                tabPanel("Table Output", withSpinner(dataTableOutput("aggdata")))
                #,tabPanel("Table", withSpinner(dataTableOutput("querydata") ))
                ,tabPanel("sql", withSpinner(verbatimTextOutput("test") ))
              )
             
      )
      
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  

  
aggregatedtable <- eventReactive(input$run, {
    id <- showNotification("Aggregating Data", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    sql_select <- paste0(input$groupcolumns,collapse = ", ")
    sql_BUY_ACCOUNT_NAME_where <- paste0(input$BUY_ACCOUNT_NAME, collapse = "','")
    sql_BUY_RECORD_TYPE_where <- paste0(input$BUY_RECORD_TYPE, collapse = "','")
    sql_BUY_SEGMENT_where <- paste0(input$BUY_SEGMENT, collapse = "','")
    sql_BUY_REGION_where <- paste0(input$BUY_REGION, collapse = "','")
    sql_BUY_DATE_SCHOOL_STARTS_where <- paste0(input$BUY_DATE_SCHOOL_STARTS, collapse = "','")
    sql_BUY_HAS_PL_SY_where <- paste0(input$BUY_HAS_PL_SY, collapse = "','")
    sql_BUY_TOT_PL_SY_where <- paste0(input$BUY_TOT_PL_SY, collapse = "','")
    sql_SCH_CRT_where <- paste0(input$SCH_CRT, collapse = "','")
    sql_TCH_IS_TARGET_where <- paste0(input$TCH_IS_TARGET, collapse = "','")
    sql_TCH_LICENSE_TYPE_where <- paste0(input$TCH_LICENSE_TYPE, collapse = "','")
    sql_TCH_LICENSE_COMBO_where <- paste0(input$TCH_LICENSE_COMBO, collapse = "','")
    sql_TCH_IS_K2_where <- paste0(input$TCH_IS_K2, collapse = "','")
    sql_TCH_IS_ELEM_where <- paste0(input$TCH_IS_ELEM, collapse = "','")
    sql_TCH_IS_MS_where <- paste0(input$TCH_IS_MS, collapse = "','")
    sql_TCH_IS_HS_where <- paste0(input$TCH_IS_HS, collapse = "','")
    sql_TCH_SUB_GRADE_BAND_where <- paste0(input$TCH_SUB_GRADE_BAND, collapse = "','")
    sql_TCH_EVER_ACTIVE_SY_where <- paste0(input$TCH_EVER_ACTIVE_SY, collapse = "','")
    sql_TCH_ACTIVE_LAST_SY_where <- paste0(input$TCH_ACTIVE_LAST_SY, collapse = "','")
    sql_TCH_IS_FULL_PAID_where <- paste0(input$TCH_IS_FULL_PAID, collapse = "','")
    
    trythequery <- tryCatch(DBI::dbGetQuery(con,
                                            paste0("SELECT ",
                                                  sql_select,
                                                  ", SY_LABEL",
                                                  ", COUNT(DISTINCT TCH_USERID) AS distinct_teachers ",
                                                  ", distinct_teachers - lag(distinct_teachers, 1)  over
                                                  (partition by ", sql_select,
                                                  " order by SY_LABEL) as diff_to_prev",
                                                  " FROM analytics.sandbox.teacher_daily_activity_63_days_in_sy_pulled_20220902 ",
                                                  "WHERE SY_LABEL IN (","'", input$firstsy,"'",
                                                  ",", "'", input$secondsy, "'", ") ",
                                                  
                                                 if_else(length(input$BUY_ACCOUNT_NAME)==0 , " ",
                                                         paste0(" AND ", " BUY_ACCOUNT_NAME IN (","'", sql_BUY_ACCOUNT_NAME_where, "'", ") ")
                                                  ),
                                                 if_else(length(input$BUY_RECORD_TYPE)==0 , " ",
                                                         paste0(" AND ", " BUY_RECORD_TYPE IN (","'", sql_BUY_RECORD_TYPE_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$BUY_SEGMENT)==0 , " ",
                                                         paste0(" AND ", " BUY_SEGMENT IN (","'", sql_BUY_SEGMENT_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$BUY_REGION)==0 , " ",
                                                         paste0(" AND ", " BUY_REGION IN (","'", sql_BUY_REGION_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$BUY_DATE_SCHOOL_STARTS)==0 , " ",
                                                         paste0(" AND ", " BUY_DATE_SCHOOL_STARTS IN (","'", sql_BUY_DATE_SCHOOL_STARTS_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$BUY_HAS_PL_SY)==0 , " ",
                                                         paste0(" AND ", " BUY_HAS_PL_SY IN (","'", sql_BUY_HAS_PL_SY_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$BUY_TOT_PL_SY)==0 , " ",
                                                         paste0(" AND ", " BUY_TOT_PL_SY IN (","'", sql_BUY_TOT_PL_SY_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$SCH_CRT)==0 , " ",
                                                         paste0(" AND ", " SCH_CRT IN (","'", sql_SCH_CRT_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_IS_TARGET)==0 , " ",
                                                         paste0(" AND ", " TCH_IS_TARGET IN (","'", sql_TCH_IS_TARGET_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_LICENSE_TYPE)==0 , " ",
                                                         paste0(" AND ", " TCH_LICENSE_TYPE IN (","'", sql_TCH_LICENSE_TYPE_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_LICENSE_COMBO)==0 , " ",
                                                         paste0(" AND ", " TCH_LICENSE_COMBO IN (","'", sql_TCH_LICENSE_COMBO_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_IS_K2)==0 , " ",
                                                         paste0(" AND ", " TCH_IS_K2 IN (","'", sql_TCH_IS_K2_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_IS_ELEM)==0 , " ",
                                                         paste0(" AND ", " TCH_IS_ELEM IN (","'", sql_TCH_IS_ELEM_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_IS_MS)==0 , " ",
                                                         paste0(" AND ", " TCH_IS_MS IN (","'", sql_TCH_IS_MS_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_IS_HS)==0 , " ",
                                                         paste0(" AND ", " TCH_IS_HS IN (","'", sql_TCH_IS_HS_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_SUB_GRADE_BAND)==0 , " ",
                                                         paste0(" AND ", " TCH_SUB_GRADE_BAND IN (","'", sql_TCH_SUB_GRADE_BAND_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_EVER_ACTIVE_SY)==0 , " ",
                                                         paste0(" AND ", " TCH_EVER_ACTIVE_SY IN (","'", sql_TCH_EVER_ACTIVE_SY_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_ACTIVE_LAST_SY)==0 , " ",
                                                         paste0(" AND ", " TCH_ACTIVE_LAST_SY IN (","'", sql_TCH_ACTIVE_LAST_SY_where, "'", ") ")
                                                 ),
                                                 if_else(length(input$TCH_IS_FULL_PAID)==0 , " ",
                                                         paste0(" AND ", " TCH_IS_FULL_PAID IN (","'", sql_TCH_IS_FULL_PAID_where, "'", ") ")
                                                 ),
                                                  " AND DAY_IN_SY >=", input$DaySY[1],
                                                  " AND DAY_IN_SY <=", input$DaySY[2],
                                                  " AND DATE >= ", "'", input$daterange[1], "'",
                                                  " AND DATE <= ", "'", input$daterange[2], "'",
                                                 " AND DATE >= ", "'", input$TCH_DATE_LAST_LOGIN_range[1], "'",
                                                 " AND DATE <= ", "'", input$TCH_DATE_LAST_LOGIN_range[2], "'",
                                                 
                                                  if_else(input$sy_active=="", " ",
                                                          paste0(" AND TCH_IS_SY_ACTIVE =", input$sy_active)
                                                  ),
                                                   " GROUP BY ",
                                                  sql_select,
                                                 ", SY_LABEL"
                                                 #,
                                                  #" limit ",input$n,"
                                                 ,
                                                 collapse = "")
                                            ),
                            error=function(e) NULL)
    trythequery
    # trythequery    %>%
    #   filter(	SY_LABEL %in% c(input$firstsy, input$secondsy))
  })

modifieddata <- reactive({
  tryCatch(aggregatedtable()  %>%
             group_by_at(input$groupcolumns) %>%
             
             mutate(grouprepeat=n(),
                    groupdiff= as.integer( mean(DIFF_TO_PREV, na.rm=TRUE))) %>%
             ungroup() %>%
             mutate(
               year= as.numeric( case_when(
                 SY_LABEL == 'SY:2018 - 2019' ~ '2018',
                 SY_LABEL == 'SY:2019 - 2020' ~ '2019',
                 SY_LABEL == 'SY:2020 - 2021' ~ '2020',
                 SY_LABEL == 'SY:2021 - 2022' ~ '2021',
                 SY_LABEL == 'SY:2022 - 2023' ~ '2022',
                 SY_LABEL == 'SY:2023 - 2024' ~ '2023',
                 SY_LABEL == 'SY:2024 - 2025' ~ '2024',
                 TRUE ~ as.character(SY_LABEL)
               )),
               Active.Teachers= if_else(grouprepeat==2,  as.double(groupdiff), ifelse( SY_LABEL==input$secondsy, as.double(DISTINCT_TEACHERS), -as.double(DISTINCT_TEACHERS) )))  %>%
             drop_na(any_of(input$drop.na.columns)) %>%
             mutate(Rank=
                      switch(input$topdown,
                             "g" = dense_rank(-Active.Teachers) ,
                             "d" =  dense_rank(Active.Teachers))
             ) %>%
             arrange(Rank) %>%
             filter(Rank <= input$n & grouprepeat >= input$status ) %>%
             mutate(`The Difference in Number of Distinct Active Users`= fct_rev(as.factor(Active.Teachers))) %>% 
             ungroup()
           
           
           ,
           error=function(e) NULL)
})

  
output$aggdata <- renderDataTable({
  id <- showNotification("Render data", type="message", duration = NULL, closeButton = FALSE)
  on.exit(removeNotification(id), add = TRUE)
  
  modifieddata()
  
  datatable(modifieddata(), filter = list(position = 'top', clear = FALSE), options = list(autoWidth = TRUE, bAutoWidth = FALSE,  scrollX = TRUE) )
  
})

groupbysize <- reactive({
  length(input$groupcolumns)
})

number_of_top_ranks <- reactive({
  
  tryCatch( dim(modifieddata())[1],
  error=function(e) input$n
            )
})

output$diffchart <- renderPlot({
  firstcheck <- !is.na(modifieddata())
  secondcheck <- !is.na(number_of_top_ranks())
  req(firstcheck, secondcheck)
  
  facets <- paste0(unlist(input$groupcolumns), collapse = " + ")
  rest <- ', ncol=2,  labeller = label_both,  scales = "free"  )'
  text <- paste0("facet_wrap( ~ Rank+ `The Difference in Number of Distinct Active Users`+", facets, rest)
  modifieddata() %>%
    ggplot(aes(x= SY_LABEL, y= DISTINCT_TEACHERS, group=1))+
    geom_line(aes(color= Active.Teachers ), size=3)+
    geom_point(color='black', size=5)+
    eval(parse(text= text))+
    scale_color_gradient(low="#CC6666", high= "#66CC99")+
    theme_solarized()+
    theme(
      strip.background = element_rect(fill = "grey20", color = "grey80", size = 1),
      strip.text = element_text(colour = "white")
    )+
    ggtitle("Number of Distinct Teachers Trend")
    
   # facet_wrap(~ eval(parse(text=facets))  , ncol=2,  labeller = label_both,  scales = "free")
}
, height=  function(){if(is.null(number_of_top_ranks())){
  400
} else {
  300+ groupbysize()*50+  number_of_top_ranks()*150}
}
,res=96)
  
  output$querydata <- renderDataTable({
    id <- showNotification("Reading the data", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    datatable(aggregatedtable(),filter = list(position = 'top', clear = FALSE), options = list(autoWidth = TRUE, bAutoWidth = FALSE,  scrollX = TRUE) )
  })
  
  output$test <- renderPrint({
    id <- showNotification("Reading the data", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    #paste0(" BUY_ACCOUNT_NAME IN (","'", sql_BUY_ACCOUNT_NAME_where, "'", ") ")
    
   # paste0(" BUY_ACCOUNT_NAME IN (","'", paste0(input$BUY_ACCOUNT_NAME, collapse = "','"), "'", ") ")
   # paste0(" BUY_ACCOUNT_NAME IN (","'", paste0(input$BUY_ACCOUNT_NAME, collapse = "','"), "'", ") ")
    # if_else(input$BUY_ACCOUNT_NAME=="", " ",
    #         paste0(" AND ", " BUY_ACCOUNT_NAME IN (","'", paste0(input$BUY_ACCOUNT_NAME, collapse = "','"), "'", ") ")
    #          )
    #sql_BUY_ACCOUNT_NAME_where <- paste0(input$BUY_ACCOUNT_NAME, collapse = "','")
   # paste0(" AND ", " BUY_ACCOUNT_NAME IN (","'", sql_BUY_ACCOUNT_NAME_where, "'", ") ")
    # if_else(input$BUY_ACCOUNT_NAME=="", " ",
    #         paste0(" AND ", " BUY_ACCOUNT_NAME IN (","'", sql_BUY_ACCOUNT_NAME_where, "'", ") ")
    # )
    #a <-
    #paste0(input$BUY_SEGMENT, collapse = "','")
    #number_of_top_ranks()
    tryCatch( paste0("SELECT ",
           sql_select,
           ", SY_LABEL",
           ", COUNT(DISTINCT TCH_USERID) AS distinct_teachers ",
           ", distinct_teachers - lag(distinct_teachers, 1)  over
                                                  (partition by ", sql_select,
           " order by SY_LABEL) as diff_to_prev",
           " FROM analytics.sandbox.teacher_daily_activity_63_days_in_sy_pulled_20220902 ",
           "WHERE SY_LABEL IN (","'", input$firstsy,"'",
           ",", "'", input$secondsy, "'", ") ",
           
           if_else(length(input$BUY_ACCOUNT_NAME)==0 , " ",
                   paste0(" AND ", " BUY_ACCOUNT_NAME IN (","'", sql_BUY_ACCOUNT_NAME_where, "'", ") ")
           ),
           if_else(length(input$BUY_RECORD_TYPE)==0 , " ",
                   paste0(" AND ", " BUY_RECORD_TYPE IN (","'", sql_BUY_RECORD_TYPE_where, "'", ") ")
           ),
           if_else(length(input$BUY_SEGMENT)==0 , " ",
                   paste0(" AND ", " BUY_SEGMENT IN (","'", sql_BUY_SEGMENT_where, "'", ") ")
           ),
           if_else(length(input$BUY_REGION)==0 , " ",
                   paste0(" AND ", " BUY_REGION IN (","'", sql_BUY_REGION_where, "'", ") ")
           ),
           if_else(length(input$BUY_DATE_SCHOOL_STARTS)==0 , " ",
                   paste0(" AND ", " BUY_DATE_SCHOOL_STARTS IN (","'", sql_BUY_DATE_SCHOOL_STARTS_where, "'", ") ")
           ),
           if_else(length(input$BUY_HAS_PL_SY)==0 , " ",
                   paste0(" AND ", " BUY_HAS_PL_SY IN (","'", sql_BUY_HAS_PL_SY_where, "'", ") ")
           ),
           if_else(length(input$BUY_TOT_PL_SY)==0 , " ",
                   paste0(" AND ", " BUY_TOT_PL_SY IN (","'", sql_BUY_TOT_PL_SY_where, "'", ") ")
           ),
           if_else(length(input$SCH_CRT)==0 , " ",
                   paste0(" AND ", " SCH_CRT IN (","'", sql_SCH_CRT_where, "'", ") ")
           ),
           if_else(length(input$TCH_IS_TARGET)==0 , " ",
                   paste0(" AND ", " TCH_IS_TARGET IN (","'", sql_TCH_IS_TARGET_where, "'", ") ")
           ),
           if_else(length(input$TCH_LICENSE_TYPE)==0 , " ",
                   paste0(" AND ", " TCH_LICENSE_TYPE IN (","'", sql_TCH_LICENSE_TYPE_where, "'", ") ")
           ),
           if_else(length(input$TCH_LICENSE_COMBO)==0 , " ",
                   paste0(" AND ", " TCH_LICENSE_COMBO IN (","'", sql_TCH_LICENSE_COMBO_where, "'", ") ")
           ),
           if_else(length(input$TCH_IS_K2)==0 , " ",
                   paste0(" AND ", " TCH_IS_K2 IN (","'", sql_TCH_IS_K2_where, "'", ") ")
           ),
           if_else(length(input$TCH_IS_ELEM)==0 , " ",
                   paste0(" AND ", " TCH_IS_ELEM IN (","'", sql_TCH_IS_ELEM_where, "'", ") ")
           ),
           if_else(length(input$TCH_IS_MS)==0 , " ",
                   paste0(" AND ", " TCH_IS_MS IN (","'", sql_TCH_IS_MS_where, "'", ") ")
           ),
           if_else(length(input$TCH_IS_HS)==0 , " ",
                   paste0(" AND ", " TCH_IS_HS IN (","'", sql_TCH_IS_HS_where, "'", ") ")
           ),
           if_else(length(input$TCH_SUB_GRADE_BAND)==0 , " ",
                   paste0(" AND ", " TCH_SUB_GRADE_BAND IN (","'", sql_TCH_SUB_GRADE_BAND_where, "'", ") ")
           ),
           if_else(length(input$TCH_EVER_ACTIVE_SY)==0 , " ",
                   paste0(" AND ", " TCH_EVER_ACTIVE_SY IN (","'", sql_TCH_EVER_ACTIVE_SY_where, "'", ") ")
           ),
           if_else(length(input$TCH_ACTIVE_LAST_SY)==0 , " ",
                   paste0(" AND ", " TCH_ACTIVE_LAST_SY IN (","'", sql_TCH_ACTIVE_LAST_SY_where, "'", ") ")
           ),
           if_else(length(input$TCH_IS_FULL_PAID)==0 , " ",
                   paste0(" AND ", " TCH_IS_FULL_PAID IN (","'", sql_TCH_IS_FULL_PAID_where, "'", ") ")
           ),
           " AND DAY_IN_SY >=", input$DaySY[1],
           " AND DAY_IN_SY <=", input$DaySY[2],
           " AND DATE >= ", "'", input$daterange[1], "'",
           " AND DATE <= ", "'", input$daterange[2], "'",
           " AND DATE >= ", "'", input$TCH_DATE_LAST_LOGIN_range[1], "'",
           " AND DATE <= ", "'", input$TCH_DATE_LAST_LOGIN_range[2], "'",
           
           if_else(input$sy_active=="", " ",
                   paste0(" AND TCH_IS_SY_ACTIVE =", input$sy_active)
           ),
           " GROUP BY ",
           sql_select,
           ", SY_LABEL"
           #,
           #" limit ",input$n,"
           ,
           collapse = ""),
           error=function(e)NULL)
  }) 
}


# Run the application 
shinyApp(ui = ui, server = server)

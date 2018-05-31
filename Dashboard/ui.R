header<-  dashboardHeader(title=HTML('<div><img src="./Greensway-Logo-180307-3.png" alt="Greensway" height="50" style="vertical-align:middle;"></div>'), 
                          titleWidth = 230)

body<-dashboardBody(
  # useShinyjs(),
  tags$head(
    tags$link(rel="shortcut icon", href="./Greensway-Micro-logo-green.png"),
    # Include our custom CSS
    # includeCSS("styles.css"),
    # tags$style(HTML('.info-box {min-height: 55px;} .info-box-icon {height: 55px; line-height: 55px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}')),
    tags$script('!function(d,s,id){
                var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';
                if(!d.getElementById(id)){js=d.createElement(s);
                js.id=id;
                js.src=p+"://platform.twitter.com/widgets.js";
                fjs.parentNode.insertBefore(js,fjs);
                }}(document,"script","twitter-wjs");')
  ),
  column(9,
    fluidRow(
     valueBoxOutput("weekNr", width = 3),
     # infoBoxOutput("weekNr", width = 3),
     valueBoxOutput("temp", width = 3),
     valueBoxOutput("fika", width = 3),
     valueBoxOutput("fikaNext", width = 3)
    ),
    fluidRow(
      tabBox(width = 12,
        tabPanel(title = "Schema",
          # h4("Semester Planner"),
          # uiOutput("choices")
          plotOutput("hm", width = "97%")
        ),
        tabPanel(title = "Redigera",
          selectInput("editName", "Välj person:", choices = Names)
        )
      )
    )
  ),
  column(3,
         box(width=NULL, height="auto",
             a("Tweets by @GreenswayEco", class="twitter-timeline",
               #href = "https://twitter.com/search?q=GreenswayEco"
               href = "https://twitter.com/GreenswayEco"
             )
         )
  )
  
  
    )

# Define UI for application that draws a histogram
ui <- dashboardPage(title="Greensway Week",
                    header,
                    sidebar = dashboardSidebar( disable = TRUE),
                    body,
                    skin = "green")

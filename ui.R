# ui

shinyUI(dashboardPage(skin = "black",
                      
  dashboardHeader(title = "Parliament Overview"),
  
  dashboardSidebar(

    sidebarUserPanel("Deputies Expenditure", image = "eu.jpg"),

    sidebarMenu(
      menuItem("Per region", tabName = "map", icon = icon("map")),
      menuItem("Per political party", tabName = "party", icon = icon("building")),
      menuItem("Deputies", tabName = "deputies", icon = icon("user")),
  
      menuItem("Biggest spenders", tabName = "data", icon = icon("database"))
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              
              fluidRow(infoBoxOutput("totalBox"),
                       infoBoxOutput("deputyBox"),
                       infoBoxOutput("avgBox")),
              
              print(h3("Average deputy expenditure per year by region [R$]")),
              
                        # mapa
              fluidRow(shiny::column(6, offset = 0.5, box(htmlOutput("map"), height = 400, width = 400)),
                       shiny::column(6, offset = 1.5, 
                                     sliderInput("year_map", "Select year (2012-17)", min = 2012, max =2017,
                                                 value = 2017, step = 1, animate = TRUE)),
                       infoBoxOutput("stateBox")),
              
              print(h5("Brazil seems to have a big problem with deputies expenditure. On top of salaries, 
                       deputies are spending an average of 400 m R$ per year. That is more than 30 m R$ per deputy per month.
                       All combined, that sums up to 200 M R$ on taxes money yearly.")),
                       
              
              shiny::column(5, offset = 0.5, print(h3("Expenditure per deputy in each state [m R$]"))),
              
              fluidRow(shiny::column(3, offset = 1, selectizeInput("state", "Select state", br_states, "SP")),
                shiny::column(10, offset = 0.5, box(plotlyOutput("estado"), height = 450, width = 500)))),
              
      
      tabItem(tabName = "party",
              
              fluidRow(infoBoxOutput("partyavgBox"),
              infoBoxOutput("depBox"),
              infoBoxOutput("partymaxBox")),
              
              shiny::column(6, offset = 0.75, print(h3("Average deputy expenditure per political party [R$]"))),
              fluidRow(shiny::column(6, offset = 0.75, sliderInput("year_party", "Select year (2012 - 2017)",
                                                                    min = 2012, max = 2017,
                                                                    value = 2016, step = 1, animate = TRUE))),
              fluidRow(shiny::column(10, offset = 1, box(htmlOutput("bolhas"), width = 800, height = 460)))
              
              ),
      
      tabItem(tabName = "deputies",
              fluidRow(infoBoxOutput("avgexpBox"),
              infoBoxOutput("maxexpBox"),
              infoBoxOutput("maxcatBox")),
              
              print(h3("Expenditure per deputy [R$] (2012-17)")),
              
              fluidRow(column(4, offset = 1, selectizeInput("partido",
                             "Select the Political Party",
                             parties)),
                       column(4, offset = 1, selectizeInput(inputId = "deputado",
                             label = "Select the Deputy",
                             choices = unique(reembolsos$deputy_name)[order(unique(reembolsos$deputy_name))]
              ))),
              fluidRow(shiny::column(12, offset = 0.5, box(plotlyOutput("gastos_deputados"), width = 50, height = 450))
                
              )
        
      ),
     
      
       tabItem(tabName = "data",
               fluidRow(infoBoxOutput("maxreceiptBox"),
                        infoBoxOutput("maxreceiptcategoryBox")),
               fluidRow(shiny::column(4, offset = 1, selectizeInput("category",
                                       "Select the Category",
                                       category))),
              # datatable
              fluidRow(box(DT::dataTableOutput("table"),
                           width = 12)))
      )
    )
  ))

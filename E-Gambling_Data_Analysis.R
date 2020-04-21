#install.packages("plotly")
library(plotly)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("d3heatmap")
library(d3heatmap)

#install.packages("shinythemes")
library(shinythemes)

#install.packages("RColorBrewer")
library(RColorBrewer)

#install.packages("DT")
library(DT)

#install.packages("maptools")
library(maptools)

#install.packages("maps")
library(maps)

#install.packages("ggmaps")
library(ggmap)

#install.packages(shinydashboard)
library(shinydashboard)

library(shiny)

##Preparation for Box Plots
#this shows boxplots of the selected metrics, with outliers disabled (to do)
#creation of subsets for easier subsetting 
monthlystakes <- c(30:37)
monthlywinnings <- c(38:45)
monthlybets<- c(46:53)
monthlyROI<- c(54:61)
weeklystakes<- c(62:68)
weeklywinnings<- c(69:75)
weeklybets<- c(76:82)

boxplotchoices <- c(30:82)

##Palette required for clusters
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

##Preparation for correlation Heat Map
gambling <- datafull
#Creating a list of variables to use for correlation
varlist <- c("AGE", "Gender", "FOTotalStakes", "FOTotalWinnings", "FOTotalBets" , "FOTotalDaysActive", "LATotalStakes", 
             "LATotalWinnings", "LATotalBets",  "LATotalDaysActive", "Totalstakes", "TotalBets")
#Replacing NAs with 0
gambling[varlist] <- replace(gambling[varlist], is.na(gambling[varlist]), 0) 

gambling_matrix <- gambling%>%
  select(AGE, Gender, FOTotalStakes, FOTotalWinnings, FOTotalBets, FOTotalDaysActive, LATotalStakes, 
         LATotalWinnings, LATotalBets,  LATotalDaysActive, Totalstakes, TotalBets)

#Creating a matrix for correlation computation
mat_g <- data.matrix(gambling_matrix)

#computation of the correlation matrix
correlation <- round(cor(mat_g), 3)

##For Stacked Bar Chart
t1 <- datafull
t1 <- t1[complete.cases(t1$Gender), ]

##Preparation for Heatmaps
#Creating new data table
dbets_months_gender <- final_data %>%
  group_by(Gender)%>%
  summarise(bets_Apr = mean(bets_Apr),
            bets_Aug = mean(bets_Aug),
            bets_Feb = mean(bets_Feb),
            bets_Jul = mean(bets_Jul),
            bets_Jun = mean(bets_Jun),
            bets_Mar = mean(bets_Mar), 
            bets_May = mean(bets_May),
            bets_Sep = mean(bets_Sep))

dbets_months_gender <- dbets_months_gender%>%
  mutate_at(2:9, round, 1)

#Sorting the data by gender
heat1 <- dbets_months_gender
heat1 <- heat1[order(heat1$Gender),]
heat_label_1 <- heat1$Gender

#Defining area of table that needs to be graphed
graph_heat_1 <- heat1[, 2:9]
graph_matrix_1 <- data.matrix(graph_heat_1)


#Creating new data table
stakes_months_gender <- final_data %>%
  group_by(Gender)%>%
  summarise(stakes_Apr = mean(stakes_Apr),
            stakes_Aug = mean(stakes_Aug),
            stakes_Feb = mean(stakes_Feb),
            stakes_Jul = mean(stakes_Jul),
            stakes_Jun = mean(stakes_Jun),
            stakes_Mar = mean(stakes_Mar), 
            stakes_May = mean(stakes_May),
            stakes_Sep = mean(stakes_Sep))

stakes_months_gender <- stakes_months_gender%>%
  mutate_at(2:9, round, 1)

#Sorting the data by gender
heat2 <- stakes_months_gender
heat2 <- heat2[order(heat2$Gender),]
heat_label_2 <- heat2$Gender

#Defining area of table that needs to be graphed
graph_heat_2 <- heat2[, 2:9]
graph_matrix_2 <- data.matrix(graph_heat_2)

#Creating new data table
wins_months_gender <- final_data %>%
  group_by(Gender)%>%
  summarise(wins_Apr = mean(winnings_Apr),
            wins_Aug = mean(winnings_Aug),
            wins_Feb = mean(winnings_Feb),
            wins_Jul = mean(winnings_Jul),
            wins_Jun = mean(winnings_Jun),
            wins_Mar = mean(winnings_Mar), 
            wins_May = mean(winnings_May),
            wins_Sep = mean(winnings_Sep))

wins_months_gender <- wins_months_gender%>%
  mutate_at(2:9, round, 1)

#Sorting the data by gender
heat3 <- wins_months_gender
heat3 <- heat3[order(heat3$Gender),]
heat_label_3 <- heat3$Gender

#Defining area of table that needs to be graphed
graph_heat_3 <- heat3[, 2:9]
graph_matrix_3 <- data.matrix(graph_heat_3)

##Preparation for heat maps by countries
#Creating new data table
data_country <- final_data %>%
  group_by(Country)%>%
  summarise(bets_Apr = mean(bets_Apr),
            bets_Aug = mean(bets_Aug),
            bets_Feb = mean(bets_Feb),
            bets_Jul = mean(bets_Jul),
            bets_Jun = mean(bets_Jun),
            bets_Mar = mean(bets_Mar), 
            bets_May = mean(bets_May),
            bets_Sep = mean(bets_Sep),
  )

data_country <- data_country%>%
  mutate_at(2:9, round, 1)

#Sorting the data by country
heat <- data_country
heat <- heat[order(heat$Country),]
heat_label <- heat$Country

#Defining area of table that needs to be graphed
graph_heat <- heat[, 2:9]
graph_matrix <- data.matrix(graph_heat)

##Heatmap for week by Gender
#Creating new data table
dbets_week_gender <- final_data %>%
  group_by(Gender)%>%
  summarise(bets_Mon = mean(bets_Mon),
            bets_Tue = mean(bets_Tue),
            bets_Wed = mean(bets_Wed),
            bets_Thu = mean(bets_Thu),
            bets_Fri = mean(bets_Fri),
            bets_Sat = mean(bets_Sat), 
            bets_Sun = mean(bets_Sun))

dbets_week_gender <- dbets_week_gender%>%
  mutate_at(2:8, round, 1)

#Sorting the data by gender
heat_w_1 <- dbets_week_gender
heat_w_1 <- heat_w_1[order(heat_w_1$Gender),]
heat_label_w_1 <- heat_w_1$Gender

#Defining area of table that needs to be graphed
graph_heat_w_1 <- heat_w_1[, 2:8]
graph_matrix_w_1 <- data.matrix(graph_heat_w_1)


#Creating new data table
stakes_week_gender <- final_data %>%
  group_by(Gender)%>%
  summarise(stakes_Mon = mean(stakes_Mon),
            stakes_Tue = mean(stakes_Tue),
            stakes_Wed = mean(stakes_Wed),
            stakes_Thu = mean(stakes_Thu),
            stakes_Fri = mean(stakes_Fri),
            stakes_Sat = mean(stakes_Sat), 
            stakes_Sun = mean(stakes_Sun))

stakes_week_gender <- stakes_week_gender%>%
  mutate_at(2:8, round, 1)

#Sorting the data by gender
heat_W_2 <- stakes_week_gender
heat_W_2 <- heat_W_2[order(heat_W_2$Gender),]
heat_label_w_2 <- heat_W_2$Gender

#Defining area of table that needs to be graphed
graph_heat_w_2 <- heat_W_2[, 2:8]
graph_matrix_w_2 <- data.matrix(graph_heat_w_2)

#Creating new data table
wins_week_gender <- final_data %>%
  group_by(Gender)%>%
  summarise(wins_Mon = mean(winnings_Mon),
            wins_Tue = mean(winnings_Tue),
            wins_Wed = mean(winnings_Wed),
            wins_Thu = mean(winnings_Thu),
            wins_Fri = mean(winnings_Fri),
            wins_Sat = mean(winnings_Sat), 
            wins_Sun = mean(winnings_Sun))

wins_week_gender <- wins_week_gender%>%
  mutate_at(2:8, round, 1)

#Sorting the data by gender
heat_w_3 <- wins_week_gender
heat_w_3 <- heat_w_3[order(heat_w_3$Gender),]
heat_label_w_3 <- heat_w_3$Gender

#Defining area of table that needs to be graphed
graph_heat_w_3 <- heat_w_3[, 2:8]
graph_matrix_w_3 <- data.matrix(graph_heat_w_3)


##SHINY DASHBOARD

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Group 9 Project"),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Additional Analysis", tabName = "analysis", icon = icon("fas fa-chart-line")),
        menuItem("Data Mart", tabName = "raw",icon = icon("file-text-o"))
      )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                tabsetPanel(
                  tabPanel("Histograms",
                           fluidRow(
                             box(title = "Histograms of Variables in the Data Mart", status = "primary", solidHeader = TRUE, plotOutput('Hist')),
                             box("Select only numeric variables", 
                                 title = "Graph Controls", status = "warning", solidHeader = TRUE, br(),
                                 selectInput("Ind","Independent Variable",choices = names(final_data)),
                                 sliderInput("bins", "Number of bins", value = 25, min = 1, max = 100))
                           )
                  ),
                  tabPanel("Box Plots",
                           fluidRow(
                             box(title= "Box Plot 1",  status = "primary", solidHeader = TRUE, plotOutput('Boxplot')),
                             box(title = "Graph Controls", status = "warning", solidHeader = TRUE,
                                 selectInput("Ind1","select what you want to compare (Box Plot 1)",choices = names(final_data[,30:82])),
                                 selectInput("Ind2","select what you want to compare (Box Plot 2)",choices = names(final_data[,30:82]))),
                           ),
                           fluidRow(
                             box(title= "Box Plot 2",  status = "primary", solidHeader = TRUE, plotOutput('Boxplot2'))
                           )
                  ),
                  tabPanel("K-Mean Clusters",
                           fluidRow(
                             box(title= "Clustering for variables in Data Mart",  status = "primary", solidHeader = TRUE, plotOutput('plot1')),
                             box(title = "Graph Controls", status = "warning", solidHeader = TRUE,
                                 selectInput('xcol', 'X Variable', names(final_data)),
                                 selectInput('ycol', 'Y Variable', names(final_data),selected = names(final_data)[[2]]),
                                 numericInput('clusters', 'Cluster count', 3, min = 1, max = 9))
                           )
                  ),
                  tabPanel("Correlation Heat Map",
                           fluidRow(
                             box(title= "Correlation of Variables for Internet Sports Gambling Activity",  status = "primary", solidHeader = TRUE, plotlyOutput("heat"))
                           )
                  ),
                  tabPanel("Stacked Bar Charts",
                           fluidRow(
                             box(title= "Total Stakes by Gender", status = "primary", solidHeader = TRUE, plotOutput("Bar1")),
                             box(title= "Total Bets by Gender", status = "primary",solidHeader = TRUE, plotOutput("Bar2"))
                           )
                  ),
                  tabPanel("Monthly Analysis by Gender",
                           fluidRow(
                             box(title= "Average No. of Monthly Bets by Gender", status = "primary", solidHeader = TRUE, d3heatmapOutput("heatmap1", height="200px", width="50%")),
                             box(title= "Average No. of Monthly Stakes by Gender", status = "danger", solidHeader = TRUE, d3heatmapOutput("heatmap2", height="200px", width="50%")),
                             box(title= "Average No. of Monthly Winnings by Gender", status = "success", solidHeader = TRUE, d3heatmapOutput("heatmap3", height="200px", width="50%")))
                  ),
                  tabPanel("Weekly Analysis by Gender",
                           fluidRow(
                             box(title= "Average No. of Weekly Bets by Gender", status = "primary", solidHeader = TRUE, d3heatmapOutput("heatmap_w_1", height="200px", width="50%")),
                             box(title= "Average No. of Weekly Stakes by Gender", status = "danger", solidHeader = TRUE, d3heatmapOutput("heatmap_w_2", height="200px", width="50%")),
                             box(title= "Average No. of Weekly Winnings by Gender", status = "success", solidHeader = TRUE, d3heatmapOutput("heatmap_w_3", height="200px", width="50%"))
                           )
                  ),
                  tabPanel("Monthly Bets by Country",
                           fluidRow(
                             box(title= "No. of Monthly Bets by Country", status = "primary", solidHeader = TRUE, d3heatmapOutput("heatmap", height="950px", width="80%"))
                           )
                  ),
                  tabPanel("World Map by age",
                           fluidRow(
                             box(plotOutput('map'))
                           )
                  )
                )
        ),
        tabItem(tabName = "analysis", 
                tabsetPanel(type = "tab",
                            tabPanel("Poker Sell By Months",tags$img(src = "Picture1.png")),
                            tabPanel("Poker Sell by Daily Hours",tags$img(src = "Picture2.png")),
                            tabPanel("Poker Buy by Months",tags$img(src = "Picture3.png")),
                            tabPanel("Poker Buy by Daily Hours",tags$img(src = "Picture4.png")),
                            tabPanel("Users Per Activity",tags$img(src = "Picture5.png")),
                            tabPanel("Most used Applications",tags$img(src = "Picture6.png")),
                            tabPanel("Most Widely Spoken User Languages",tags$img(src = "Picture7.png")),
                            tabPanel("Users by Country",tags$img(src = "Picture8.png")),
                            tabPanel("Step Duration",tags$img(src = "Picture9.png")),
                            tabPanel("Time till First Pay",tags$img(src = "Picture10.png")),
                            tabPanel("Winners_Losers by Balance",tags$img(src = "Picture11.png")),
                            tabPanel("Biggest Single Winners",tags$img(src = "Picture12.png")),
                            tabPanel("Winnings Analysis",tags$img(src = "Picture13.png"))
                )
                
        ),
        tabItem(tabName = "raw", h1("Final Data Mart"), DT::dataTableOutput("contents"))
      )
    )
  )
)

server <- function(input, output){
  data1 <- reactive({input$Ind})
  
  data_1 <- reactive({input$Ind1})
  
  data2 <- reactive({input$Ind2})
  
  selectedData <- reactive({
    final_data[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$Hist <- renderPlot({
    req(data1())
    hist(final_data[[data1()]], breaks = input$bins)
  }) 
  
  output$Boxplot <- renderPlot({
    req(data_1())
    boxplot(final_data[[data_1()]],outline= FALSE,horizontal = TRUE, height = 20, width = 60 )
  })
  
  output$Boxplot2 <- renderPlot({
    req(data1())
    boxplot(final_data[[data2()]],outline= FALSE,horizontal = TRUE,  height = 20, width = 60 )
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  output$heat <- renderPlotly({
    plot_ly(source = "heat_plot") %>%
      add_heatmap(
        x = colnames(mat_g), 
        y = colnames(mat_g), 
        z = correlation
      )
  })
  
  output$Bar1 <- renderPlot({
    bar_1 <- tapply(t1$Totalstakes, list(t1$Gender, t1$TierStakes), mean)
    barplot(bar_1, xlab = "Tiers", ylab = "Total Stakes")
    
  })
  
  output$Bar2 <- renderPlot({
    bar_2 <- tapply(t1$TotalBets, list(t1$Gender, t1$TierBets), mean)
    barplot(bar_2, xlab = "Tiers", ylab = "Total Bets")
    
  })
  
  output$heatmap1 <- renderD3heatmap({d3heatmap(graph_matrix_1, Rowv=NA, Colv=NA, 
                                                colors=brewer.pal(9,"Blues"), scale="none", show_grid=TRUE,
                                                cellnote=graph_heat_1, labRow=heat_label_1, 
                                                xaxis_font_size=14, yaxis_font_size=14, height=1200, width = 2400, na.rm = TRUE)})
  
  output$heatmap2 <- renderD3heatmap({d3heatmap(graph_matrix_2, Rowv=NA, Colv=NA, 
                                                colors=brewer.pal(9,"Reds"), scale="none", show_grid=TRUE,
                                                cellnote=graph_heat_2, labRow=heat_label_2, 
                                                xaxis_font_size=14, yaxis_font_size=14, height=1800, width = 2400, na.rm = TRUE)})
  
  output$heatmap3 <- renderD3heatmap({d3heatmap(graph_matrix_3, Rowv=NA, Colv=NA, 
                                                colors=brewer.pal(9,"Greens"), scale="none", show_grid=TRUE,
                                                cellnote=graph_heat_3, labRow=heat_label_3, 
                                                xaxis_font_size=14, yaxis_font_size=14, height=1800, width = 2400, na.rm = TRUE)})
  
  output$heatmap <- renderD3heatmap({d3heatmap(graph_matrix, Rowv=NA, Colv=NA, 
                                               colors=brewer.pal(9,"Blues"), scale="none", show_grid=TRUE,
                                               cellnote=graph_heat, labRow=heat_label, 
                                               xaxis_font_size=12, yaxis_font_size=12, yaxis_width = 280, height=1200, width = 2400, na.rm = TRUE)})
  
  output$heatmap_w_1 <- renderD3heatmap({d3heatmap(graph_matrix_w_1, Rowv=NA, Colv=NA, 
                                                   colors=brewer.pal(9,"Blues"), scale="none", show_grid=TRUE,
                                                   cellnote=graph_heat_w_1, labRow=heat_label_w_1, 
                                                   xaxis_font_size=14, yaxis_font_size=14, height=1200, width = 2400, na.rm = TRUE)})
  
  output$heatmap_w_2 <- renderD3heatmap({d3heatmap(graph_matrix_w_2, Rowv=NA, Colv=NA, 
                                                   colors=brewer.pal(9,"Reds"), scale="none", show_grid=TRUE,
                                                   cellnote=graph_heat_w_2, labRow=heat_label_w_2, 
                                                   xaxis_font_size=14, yaxis_font_size=14, height=1800, width = 2400, na.rm = TRUE)})
  
  output$heatmap_w_3 <- renderD3heatmap({d3heatmap(graph_matrix_w_3, Rowv=NA, Colv=NA, 
                                                   colors=brewer.pal(9,"Greens"), scale="none", show_grid=TRUE,
                                                   cellnote=graph_heat_w_3, labRow=heat_label_w_3, 
                                                   xaxis_font_size=14, yaxis_font_size=14, height=1800, width = 2400, na.rm = TRUE)})
  output$map <- renderPlot({
    register_google(key = "AIzaSyAnX8eeXBLkTY94KWyWm7b2eO1SbkJfotk")
    ggmap(get_map(location='data', maptype = "terrain", source='google', zoom =1)) + geom_point(aes(x=long.y, y=lat.y, color= AGE), data =final_data, size=0.5)
  })
  
  output$contents <- DT::renderDataTable({
    final_data
  })
  
}

shinyApp(ui, server)
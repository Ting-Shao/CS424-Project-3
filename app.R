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
library(readxl)
library(ggplot2)
library(lubridate)
library(shinyWidgets)
library(readr)
library(tigris)
library(mapview)
library(leaflet)
library(scales)
library(usmap)
library(stringr)
library(DT)
library(jpeg)
library(grid)
library(leafpop)

#read data
#EU_2010 <- read_excel("Energy_Usage_2010_modified.xls")
EU_2010 <- read_csv("Energy_Usage_2010.csv")

#preprocess data
names(EU_2010)[names(EU_2010) == "CENSUS BLOCK"] <- 'GEOID10'
EU_2010$GEOID10 <- as.character(EU_2010$GEOID10)



Communities <- unique(EU_2010$`COMMUNITY AREA NAME`)
e_c <- c("KWH JANUARY 2010", "KWH FEBRUARY 2010", "KWH MARCH 2010", "KWH APRIL 2010" , "KWH MAY 2010", "KWH JUNE 2010", "KWH JULY 2010","KWH AUGUST 2010","KWH SEPTEMBER 2010","KWH OCTOBER 2010","KWH NOVEMBER 2010","KWH DECEMBER 2010","TOTAL KWH")
g_c <- c("THERM JANUARY 2010", "THERM FEBRUARY 2010", "THERM MARCH 2010", "TERM APRIL 2010", "THERM MAY 2010", "THERM JUNE 2010", "THERM JULY 2010", "THERM AUGUST 2010", "THERM SEPTEMBER 2010", "THERM OCTOBER 2010", "THERM NOVEMBER 2010", "THERM DECEMBER 2010", "TOTAL THERMS")



Building_types <- c("All", "Residential", "Commercial", "Industrial")
entire_select <- c("oldest buildings",
                   "newest building",
                   "tallest buildings",
                   "blocks that use the most electricity over the year",
                   "blocks that use the most gas over the year",
                   "most population",
                   "most occupied percentage",
                   "highest percentage of renters")

blocks17031 <- blocks(state = "17", county = "031", year = "2010")
tracts17031 <- tracts(state = "17", county = "031", cb = TRUE, year = "2010")
 
show <- c("Gas",
          "Electricity",
          "Building Age",
          "Building Type",
          "Building Height", 
          "Total Population")
month <- append("Total", month.name)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "CS424 Spring 2021 Project3"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     sidebarMenu(
                         id="tabs",
                         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("Split screen visualization", tabName = "split", icon = icon("dashboard")),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("The entire city of Chicago", tabName = "entire_Chicago", icon = icon("dashboard")),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         #menuItem("Plants added or idled", tabName = "Plants_variation", icon = icon("dashboard")),
                         
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("About page", tabName = "about", icon = icon("question"))
                     ),
                     hr()
    ),
    #==========    ==========    ==========    dashboard    ==========    ==========    ==========  
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    navbarPage("Near West Side",
                               tabPanel("Plot",
                                        box( width = NULL, status = "primary", solidHeader = TRUE, title= "Plot",
                                             column(4,
                                                    selectInput('show1', 'To show:', show, selected = "Electricity"),
                                             ),
                                             column(3,
                                                    selectInput('bt1', 'Building type:', Building_types, selected = "All"),
                                             ),
                                             column(3,
                                                    selectInput('month1', 'Month:', month, selected = "Total"),
                                             ),
                                             column(2,
                                                    actionButton("reset1", "Reset"),
                                                    textOutput("text"),
                                             ),
                                             
                                             
                                             #helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                                             leafletOutput('NWSplot'),
                                             #plotOutput("total_amount of_electricity_and_gas"),
                                        ),
                                        
                               ),
                               tabPanel("Graph",
                                        box( width = NULL, status = "primary", solidHeader = TRUE, title= "Graph",
                                             plotOutput("graph1", height = "300px"),
                                             plotOutput("graph2", height = "300px")
                                        )
                                        
                                        # sidebarLayout(
                                        #     sidebarPanel(
                                        #         radioButtons("plotType", "Plot type",
                                        #                      c("Scatter"="p", "Line"="l")
                                        #         )
                                        #     ),
                                        #     mainPanel(
                                        #         box( width = NULL, status = "primary", solidHeader = TRUE, title= "Table",
                                        #              plotOutput("graph1",height = "300px"),
                                        #              plotOutput("graph2",height = "300px")
                                        #         )
                                        #     )
                                        # ),
                               ),
                               tabPanel("Table",
                                        box( width = NULL, status = "primary", solidHeader = TRUE, title= "Table",
                                             dataTableOutput("Table"),
                                             
                                        )
                               )
                    ),
                    
            ),
            tabItem(tabName = "split",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title= "Split screen visualization",
                         #helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                         column(6,
                                column(7,
                                       selectInput('ca1', 'Community Area1', Communities, selected = "Near West Side"),
                                       column(6,
                                              selectInput('sbt1', 'Building type:', Building_types, selected = "All"),
                                       ),
                                       column(6,
                                              selectInput('smonth1', 'Month:', month, selected = "Total"),
                                       ),
                                       style="z-index:1002;"
                                ),
                                column(5,
                                       selectInput('sshow1', 'To show:', show, selected = "Electricity"),
                                       actionButton("button2", "Reset"),
                                       style="z-index:1002;"
                                ),
                                column(12,
                                       navbarPage("Split1",
                                                  tabPanel("Plot",
                                                           leafletOutput('split1', width = "450px"),
                                                  ),
                                                  tabPanel("Graph",
                                                           plotOutput("s1graph1", height = "250px"),
                                                           plotOutput("s1graph2", height = "250px")
                                                  ),
                                                  tabPanel("Table",
                                                           dataTableOutput("table1"),
                                                  )
                                       ),
                                )
                                
                                
                         ),
                         column(6,
                                column(7,
                                       selectInput('ca2', 'Community Area2', Communities, selected = "Loop"),
                                       column(6,
                                              selectInput('sbt2', 'Building type:', Building_types, selected = "All"),
                                       ),
                                       column(6,
                                              selectInput('smonth2', 'Month:', month, selected = "Total"),
                                       ),
                                       style="z-index:1002;"
                                ),
                                column(5,
                                       selectInput('sshow2', 'To show:', show, selected = "Electricity"),
                                       actionButton("button3", "Reset"),
                                       style="z-index:1002;"
                                ),
                                column(12,
                                       navbarPage("Split2",
                                                  tabPanel("Plot",
                                                           leafletOutput('split2', width = "450px"),
                                                  ),
                                                  tabPanel("Graph",
                                                           plotOutput("s2graph1", height = "250px"),
                                                           plotOutput("s2graph2", height = "250px")
                                                  ),
                                                  tabPanel("Table",
                                                           dataTableOutput("table2"),
                                                  )
                                       ),
                                )
                         ),
                    )
            ),
            tabItem(tabName = "entire_Chicago",
                    
                    
                    
                    box( width = NULL, status = "primary", solidHeader = TRUE, title= "10% census tracts",
                         selectInput('show_en', 'To show:', entire_select),
                         leafletOutput("entire_city", height = "500px")
                         
                         
                    )
            ),
            
            
            tabItem(tabName = "about",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title= "About page",
                         #helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                         mainPanel(
                             h1("Data reference"),
                             h3("This is a dataset hosted by the City of Chicago."),
                             h3("Displays several units of energy consumption for households, businesses, and industries in the City of Chicago during 2010. Electric The data was aggregated from ComEd and Peoples Natural Gas by Accenture. Electrical and gas usage data comprises 88 percent of Chicago's buildings in 2010. The electricity data comprises 68 percent of overall electrical usage in the city while gas data comprises 81 percent of all gas consumption in Chicago for 2010."),
                             h5("The original data is available from "),
                             uiOutput("tab1"),
                             h5("and also available at "),
                             uiOutput("tab2"),
                             
                             h1("App developer"),
                             h3("Ting-Shao, Lee"),
                             h3("This application is part of my CS424 project 3 at the University of Illinois at Chicago, Spring 2021."),
                         )
                    )
            )
        )     
    )
        
    #setBackgroundColor("AliceBlue"),

    # Sidebar with a slider input for number of bins 
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    
    output$text <- renderText({ 
        month1 <- input$month1
        month1 <-  paste(toupper(month1),"2010")
    })
    
    output$NWSplot <- renderLeaflet({
        show1 <- input$show1
        bt<- input$bt1
        month1 <- input$month1
        month1 <-  paste(toupper(month1),"2010")

        NWS_2010 <- subset(EU_2010, EU_2010$"COMMUNITY AREA NAME"=="Near West Side")
        if (bt!="All"){
            NWS_2010 <- subset(NWS_2010, NWS_2010$"BUILDING TYPE"==bt)
        }
        
        aggNWS <- aggregate(NWS_2010[,c(e_c,g_c)],by=list(NWS_2010$GEOID10),FUN=sum)
        names(aggNWS)[names(aggNWS) == "Group.1"] <- 'GEOID10'
        
        mNWS_2010 <- merge(blocks17031, NWS_2010, by.x = "GEOID10", by.y = "GEOID10")
        #maggNWS <- merge(blocks17031, aggNWS, by.x = "GEOID10", by.y = "GEOID10")
        summ =colSums(aggNWS[,-1],na.rm = TRUE)
        # 
        e <-data.frame(as.factor(1:12),unname(summ[1:12]),rep("Electricity",12))
        names(e)<-c("Month","value","type")

        g <-data.frame(as.factor(1:12),unname(summ[15:length(summ)-1]),rep("Gas",12))
        names(g)<-c("Month","value","type")
        

        output$Table <- DT::renderDataTable(
            DT::datatable({
                NWS_2010
            },
            options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(0, 'asc')), scrollX = T
            ), rownames = FALSE
            )
        )
        
        output$graph1 <- renderPlot({
            ggplot(e, aes(y=value, x=Month)) + geom_bar(position="dodge", stat="identity", fill="red3")+ ggtitle("Electricity")
        })
        output$graph2 <- renderPlot({
            ggplot(g, aes(y=value, x=Month)) + geom_bar(position="dodge", stat="identity", fill="yellow3")+ ggtitle("Gas")
        })
        

        if (show1=="Electricity"){
            if(month1=="TOTAL 2010"){
                zcol = "TOTAL KWH"
            }else{
                zcol <- paste("KWH", month1)
            }
        }else if (show1=="Gas"){
            if(month1=="TOTAL 2010"){
                zcol = "TOTAL THERMS"
            }else{
                zcol <- paste("THERM", month1)
            }
        }else if (show1=="Building Age"){
            zcol = "AVERAGE BUILDING AGE"
        }else if (show1=="Building Type"){
            zcol = "BUILDING TYPE"
        }else if (show1=="Building Height"){
            zcol = "AVERAGE STORIES"
        }else if (show1=="Total Population"){
            zcol = "TOTAL POPULATION"
        }
        mapview(mNWS_2010, zcol = zcol, alpha.regions = 0.2, aplha = 1)@map
        
    })
    

    
    output$split1 <- renderLeaflet({
        ca <- input$ca1
        show <- input$sshow1
        bt <- input$sbt1
        month <- input$smonth1
        month <-  paste(toupper(month),"2010")
        
        ca_2010 <- subset(EU_2010, EU_2010$"COMMUNITY AREA NAME"==ca)
        if (bt!="All"){
            ca_2010 <- subset(ca_2010, ca_2010$"BUILDING TYPE"==bt)
        }
        mca_2010 <- merge(blocks17031, ca_2010, by.x = "GEOID10", by.y = "GEOID10")
        
        output$table1 <- DT::renderDataTable(
            DT::datatable({
                ca_2010
            },
            options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc')), scrollX = T
            ), rownames = FALSE
            )
        )
        
        
        if (show=="Electricity"){
            if(month=="TOTAL 2010"){
                zcol <- "TOTAL KWH"
            }else{
                zcol <- paste("KWH", month)
            }
        }else if (show=="Gas"){
            if(month=="TOTAL 2010"){
                zcol <- "TOTAL THERMS"
            }else{
                zcol <- paste("THERM", month)
            }
        }else if (show=="Building Age"){
            zcol <- "AVERAGE BUILDING AGE"
        }else if (show=="Building Type"){
            zcol <- "BUILDING TYPE"
        }else if (show=="Building Height"){
            zcol <- "AVERAGE STORIES"
        }else if (show=="Total Population"){
            zcol <- "TOTAL POPULATION"
        }
        plots1 <- mapview(mca_2010, zcol = zcol, alpha.regions = 0.2, aplha = 1)@map
        plots1
    })
    
    output$s1graph1 <- renderPlot({
        ca <- input$ca1
        ca_2010 <- subset(EU_2010, EU_2010$"COMMUNITY AREA NAME"==ca)
        aggca_2010 <- aggregate(ca_2010[,c(5:17,19:31)],by=list(ca_2010$GEOID10),FUN=sum)
        names(aggca_2010)[names(aggca_2010) == "Group.1"] <- 'GEOID10'
        summ =colSums(aggca_2010[,-1],na.rm = TRUE)
        
        e <-data.frame(as.factor(1:12),unname(summ[1:12]),rep("Electricity",12))
        names(e)<-c("Month","value","type")
        ggplot(e, aes(y=value, x=Month)) + geom_bar(position="dodge", stat="identity", fill="red3")+ ggtitle("Electricity")
    })
    output$s1graph2 <- renderPlot({
        ca <- input$ca1
        ca_2010 <- subset(EU_2010, EU_2010$"COMMUNITY AREA NAME"==ca)
        aggca_2010 <- aggregate(ca_2010[,c(5:17,19:31)],by=list(ca_2010$GEOID10),FUN=sum)
        names(aggca_2010)[names(aggca_2010) == "Group.1"] <- 'GEOID10'
        summ =colSums(aggca_2010[,-1],na.rm = TRUE)
        
        g <-data.frame(as.factor(1:12),unname(summ[15:length(summ)-1]),rep("Gas",12))
        names(g)<-c("Month","value","type")
        ggplot(g, aes(y=value, x=Month)) + geom_bar(position="dodge", stat="identity", fill="yellow3")+ ggtitle("Gas")
    })
    
    output$split2 <- renderLeaflet({
        ca <- input$ca2
        show <- input$sshow2
        bt <- input$sbt2
        month <- input$smonth2
        month <-  paste(toupper(month),"2010")
        
        ca_2010 <- subset(EU_2010, EU_2010$"COMMUNITY AREA NAME"==ca)
        if (bt!="All"){
            ca_2010 <- subset(ca_2010, ca_2010$"BUILDING TYPE"==bt)
        }
        mca1_2010 <- merge(blocks17031, ca_2010, by.x = "GEOID10", by.y = "GEOID10")
        
        output$table2 <- DT::renderDataTable(
            DT::datatable({
                ca_2010
            },
            options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc')), scrollX = T
            ), rownames = FALSE
            )
        )
        
        
        if (show=="Electricity"){
            if(month=="TOTAL 2010"){
                zcol <- "TOTAL KWH"
            }else{
                zcol <- paste("KWH", month)
            }
        }else if (show=="Gas"){
            if(month=="TOTAL 2010"){
                zcol <- "TOTAL THERMS"
            }else{
                zcol <- paste("THERM", month)
            }
        }else if (show=="Building Age"){
            zcol <- "AVERAGE BUILDING AGE"
        }else if (show=="Building Type"){
            zcol <- "BUILDING TYPE"
        }else if (show=="Building Height"){
            zcol <- "AVERAGE STORIES"
        }else if (show=="Total Population"){
            zcol <- "TOTAL POPULATION"
        }
        plots2 <- mapview(mca1_2010, zcol = zcol, alpha.regions = 0.2, aplha = 1)@map
        plots2
    })
    
    
    output$s2graph1 <- renderPlot({
        ca2 <- input$ca2
        ca_2010 <- subset(EU_2010, EU_2010$"COMMUNITY AREA NAME"==ca2)
        aggca_2010 <- aggregate(ca_2010[,c(5:17,19:31)],by=list(ca_2010$GEOID10),FUN=sum)
        names(aggca_2010)[names(aggca_2010) == "Group.1"] <- 'GEOID10'
        
        summ =colSums(aggca_2010[,-1],na.rm = TRUE)
        # 
        e <-data.frame(as.factor(1:12),unname(summ[1:12]),rep("Electricity",12))
        names(e)<-c("Month","value","type")
        ggplot(e, aes(y=value, x=Month)) + geom_bar(position="dodge", stat="identity", fill="red3")+ ggtitle("Electricity")
    })
    output$s2graph2 <- renderPlot({
        ca2 <- input$ca2
        ca_2010 <- subset(EU_2010, EU_2010$"COMMUNITY AREA NAME"==ca2)
        aggca_2010 <- aggregate(ca_2010[,c(5:17,19:31)],by=list(ca_2010$GEOID10),FUN=sum)
        names(aggca_2010)[names(aggca_2010) == "Group.1"] <- 'GEOID10'
        
        summ =colSums(aggca_2010[,-1],na.rm = TRUE)
        
        g <-data.frame(as.factor(1:12),unname(summ[15:length(summ)-1]),rep("Gas",12))
        names(g)<-c("Month","value","type")
        ggplot(g, aes(y=value, x=Month)) + geom_bar(position="dodge", stat="identity", fill="yellow3")+ ggtitle("Gas")
    })
    
    output$entire_city <- renderLeaflet({
        show_en <- input$show_en
        
        agg_eu_sum <- aggregate(EU_2010[,c(e_c,g_c)],by=list(EU_2010$GEOID10),FUN=sum)
        agg_eu_mean <- aggregate(EU_2010[,c("TOTAL POPULATION","AVERAGE STORIES","AVERAGE BUILDING AGE","OCCUPIED UNITS PERCENTAGE" ,"RENTER-OCCUPIED HOUSING PERCENTAGE" )],by=list(EU_2010$GEOID10),FUN=mean)
        agg_eu<- merge(agg_eu_sum,agg_eu_mean,by="Group.1")
        
        agg_eu$tract <- substr(agg_eu$"Group.1", start=6, stop=11)
        agg_c_sum <- aggregate(agg_eu[,c(e_c,g_c)],by=list(agg_eu$tract),FUN=sum)
        agg_c_mean <- aggregate(agg_eu[,c("TOTAL POPULATION","AVERAGE STORIES","AVERAGE BUILDING AGE","OCCUPIED UNITS PERCENTAGE" ,"RENTER-OCCUPIED HOUSING PERCENTAGE" )],by=list(agg_eu$tract),FUN=mean)
        agg_c<- merge(agg_c_sum,agg_c_mean,by="Group.1")
        
        num <- as.integer(nrow(agg_c)/10)
        sorted<- agg_c[order(-agg_c$`TOTAL KWH`),] 
        
        
        magg_c <- merge(tracts17031, agg_c, by.x = "TRACT", by.y = "Group.1")
        
        if (show_en=="oldest buildings"){
            sorted<- agg_c[order(-agg_c$`AVERAGE BUILDING AGE`),] 
        }else if (show_en=="newest building"){
            sorted<- agg_c[order(agg_c$`AVERAGE BUILDING AGE`),] 
        }else if (show_en=="tallest buildings"){
            sorted<- agg_c[order(-agg_c$`AVERAGE STORIES`),] 
        }else if (show_en=="blocks that use the most electricity over the year"){
            sorted<- agg_c[order(-agg_c$`TOTAL KWH`),] 
        }else if (show_en=="blocks that use the most gas over the year"){
            sorted<- agg_c[order(-agg_c$`TOTAL THERMS`),] 
        }else if (show_en=="most population"){
            sorted<- agg_c[order(-agg_c$`TOTAL POPULATION`),] 
        }else if (show_en=="most occupied percentage"){
            sorted<- agg_c[order(-agg_c$`OCCUPIED UNITS PERCENTAGE`),] 
        }else if (show_en=="highest percentage of renters"){
            sorted<- agg_c[order(-agg_c$`RENTER-OCCUPIED HOUSING PERCENTAGE`),] 
        }
        msorted <- merge(tracts17031, sorted[1:num,], by.x = "TRACT", by.y = "Group.1")
        
        
        
        
        mapview(msorted, alpha.regions = 0.2, aplha = 1)@map
    })
    
#Reset button
    observeEvent(input$reset1, {
        updateSelectInput(session, 'show1', 'To show:', show, selected = "Electricity")
        updateSelectInput(session, 'bt1', 'Building type:', Building_types, selected = "All")
        updateSelectInput(session, 'month1', 'Month:', month, selected = "Total")
    })
    observeEvent(input$button2, {
        updateSelectInput(session, 'ca1', 'Community Area1', Communities, selected = "Near West Side")
        updateSelectInput(session, 'sshow1', 'To show:', show, selected = "Electricity")
        updateSelectInput(session, 'sbt1', 'Building type:', Building_types, selected = "All")
        updateSelectInput(session, 'smonth1', 'Month:', month, selected = "Total")
    })
    observeEvent(input$button3, {
        updateSelectInput(session, 'ca2', 'Community Area2', Communities, selected = "Loop")
        updateSelectInput(session, 'sshow2', 'To show:', show, selected = "Electricity")
        updateSelectInput(session, 'sbt2', 'Building type:', Building_types, selected = "All")
        updateSelectInput(session, 'smonth2', 'Month:', month, selected = "Total")
    })
    
    output$tab1 <- renderUI({
        url <- a("Source 1", href="https://www.kaggle.com/chicago/chicago-energy-usage-2010")
        tagList("URL link:", url)
    })
    output$tab2 <- renderUI({
        url2 <- a("Source 2", href="https://data.cityofchicago.org/Environment-Sustainable-Development/Energy-Usage-2010/8yq3-m6wp")
        tagList("URL link:", url2)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

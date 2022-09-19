library(tidyverse)
library(shiny)
library(scales)
library("plotly")
library(shinycssloaders)

options(scipen=999)


invalids <- c(
  "Antigua"
  ,"Puerto Escondido"
  ,"Pedasi"
  ,"Banos"
  ,"Cusco"
)

dat <- read_csv('selina.csv') %>% 
  rowwise() %>% 
  mutate(`Total Revenue`=sum(`Room Revenue USD`,`Food and Beverage Revenue`,`Activities Revenue`,`Co-Working Revenue`),
         Occupancy=`Sold Beds`/`Available Beds`) %>% 
  mutate(TREVOB=`Total Revenue`/`Sold Beds`) %>% 
  mutate(Opening=if_else(Selina %in% invalids,"Unreliable Data",if_else(
    Selina %in% c("Playa del Carmen","Montanita","Mexico City","Cartagena"),"Q1 2018",if_else(
      Selina %in% c("Quito","Lima","Bogota La Candelaria"),"Q3 2018","Q4 2017"
    ))
  )) %>% 
  mutate(Period=format(as.Date(Date, format = "%m/%d/%Y"),"%Y/%m")) %>% 
  ungroup()


ui <- 

fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Dancing+Script&display=swap');
      
      h1 {
        font-family: 'Dancing Script', cursive;
        font-weight: 700;
        line-height: 1.1;
        color: #1A4C64; 
      }
      
      
      h3 {
        font-family: 'Dancing Script', cursive;
        font-weight: 700;
        line-height: 1.1;
        color: #1A4C64; 
      }
      
      .box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#1A4C64
                    }

.box.box-solid.box-primary{
border-bottom-color:#1A4C64;
border-left-color:#1A4C64;
border-right-color:#1A4C64;
border-top-color:#1A4C64;
}

body { 
            background-color: pink;
            color:black
            
            
}


#buy{background-color:yellow}
#sell{background-color:blue}
#del{background-color:red}

.dataTables_filter, .dataTables_info, .dataTables_paginate, .dataTables_length   {
color: black !important;
}

<!-- 
above block is used for data table navigation text
-->
    "))
  ),
  fluidRow(
  column(1,h1("Hospitality:"))
 ,column(2,h3("Performance Summary Q42017 - Y2018"))
 ,column(2,selectInput("cat","Room Type",choices=c("All",unique(dat$`Sub Category`)),multiple=FALSE))
 ,column(2,selectInput("selina","Property",choices=c(unique(dat$Selina)),multiple=TRUE))
 ,column(2,selectInput("opening","Opening",choices=c(unique(dat$Opening)),multiple=TRUE))
 ,column(3,h3("*Note: certain locations have insufficient/missing data to conduct proper analysis and comparison*"))
    ),
  #sidebarLayout( 
    #sidebarPanel(width=4),
    mainPanel(
      fluidRow(
        column(12,plotlyOutput(outputId = "grid") %>% withSpinner(color="#1A4C64"))
      ),
      fluidRow(h5("*Note on the above: reference lines represent average occupancy/TREVOB for Q42017 group | circle size represents guest review | Only room type filter applies.")),
      br(),
      fluidRow(column(4,plotOutput("rev") %>% withSpinner(color="#1A4C64")),
               column(4,plotOutput("room1") %>% withSpinner(color="#1A4C64")),
               column(4,plotOutput("room2") %>% withSpinner(color="#1A4C64"))
      ),
      br(),
      fluidRow(
        column(6,plotlyOutput("revt") %>% withSpinner(color="#1A4C64"))
        ,column(6,plotlyOutput("roomt") %>% withSpinner(color="#1A4C64"))
      )
      
    )
    
  
  
  )
  


server <- function(input, output, session) {
  
  
  output$grid <- renderPlotly({
    dat_comp <- dat %>% 
      filter(Opening=="Q4 2017") %>% 
      filter(`Sub Category`==input$cat|input$cat=="All")
    
    avg_occ <- sum(dat_comp$`Sold Beds`)/sum(dat_comp$`Available Beds`)
    avg_trv <- sum(dat_comp$`Total Revenue`)/sum(dat_comp$`Sold Beds`)
    
    dat_loc <- dat %>% 
      filter(`Sub Category`==input$cat|input$cat=="All") %>% 
      group_by(Opening,Selina) %>% 
      summarise(Total_Revenue=sum(`Total Revenue`),
                Available_Beds=sum(`Available Beds`),
                Sold_Beds=sum(`Sold Beds`),
                Rating=mean(`Average Guest Review`),
                count=n()
      ) %>% 
      mutate(Occupancy=Sold_Beds/Available_Beds,
             TREVOB=Total_Revenue/Sold_Beds)
    
    
    ggp <- ggplot(dat_loc) +
      geom_point(aes(x=Occupancy,y=TREVOB,size=Rating,colour=Opening)) +
      geom_text(aes(x=Occupancy,y=TREVOB,label=Selina),hjust=.5, vjust=0,angle=30,size=3) +
      geom_vline(xintercept=avg_occ,linetype="dotted") +
      geom_hline(yintercept=avg_trv,linetype="dotted") +
      #scale_color_manual(values=c("gray", "red")) +
      ggtitle("Occupancy vs. Revenue (Q42017 - Y2018)","by Location") +
      labs(y="TREVOB ($USD)") +
      theme_minimal()
    
    ggplotly(ggp) %>% 
      config(displayModeBar = F)
    
  })
  
  dat_revenue <- reactive({
    dat %>% 
      filter(is.null(input$selina)|Selina %in% input$selina) %>% 
      filter(is.null(input$opening)|Opening %in% input$opening) %>% 
      pivot_longer(c(`Room Revenue USD`,`Food and Beverage Revenue`,`Activities Revenue`,`Co-Working Revenue`),"Revenue Type", values_to="Amount") 
  })

  output$rev <- renderPlot({
    dat_rev <- dat_revenue() %>% 
      group_by(`Revenue Type`) %>% 
      summarise(Revenue=sum(Amount)) %>% 
      arrange(desc(Revenue))
    ggplot(dat_rev) + 
      geom_bar(aes(x=`Revenue Type`,y=Revenue),stat="identity") +
      geom_text(aes(x=`Revenue Type`,y=Revenue,label=percent(Revenue/sum(Revenue))),vjust=0) +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1)) +
      ggtitle("Revenue Streams")
    
    })
  
  output$room1 <- renderPlot({
  
  dat_room <- dat %>% 
    filter(is.null(input$selina)|Selina %in% input$selina) %>% 
    filter(is.null(input$opening)|Opening %in% input$opening) %>% 
    group_by(`Sub Category`) %>% 
    summarise(Revenue=sum(`Room Revenue USD`),Inventory=sum(`Available Beds`),Sales=sum(`Sold Beds`))
  
  ggplot(dat_room) + 
    geom_bar(aes(x=`Sub Category`,y=Revenue),stat="identity") +
    geom_text(aes(x=`Sub Category`,y=Revenue,label=percent(Revenue/sum(Revenue))),vjust=0) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    ggtitle("Room Revenue Streams")
  
  })
  
  output$room2 <- renderPlot({
    
    dat_room <- dat %>% 
      filter(is.null(input$selina)|Selina %in% input$selina) %>% 
      filter(is.null(input$opening)|Opening %in% input$opening) %>% 
      group_by(`Sub Category`) %>% 
      summarise(Revenue=sum(`Room Revenue USD`),Inventory=sum(`Available Beds`),Sales=sum(`Sold Beds`))
    
    
    ggplot(dat_room) + 
      geom_bar(aes(x=`Sub Category`,y=Inventory),stat="identity") +
      geom_text(aes(x=`Sub Category`,y=Inventory,label=percent(Inventory/sum(Inventory))),vjust=0) +
      geom_text(aes(x=`Sub Category`,y=Inventory,label=percent(Sales/Inventory),color="Occupancy"),vjust=2) +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1)) +
      ggtitle("Room Inventory") +
      labs(colour = "Legend")
    
  })
  
  
  output$revt <- renderPlotly({
    dat_trend_rev <- dat_revenue() %>% 
      group_by(Period,`Revenue Type`) %>% 
      summarise(Revenue=sum(Amount))
    ggp <- ggplot(dat_trend_rev) + 
      geom_bar(aes(x=Period,y=Revenue,fill=`Revenue Type`),stat="identity") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90)) +
      ggtitle("Trending: Revenue Streams")
    
    ggplotly(ggp) %>% 
      config(displayModeBar = F)
  })
  
  output$roomt <- renderPlotly({
    dat_trend_room <- dat %>% 
      filter(is.null(input$selina)|Selina %in% input$selina) %>% 
      filter(is.null(input$opening)|Opening %in% input$opening) %>% 
      group_by(Period,`Sub Category`) %>% 
      summarise(Revenue=sum(`Room Revenue USD`),Inventory=sum(`Available Beds`),Sales=sum(`Sold Beds`))
    ggp <- ggplot(dat_trend_room) + 
      geom_bar(aes(x=Period,y=Revenue,fill=`Sub Category`),stat="identity") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90)) +
      ggtitle("Trending: Room Revenue")
    
    ggplotly(ggp) %>% 
      config(displayModeBar = F)
  })
  
  
}


shinyApp(ui, server)

#library(rsconnect)
#rsconnect::deployApp("/home/andr31/R/github/hsptlty")

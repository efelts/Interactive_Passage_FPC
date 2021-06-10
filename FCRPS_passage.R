#load necessary libraries

library(tidyverse)
library(lubridate)
library(gghighlight)
library(plotly)
library(ggthemes)
library(viridis)
library(shiny)
library(shinydashboard)


#define FPC url to be able to download

#right now bringing in data manually. This will change to pull from FPC daily updates

lgr_daily <- read_csv("data/LGR_daily.csv") %>% 
  mutate(ChinookTotal=ChinookAdult+ChinookJack,
         CohoTotal=CohoAdult+CohoJack,
         date = mdy(Date)) %>% 
  dplyr::select(Dam,date,ChinookTotal,ChinookAdult,ChinookJack,
                CohoTotal,CohoAdult,CohoJack,Steelhead,UnclippedSteelhead,
                Sockeye,Pink,Chum,Lamprey,Shad)

#pivot longer and make a dummy date column for plotting purposes

lgr_long <- pivot_longer(lgr_daily,3:15,names_to="Species",values_to = "count") %>% 
  mutate(julian=yday(date),
         wk=week(date),
         yr=year(date)) %>% 
  mutate(spp=ifelse(Species=="ChinookTotal"&
                      julian<230,"SpSuChinook",
                    ifelse(Species=="ChinookAdult"&
                             julian<230,"SpSuChinook",
                           ifelse(Species=="ChinookJack"&
                                    julian<230,"SpSuChinook",
                                  ifelse(Species=="ChinookTotal"&
                                           julian>229,"FallChinook",
                                         ifelse(Species=="ChinookAdult"&
                                                  julian>229,"FallChinook",
                                                ifelse(Species=="ChinookJack"&
                                                         julian>229,"FallChinook",Species))))))) %>% 
  
  mutate(spp=ifelse(str_starts(Species,"Coho"),"Coho",spp)) %>% 
  mutate(spp=ifelse(str_ends(Species,"Steelhead"),"Steelhead",spp)) %>% 
  mutate(Origin=ifelse(str_starts(Species,"Unclipped"),"Wild","Combined")) %>% 
  filter(!is.na(count)) %>% 
  mutate(dummy_yr=2050,
         d=day(date),
         m=month(date),
         dummy_date=paste(m,d,dummy_yr,sep="-")) %>% 
  mutate(dummyd=mdy(dummy_date)) %>% 
  dplyr::select(Dam,date,Species,spp,Origin,count,julian,wk,yr,dummyd) %>% 
  mutate(Maturity=ifelse(str_ends(Species,"Jack"),"Jack",
                         ifelse(str_ends(Species,"Adult"),"Adult","Total")))

#get annual totals by species so they can be joined back in to calculate
#proportion of total run by date

lgr_annual <- lgr_long %>% 
  filter(yr<2021) %>% 
  group_by(spp,Maturity,Origin,yr) %>% 
  summarize(annual_total=sum(count))


lgr_daily.df <- left_join(lgr_long,lgr_annual,by=c("spp","Maturity","Origin","yr")) %>% 
  mutate(prop_total=count/annual_total) %>% 
  mutate(select_spp=ifelse(spp=="SpSuChinook","Spring-Summer Chinook",
                           ifelse(spp=="FallChinook","Fall Chinook",
                                  spp)))

#get summaries for last 5 years by species so they can be joined back in
#as a reference 

lgr_last5 <- lgr_daily.df %>% 
  filter(yr>2015&yr<2021) %>% 
  group_by(julian,select_spp,Maturity,Origin) %>% 
  summarize(count=mean(count),
          dummyd=first(dummyd)) %>% 
  mutate(Year="Average of Last 5 Years")


#put daily counts with 5-year summaries and rename some columns for better 
#handling in plotly outputs

lgr.dat <- lgr_daily.df %>% 
  filter(yr>2019) %>% 
  mutate(Year=as.factor(yr)) %>% 
  bind_rows(lgr_last5) %>% 
  mutate(Day=format(as.Date(dummyd),"%m-%d")) %>% 
  mutate(Number_Passed=round(count))


#test out internals of shiny function

test.dat <- lgr.dat %>% 
  filter(select_spp=="Lamprey")

test_plot <- test.dat %>% 
  ggplot(aes(x=dummyd,y=Number_Passed,
             color=Year,label=Day))+
  geom_line()+
  scale_color_viridis(discrete=T)+
  scale_x_date(limits=as_date(c("2050-1-1","2050-12-31")),
               date_breaks="1 month",date_labels="%B")+
  facet_wrap(~Maturity)+
  theme_fivethirtyeight()+
  labs(x="",y="Number of Chinook passing Lower Granite Dam",
       color="")+
  ggtitle("Daily Adult Chinook Count at Lower Granite Dam")
test_plot


ggplotly(test_plot)


date1 <- format(as.character(Sys.Date(),"%m-%d")) 
date_plot <- as.Date(paste("2050",show_todays_date,sep="-"))
mdy(date_plot)

#start making a shiny app with these data, just Chinook for now

#build the UI

ui <- dashboardPage(
  
 dashboardHeader(title="Daily Passage Counts"),
 
 dashboardSidebar(width=300,
                  sidebarMenu(
                    menuItem("Graphing",tabName="main",icon=icon("chart-line"))
                  )),
 
 dashboardBody(
   tabItems(
     
     
     tabItem(tabName="main",
             tabsetPanel(type="tabs",
                         selectInput("species",
                                     label="Choose a Species:",
                                     choices=c("Spring-Summer Chinook",
                                               "Fall Chinook",
                                               "Steelhead",
                                               "Sockeye",
                                               "Coho",
                                               "Lamprey")),
                         sliderInput("dates","Choose a date range",
                                     min=as.Date("2050-01-01","%Y-%m-%d"),
                                     max=as.Date("2050-12-31","%Y-%m-%d"),
                                     value=c(as.Date("2050-03-18","%Y-%m-%d"),
                                             as.Date("2050-08-12","%Y-%m-%d")),
                                     timeFormat="%m-%d")),
                         tabPanel("Graph",
                                  plotlyOutput("plot",height=700))))
                                         
                                        
     
   )
)


server <- function(input,output,session){
  
  reactive_dat <- reactive({

    lgr.dat %>%
      filter(select_spp==input$species) 
    })
  
  reactive_plot <- reactive({
    plotdat=reactive_dat()
      
    
    if(input$species=="Spring-Summer Chinook")
      return(
    plotdat %>% 
      ggplot(aes_string(x=plotdat$dummyd,y=plotdat$Number_Passed))+
      geom_line(data=plotdat,aes(color=Year))+
      scale_color_viridis(discrete=TRUE)+
      facet_wrap(~Maturity)+
      theme_bw()+
      scale_x_date(limits=as_date(c("2050-04-01","2050-8-17")),
                   date_breaks="1 month",date_labels="%B")+
      labs(x="",y="Number passing Lower Granite Dam",
           color="")+
      ggtitle("Daily Spring-Summer Chinook Count at Lower Granite Dam"))
    
    if(input$species=="Fall Chinook")
      return(
        plotdat %>% 
          ggplot(aes_string(x=plotdat$dummyd,y=plotdat$Number_Passed))+
          geom_line(data=plotdat,aes(color=Year))+
          scale_color_viridis(discrete=TRUE)+
          facet_wrap(~Maturity)+
          theme_bw()+
          scale_x_date(limits=as_date(c("2050-08-17","2050-12-31")),
                       date_breaks="1 month",date_labels="%B")+
          labs(x="",y="Number passing Lower Granite Dam",
               color="")+
          ggtitle("Daily Fall Chinook Count at Lower Granite Dam"))
    
    if(input$species=="Steelhead")
      return(
        plotdat %>% 
          ggplot(aes_string(x=plotdat$dummyd,y=plotdat$Number_Passed))+
          geom_line(data=plotdat,aes(color=Year))+
          scale_color_viridis(discrete=TRUE)+
          facet_wrap(~Origin)+
          theme_bw()+
          scale_x_date(limits=as_date(c("2050-08-17","2050-12-31")),
                       date_breaks="1 month",date_labels="%B")+
          labs(x="",y="Number passing Lower Granite Dam",
               color="")+
          ggtitle("Daily Steelhead Count at Lower Granite Dam"))
    
    if(input$species=="Sockeye")
      return(
        plotdat %>% 
          ggplot(aes_string(x=plotdat$dummyd,y=plotdat$Number_Passed))+
          geom_line(data=plotdat,aes(color=Year))+
          scale_color_viridis(discrete=TRUE)+
          theme_bw()+
          scale_x_date(limits=as_date(c("2050-06-1","2050-12-31")),
                       date_breaks="1 month",date_labels="%B")+
          labs(x="",y="Number passing Lower Granite Dam",
               color="")+
          ggtitle("Daily Sockeye Count at Lower Granite Dam"))
    
    
    if(input$species=="Coho")
      return(
        plotdat %>% 
          ggplot(aes_string(x=plotdat$dummyd,y=plotdat$Number_Passed))+
          geom_line(data=plotdat,aes(color=Year))+
          scale_color_viridis(discrete=TRUE)+
          facet_wrap(~Maturity)+
          theme_bw()+
          scale_x_date(limits=as_date(c("2050-09-01","2050-12-31")),
                       date_breaks="1 month",date_labels="%B")+
          labs(x="",y="Number passing Lower Granite Dam",
               color="")+
          ggtitle("Daily Coho Count at Lower Granite Dam"))
    
    if(input$species=="Lamprey")
      return(
        plotdat %>% 
          ggplot(aes_string(x=plotdat$dummyd,y=plotdat$Number_Passed))+
          geom_line(data=plotdat,aes(color=Year))+
          scale_color_viridis(discrete=TRUE)+
          theme_bw()+
          scale_x_date(limits=as_date(c("2050-03-01","2050-12-31")),
                       date_breaks="1 month",date_labels="%B")+
          labs(x="",y="Number passing Lower Granite Dam",
               color="")+
          ggtitle("Daily Lamprey Count at Lower Granite Dam"))

  })
  
  output$plot <- renderPlotly({
    req(reactive_plot())
    ggplotly(reactive_plot(),tooltip=c("Year","y")) %>% 
      layout(hovermode="compare")
  })
  
  
  
  
}

shinyApp(ui,server)
